//! A **dynamic**, runtime-wired counterpart to the static `#[pipeline]` macro.
//!
//! Where `pipeline-dsl` derives the dataflow graph from stage *function
//! signatures* at compile time, `pipeline-graph` lets you assemble the same
//! shape of graph at **runtime**: decide which stages exist, pick their
//! implementations, and wire them — all while keeping the wiring rules and the
//! value layer (`Value` / `Vector` / `Buckets` / `Reset`) identical.
//!
//! # Design in one paragraph
//!
//! Slots are typed handles ([`Slot<T>`]); the *only* way to touch a node is
//! through its handle, so value **type-correctness is a compile-time property**
//! of the handle — no `Any`, no downcast. A stage declares its read/write set as
//! its ports (`Input(slot)` / `Output(slot)`), and its closure receives **exactly**
//! those typed references, so it cannot reach an undeclared node — that is, the
//! declaration *is* the capability. [`Graph::build`] then proves the wiring
//! invariants once (single-writer, missing-producer, intra-stage disjointness,
//! acyclic) and returns a [`Pipeline`]. Because those invariants are proven up
//! front and execution is sequential, the per-stage reference fetch is a plain
//! pointer reborrow — no `RefCell`, no runtime borrow check on the value path.
//!
//! The only type erasure lives in the node store (a raw pointer plus a drop and
//! a reset function pointer captured where the concrete type is known); it never
//! appears on the typed access path.
#![warn(missing_docs)]

use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};
use std::marker::PhantomData;
use std::ptr::NonNull;
use std::time::{Duration, Instant};

use pipeline::{Error, Reset};
use pipeline_diagram::{Edge, Group, Node};

// Re-export the value layer so a single `pipeline-graph` dependency suffices
// (`pipeline_graph::Vector`). The same types are available under the shared
// `pipeline::` name if you also depend on `pipeline-core` directly.
pub use pipeline::{Buckets, StageStats, Updated, Value, Vector, value};

// ---------------------------------------------------------------------------
// Slots
// ---------------------------------------------------------------------------

/// A typed handle to a graph node. `Copy`, and carries the node's value type
/// `T` in its own type — so every access through it is statically `T`.
pub struct Slot<T> {
    id: u32,
    _pd: PhantomData<fn() -> T>,
}

// Manual impls: deriving would wrongly require `T: Clone`/`T: Copy`.
impl<T> Clone for Slot<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Slot<T> {}

// ---------------------------------------------------------------------------
// Data nodes (the only type-erased part — never on the access path)
// ---------------------------------------------------------------------------

/// How a data node relates to the caller and to per-cycle reset.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Role {
    /// Produced by a stage (or unused scratch). Reset iff it has a writer.
    Internal,
    /// Caller-fed, produced by no stage, dirty state cleared each cycle
    /// (the `external = "..."` analogue).
    External,
    /// Caller-seeded constant, produced by no stage, never reset
    /// (the constructor-`args` analogue, e.g. config).
    Arg,
}

struct DataNode {
    /// Owns exactly one boxed `T`, allocated where `T` was statically known.
    ptr: NonNull<u8>,
    /// Drop glue for the concrete `T`.
    drop: unsafe fn(NonNull<u8>),
    /// `Reset::reset` glue for the concrete `T` (no-op for [`Role::Arg`]).
    reset: unsafe fn(NonNull<u8>) -> Result<(), Error>,
    /// `Updated::is_updated` glue for the concrete `T` — whether the node
    /// changed this cycle. Always `false` for [`Role::Arg`] (constants).
    is_updated: unsafe fn(NonNull<u8>) -> bool,
    role: Role,
    /// Stage that writes this node, assigned during `build()`.
    writer: Option<usize>,
    name: String,
}

impl DataNode {
    /// Nodes whose per-cycle dirty state the pipeline clears after `compute()`.
    fn is_reset(&self) -> bool {
        self.role == Role::External || self.writer.is_some()
    }
    /// Nodes a reader may depend on without a stage producer.
    fn is_caller_fed(&self) -> bool {
        matches!(self.role, Role::External | Role::Arg)
    }
}

unsafe fn drop_glue<T>(p: NonNull<u8>) {
    drop(unsafe { Box::from_raw(p.cast::<T>().as_ptr()) });
}

unsafe fn reset_glue<T: Reset<Error = Error>>(p: NonNull<u8>) -> Result<(), Error> {
    unsafe { (*p.cast::<T>().as_ptr()).reset() }
}

/// Reset glue for [`Role::Arg`] nodes, whose `T` need not be `Reset`.
fn noop_reset(_: NonNull<u8>) -> Result<(), Error> {
    Ok(())
}

unsafe fn updated_glue<T: Updated>(p: NonNull<u8>) -> bool {
    unsafe { (*p.cast::<T>().as_ptr()).is_updated() }
}

/// Dirtiness glue for [`Role::Arg`] nodes (constants): never dirty, and `T`
/// need not be `Updated`.
fn never_updated(_: NonNull<u8>) -> bool {
    false
}

/// Opaque, append-only collection of nodes. Public only because it appears in
/// the [`Port`] trait signature; it has no usable public surface.
pub struct Store {
    nodes: Vec<DataNode>,
}

impl Drop for Store {
    fn drop(&mut self) {
        for node in &self.nodes {
            // SAFETY: each node was created from `Box<T>` with matching glue.
            unsafe { (node.drop)(node.ptr) };
        }
    }
}

impl Store {
    #[inline]
    unsafe fn get<T>(&self, id: u32) -> &T {
        // SAFETY: node `id` was created as `T` (slot-type invariant); the value
        // lives in a separate allocation, so reborrowing it as `&T` does not
        // alias the shared borrow of `self`.
        unsafe { &*self.nodes[id as usize].ptr.cast::<T>().as_ptr() }
    }

    #[inline]
    #[allow(clippy::mut_from_ref)]
    unsafe fn get_mut<T>(&self, id: u32) -> &mut T {
        // SAFETY: as `get`, plus `build()` proves no other live reference aliases
        // this node during the stage that holds the `&mut` (single-writer +
        // intra-stage disjointness + sequential execution).
        unsafe { &mut *self.nodes[id as usize].ptr.cast::<T>().as_ptr() }
    }
}

// ---------------------------------------------------------------------------
// Ports: declaration == capability
// ---------------------------------------------------------------------------

/// Access kind of a port.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Access {
    /// The stage reads the node (`&T`).
    Input,
    /// The stage writes the node (`&mut T`).
    Output,
}

/// Declares that a stage takes the slot as an **input**; yields `&T` to the
/// stage body.
pub struct Input<T>(pub Slot<T>);
/// Declares that a stage produces the slot as an **output**; yields `&mut T` to
/// the stage body.
pub struct Output<T>(pub Slot<T>);

/// A single declared port. Implemented for [`Input`] and [`Output`].
pub trait Port {
    /// The reference handed to the stage body: `&T` for [`Input`], `&mut T` for
    /// [`Output`].
    type Ref<'a>;
    /// Id of the node this port addresses.
    fn id(&self) -> u32;
    /// Whether this port reads or writes the node.
    fn access(&self) -> Access;
    /// Fetch the typed reference for this port from the store.
    ///
    /// # Safety
    /// The engine must uphold the access discipline proven by `build()`.
    unsafe fn fetch<'a>(&self, store: &'a Store) -> Self::Ref<'a>;
}

impl<T: 'static> Port for Input<T> {
    type Ref<'a> = &'a T;
    fn id(&self) -> u32 {
        self.0.id
    }
    fn access(&self) -> Access {
        Access::Input
    }
    unsafe fn fetch<'a>(&self, store: &'a Store) -> &'a T {
        unsafe { store.get::<T>(self.0.id) }
    }
}

impl<T: 'static> Port for Output<T> {
    type Ref<'a> = &'a mut T;
    fn id(&self) -> u32 {
        self.0.id
    }
    fn access(&self) -> Access {
        Access::Output
    }
    unsafe fn fetch<'a>(&self, store: &'a Store) -> &'a mut T {
        unsafe { store.get_mut::<T>(self.0.id) }
    }
}

/// A stage's full port list. Implemented for tuples of [`Port`] (arity 1..=6).
/// `Refs<'a>` is the matching tuple of `&T` / `&mut T` handed to the stage body.
pub trait Ports {
    /// The tuple of references (`&T` / `&mut T`) handed to the stage body.
    type Refs<'a>;
    /// The `(node id, access)` list, used for validation and topological sort.
    fn metadata(&self) -> Vec<(u32, Access)>;
    /// Fetch every port's reference from the store into the [`Self::Refs`] tuple.
    ///
    /// # Safety
    /// See [`Port::fetch`].
    unsafe fn fetch<'a>(&self, store: &'a Store) -> Self::Refs<'a>;
}

macro_rules! impl_ports_tuple {
    ($($P:ident),+) => {
        impl<$($P: Port),+> Ports for ($($P,)+) {
            type Refs<'a> = ($($P::Ref<'a>,)+);
            fn metadata(&self) -> Vec<(u32, Access)> {
                #[allow(non_snake_case)]
                let ($($P,)+) = self;
                vec![$(($P.id(), $P.access())),+]
            }
            unsafe fn fetch<'a>(&self, store: &'a Store) -> Self::Refs<'a> {
                #[allow(non_snake_case)]
                let ($($P,)+) = self;
                ($(unsafe { $P.fetch(store) },)+)
            }
        }
    };
}
impl_ports_tuple!(A);
impl_ports_tuple!(A, B);
impl_ports_tuple!(A, B, C);
impl_ports_tuple!(A, B, C, D);
impl_ports_tuple!(A, B, C, D, E);
impl_ports_tuple!(A, B, C, D, E, F);

/// Converts a stage body that takes its declared ports as **separate
/// arguments** — exactly like a static `#[stage]` function — into the engine's
/// internal runner. Implemented for `FnMut`s of arity 1..=6 (closures *and*
/// free functions) whose argument types match the ports' reference types.
///
/// This is what lets you write `fn merge(cfg: &Config, synths: &Vector<_>,
/// merged: &mut Vector<_>) -> Result<Flow, Error>` and pass it straight to
/// [`Graph::stage`].
pub trait IntoStage<P: Ports> {
    #[doc(hidden)]
    fn into_runner(self, ports: P) -> Runner;
}

macro_rules! impl_into_stage {
    ($($P:ident $r:ident),+) => {
        impl<$($P: Port + 'static,)+ Func> IntoStage<($($P,)+)> for Func
        where
            Func: FnMut($($P::Ref<'_>),+) -> Result<Flow, Error> + 'static,
        {
            fn into_runner(mut self, ports: ($($P,)+)) -> Runner {
                Box::new(move |store: &Store| {
                    // SAFETY: `build()` proved this stage's ports are mutually
                    // disjoint and single-writer; execution is sequential, so no
                    // other reference to these nodes is live.
                    #[allow(non_snake_case)]
                    let ($($r,)+) = unsafe { ports.fetch(store) };
                    self($($r),+)
                })
            }
        }
    };
}
impl_into_stage!(A a);
impl_into_stage!(A a, B b);
impl_into_stage!(A a, B b, C c);
impl_into_stage!(A a, B b, C c, D d);
impl_into_stage!(A a, B b, C c, D d, E e);
impl_into_stage!(A a, B b, C c, D d, E e, F f);

// ---------------------------------------------------------------------------
// Control flow & errors
// ---------------------------------------------------------------------------

/// Returned by a stage body to drive early-exit, mirroring `controlflow_break`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Flow {
    /// Continue to the next stage in topological order.
    Continue,
    /// Stop the cycle: skip the remaining stages (reset still runs).
    Break,
}

/// A wiring error detected at [`Graph::build`] time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GraphError {
    /// A stage lists the same node more than once in its ports.
    DuplicateSlotInStage {
        /// Name of the offending stage.
        stage: String,
        /// Name of the duplicated node.
        slot: String,
    },
    /// A node is written by more than one stage (single-writer violation).
    MultipleWriters {
        /// Name of the multiply-written node.
        slot: String,
        /// First stage found writing it.
        first: String,
        /// Second stage found writing it.
        second: String,
    },
    /// A node is read but produced by no stage and not declared external/arg.
    MissingProducer {
        /// Name of the unproduced node.
        slot: String,
        /// A stage that reads it.
        reader: String,
    },
    /// The stage graph contains a cycle (no valid topological order).
    Cycle,
}

impl std::fmt::Display for GraphError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GraphError::DuplicateSlotInStage { stage, slot } => {
                write!(f, "stage '{stage}' lists slot '{slot}' more than once")
            }
            GraphError::MultipleWriters {
                slot,
                first,
                second,
            } => write!(
                f,
                "slot '{slot}' is written by multiple stages: '{first}' and '{second}'"
            ),
            GraphError::MissingProducer { slot, reader } => write!(
                f,
                "slot '{slot}' is read by '{reader}' but produced by no stage and not declared external"
            ),
            GraphError::Cycle => write!(f, "the stage graph contains a cycle"),
        }
    }
}
impl std::error::Error for GraphError {}

// ---------------------------------------------------------------------------
// Graph builder
// ---------------------------------------------------------------------------

/// Type-erased stage body: fetches its declared ports from the store and runs.
type Runner = Box<dyn FnMut(&Store) -> Result<Flow, Error>>;

struct StageDef {
    name: String,
    ports: Vec<(u32, Access)>,
    runner: Runner,
    /// If set, the engine may skip this stage in cycles where none of its
    /// declared inputs changed (see [`Graph::stage_skip_when_clean`]).
    skip_when_clean: bool,
}

/// Runtime builder for a pipeline graph.
pub struct Graph {
    name: String,
    store: Store,
    stages: Vec<StageDef>,
}

impl Default for Graph {
    fn default() -> Self {
        Self::new()
    }
}

impl Graph {
    /// Create an empty graph builder.
    pub fn new() -> Self {
        Self::named("pipeline")
    }

    /// Create an empty graph builder with a display name used by diagram
    /// generation.
    pub fn named(name: impl Into<String>) -> Self {
        Graph {
            name: name.into(),
            store: Store { nodes: Vec::new() },
            stages: Vec::new(),
        }
    }

    /// Allocate a slot holding a default-constructed `T`. `T` must be resettable
    /// so the engine can clear its per-cycle dirty state.
    pub fn slot<T>(&mut self, name: &str) -> Slot<T>
    where
        T: Default + Reset<Error = Error> + Updated + 'static,
    {
        let raw = Box::into_raw(Box::<T>::default());
        let ptr = unsafe { NonNull::new_unchecked(raw as *mut u8) };
        let id = self.store.nodes.len() as u32;
        self.store.nodes.push(DataNode {
            ptr,
            drop: drop_glue::<T>,
            reset: reset_glue::<T>,
            is_updated: updated_glue::<T>,
            role: Role::Internal,
            writer: None,
            name: name.to_string(),
        });
        Slot {
            id,
            _pd: PhantomData,
        }
    }

    /// Declare a caller-seeded constant input: read by stages, produced by no
    /// stage, and **never reset** (the constructor-`args` analogue, e.g. a
    /// config). `T` need not be `Reset`.
    pub fn arg<T: 'static>(&mut self, name: &str, value: T) -> Slot<T> {
        let raw = Box::into_raw(Box::new(value));
        let ptr = unsafe { NonNull::new_unchecked(raw as *mut u8) };
        let id = self.store.nodes.len() as u32;
        self.store.nodes.push(DataNode {
            ptr,
            drop: drop_glue::<T>,
            reset: noop_reset,
            is_updated: never_updated,
            role: Role::Arg,
            writer: None,
            name: name.to_string(),
        });
        Slot {
            id,
            _pd: PhantomData,
        }
    }

    /// Mark a slot as externally fed: read by stages, written by none, and its
    /// dirty state cleared each `compute()` (the analogue of `external = "..."`).
    pub fn external<T>(&mut self, slot: Slot<T>) {
        self.store.nodes[slot.id as usize].role = Role::External;
    }

    /// Seed / replace a slot's value before building (e.g. allocate a sized
    /// buffer).
    pub fn set<T: 'static>(&mut self, slot: Slot<T>, value: T) {
        unsafe { *self.store.get_mut::<T>(slot.id) = value };
    }

    /// Register a stage. `ports` is a tuple of `Input`/`Output`; `body` is a
    /// closure or free function taking the matching references as **separate
    /// arguments** (like a static `#[stage]`), chosen at runtime. The stage runs
    /// every `compute()`.
    pub fn stage<P, S>(&mut self, name: &str, ports: P, body: S)
    where
        P: Ports + 'static,
        S: IntoStage<P>,
    {
        self.push_stage(name, ports, body, false);
    }

    /// Register a stage the engine **may skip** in any cycle where none of its
    /// declared `Input` slots changed (`is_updated()`). The body must be a
    /// **pure function of its declared inputs** (run-gated `#[state]`-style
    /// captures are fine): when skipped it does not run and its outputs are not
    /// written, so "nothing changed" stays clean and the skip propagates to
    /// dependents. Do not mark a stage skippable if it has side effects that must
    /// happen every cycle, or if it must emit a value to bootstrap consumers
    /// (such a source has no dirty input to trigger it).
    pub fn stage_skip_when_clean<P, S>(&mut self, name: &str, ports: P, body: S)
    where
        P: Ports + 'static,
        S: IntoStage<P>,
    {
        self.push_stage(name, ports, body, true);
    }

    fn push_stage<P, S>(&mut self, name: &str, ports: P, body: S, skip_when_clean: bool)
    where
        P: Ports + 'static,
        S: IntoStage<P>,
    {
        let meta = ports.metadata();
        let runner = body.into_runner(ports);
        self.stages.push(StageDef {
            name: name.to_string(),
            ports: meta,
            runner,
            skip_when_clean,
        });
    }

    /// Validate wiring and produce an executable [`Pipeline`].
    pub fn build(mut self) -> Result<Pipeline, GraphError> {
        let n = self.stages.len();

        // (1) intra-stage disjointness + (2) single-writer; assign writers.
        for six in 0..n {
            let mut seen = HashSet::new();
            for &(id, _) in &self.stages[six].ports {
                if !seen.insert(id) {
                    return Err(GraphError::DuplicateSlotInStage {
                        stage: self.stages[six].name.clone(),
                        slot: self.store.nodes[id as usize].name.clone(),
                    });
                }
            }
            for k in 0..self.stages[six].ports.len() {
                let (id, acc) = self.stages[six].ports[k];
                if acc == Access::Output {
                    if let Some(prev) = self.store.nodes[id as usize].writer {
                        return Err(GraphError::MultipleWriters {
                            slot: self.store.nodes[id as usize].name.clone(),
                            first: self.stages[prev].name.clone(),
                            second: self.stages[six].name.clone(),
                        });
                    }
                    self.store.nodes[id as usize].writer = Some(six);
                }
            }
        }

        // (3) missing-producer.
        for st in &self.stages {
            for &(id, acc) in &st.ports {
                if acc == Access::Input {
                    let node = &self.store.nodes[id as usize];
                    if node.writer.is_none() && !node.is_caller_fed() {
                        return Err(GraphError::MissingProducer {
                            slot: node.name.clone(),
                            reader: st.name.clone(),
                        });
                    }
                }
            }
        }

        // (4) topological sort (writer -> reader edges), deterministic by index.
        let mut indeg = vec![0usize; n];
        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
        let mut edges = HashSet::new();
        for (six, st) in self.stages.iter().enumerate() {
            for &(id, acc) in &st.ports {
                if acc == Access::Input
                    && let Some(w) = self.store.nodes[id as usize].writer
                    && w != six
                    && edges.insert((w, six))
                {
                    adj[w].push(six);
                    indeg[six] += 1;
                }
            }
        }
        let mut heap: BinaryHeap<Reverse<usize>> =
            (0..n).filter(|&i| indeg[i] == 0).map(Reverse).collect();
        let mut order = Vec::with_capacity(n);
        while let Some(Reverse(u)) = heap.pop() {
            order.push(u);
            for &v in &adj[u] {
                indeg[v] -= 1;
                if indeg[v] == 0 {
                    heap.push(Reverse(v));
                }
            }
        }
        if order.len() != n {
            return Err(GraphError::Cycle);
        }

        // Contiguous per-stage stats (registration order), so `stats()` can hand
        // back a `&[StageStats]` with no per-call work. Names are cloned once here.
        let stage_stats = self
            .stages
            .iter()
            .map(|s| StageStats {
                name: s.name.clone(),
                ran: 0,
                skipped: 0,
                time: Duration::ZERO,
            })
            .collect();

        Ok(Pipeline {
            name: self.name,
            store: self.store,
            stages: self.stages,
            order,
            stage_stats,
            stats_enabled: false,
            stats_since: None,
        })
    }
}

// ---------------------------------------------------------------------------
// Executable pipeline
// ---------------------------------------------------------------------------

/// A validated, executable graph. Run it with [`Pipeline::compute`].
pub struct Pipeline {
    name: String,
    store: Store,
    stages: Vec<StageDef>,
    order: Vec<usize>,
    /// Per-stage stats in registration order (parallel to `stages`), so
    /// [`Pipeline::stats`] returns a `&[StageStats]` with no per-call work.
    stage_stats: Vec<StageStats>,
    /// When set, `compute()` records per-stage [`StageStats`] (run/skip counts +
    /// timing). Off by default so the hot path does no counter writes or clock
    /// reads.
    stats_enabled: bool,
    /// Monotonic start of the current stats window, stamped by
    /// [`Pipeline::reset_stats`]. `None` until the first reset (so `build()`
    /// reads no clock). Powers [`Pipeline::stats_age`] for rate/utilization math.
    stats_since: Option<Instant>,
}

impl Pipeline {
    /// Display name of this runtime graph, used by diagram generation.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Run all stages in topological order, then clear the per-cycle dirty state
    /// of every written and external slot — exactly the static `compute()`.
    ///
    /// A stage registered via [`Graph::stage_skip_when_clean`] is skipped in any
    /// cycle where none of its declared inputs changed; a skipped stage doesn't
    /// run and doesn't write, so the "unchanged" status propagates to dependents.
    pub fn compute(&mut self) -> Result<(), Error> {
        // Monomorphize two flavors: the stats path records counters + timing,
        // the default path does neither (no counter writes, no clock reads).
        if self.stats_enabled {
            self.run::<true>()
        } else {
            self.run::<false>()
        }
    }

    fn run<const STATS: bool>(&mut self) -> Result<(), Error> {
        for i in 0..self.order.len() {
            let ix = self.order[i];
            // The skip decision is unconditional behaviour; only the recording
            // is gated by STATS (compiled away when false).
            if self.stages[ix].skip_when_clean && !self.stage_inputs_dirty(ix) {
                if STATS {
                    self.stage_stats[ix].skipped += 1;
                }
                continue;
            }
            let flow = if STATS {
                let start = Instant::now();
                let flow = (self.stages[ix].runner)(&self.store)?;
                self.stage_stats[ix].time += start.elapsed();
                self.stage_stats[ix].ran += 1;
                flow
            } else {
                (self.stages[ix].runner)(&self.store)?
            };
            match flow {
                Flow::Continue => {}
                Flow::Break => break,
            }
        }
        for node in &self.store.nodes {
            if node.is_reset() {
                // SAFETY: node holds `T`; reset glue matches that `T`.
                unsafe { (node.reset)(node.ptr)? };
            }
        }
        Ok(())
    }

    /// Whether any of stage `six`'s declared inputs changed this cycle.
    fn stage_inputs_dirty(&self, six: usize) -> bool {
        self.stages[six].ports.iter().any(|&(id, acc)| {
            acc == Access::Input && {
                let node = &self.store.nodes[id as usize];
                // SAFETY: node holds `T`; is_updated glue matches that `T`.
                unsafe { (node.is_updated)(node.ptr) }
            }
        })
    }

    /// Enable or disable per-stage stats collection (run/skip counts + timing).
    /// **Off by default**: when off, `compute()` writes no counters and reads no
    /// clock. Returns `&mut self` for chaining.
    pub fn collect_stats(&mut self, enable: bool) -> &mut Self {
        self.stats_enabled = enable;
        self
    }

    /// Per-stage counters in registration order, accumulated across `compute()`
    /// calls made while [`Pipeline::collect_stats`] was enabled. `time` is the
    /// total wall-clock spent running each stage.
    ///
    /// Returns a borrowed slice — no allocation, no per-call work — so it's cheap
    /// to poll from a `compute()` loop.
    pub fn stats(&self) -> &[StageStats] {
        &self.stage_stats
    }

    /// Zero every stage's counters and start a fresh measurement window (stamps
    /// a monotonic instant for [`Pipeline::stats_age`]). Independent of
    /// [`Pipeline::collect_stats`]: call it to begin/restart a reporting window.
    pub fn reset_stats(&mut self) {
        for s in &mut self.stage_stats {
            s.ran = 0;
            s.skipped = 0;
            s.time = Duration::ZERO;
        }
        self.stats_since = Some(Instant::now());
    }

    /// How long the current stats window has been accumulating (monotonic),
    /// i.e. elapsed since the last [`Pipeline::reset_stats`]; `None` if never
    /// reset. Combine with [`StageStats`] for rates / utilization, e.g.
    /// `stage.ran as f64 / age.as_secs_f64()` or `stage.time.as_secs_f64() /
    /// age.as_secs_f64()`.
    pub fn stats_age(&self) -> Option<Duration> {
        self.stats_since.map(|t| t.elapsed())
    }

    /// Read a slot between cycles (e.g. to inspect results).
    pub fn get<T: 'static>(&self, slot: Slot<T>) -> &T {
        unsafe { self.store.get::<T>(slot.id) }
    }

    /// Mutate a slot between cycles (e.g. to feed an external input).
    pub fn get_mut<T: 'static>(&mut self, slot: Slot<T>) -> &mut T {
        unsafe { self.store.get_mut::<T>(slot.id) }
    }

    /// Replace a slot's whole value (e.g. allocate a sized external buffer).
    pub fn set<T: 'static>(&mut self, slot: Slot<T>, value: T) {
        *self.get_mut(slot) = value;
    }

    /// Emit a Graphviz DOT description of the live graph — the runtime analogue
    /// of the macro's compile-time diagram.
    pub fn dot(&self) -> String {
        let mut s = String::from("digraph pipeline {\n  rankdir=LR;\n");
        for st in &self.stages {
            s.push_str(&format!("  \"{}\" [shape=box];\n", st.name));
        }
        for st in &self.stages {
            for &(id, acc) in &st.ports {
                let node = &self.store.nodes[id as usize];
                match acc {
                    Access::Input => {
                        s.push_str(&format!("  \"{}\" -> \"{}\";\n", node.name, st.name))
                    }
                    Access::Output => {
                        s.push_str(&format!("  \"{}\" -> \"{}\";\n", st.name, node.name))
                    }
                }
            }
        }
        s.push_str("}\n");
        s
    }

    /// Render the live graph as an interactive HTML diagram, stamped with the
    /// current local time (shown in the footer as when the diagram was generated).
    pub fn html_diagram(&self) -> String {
        let generated_at = chrono::Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
        let json = self.diagram_json_inner(Some(&generated_at));
        pipeline_diagram::render_html(&json).expect("runtime diagram JSON is valid")
    }

    /// Return the live graph as a `pipeline-diagram` graph JSON (stages + slots
    /// as nodes, ports as edges). See `pipeline-diagram` for the shape.
    pub fn diagram_json(&self) -> String {
        self.diagram_json_inner(None)
    }

    fn diagram_json_inner(&self, generated_at: Option<&str>) -> String {
        let mut nodes = Vec::with_capacity(self.stages.len() + self.store.nodes.len());
        for (ix, st) in self.stages.iter().enumerate() {
            nodes.push(Node {
                id: stage_diagram_id(ix),
                label: st.name.clone(),
                group: Group::Stage,
                full_label: Some(format!("Stage: {}", st.name)),
            });
        }
        for (id, node) in self.store.nodes.iter().enumerate() {
            nodes.push(Node {
                id: slot_diagram_id(id),
                label: node.name.clone(),
                group: Group::Variable,
                full_label: Some(format!("Slot: {}", node.name)),
            });
        }

        let mut edges = Vec::new();
        for (ix, st) in self.stages.iter().enumerate() {
            let stage_id = stage_diagram_id(ix);
            for &(id, acc) in &st.ports {
                let slot_id = slot_diagram_id(id as usize);
                match acc {
                    Access::Input => edges.push(Edge {
                        from: slot_id,
                        to: stage_id.clone(),
                    }),
                    Access::Output => edges.push(Edge {
                        from: stage_id.clone(),
                        to: slot_id,
                    }),
                }
            }
        }

        pipeline_diagram::graph_json(&self.name, &nodes, &edges, generated_at)
    }

    /// Write [`Pipeline::html_diagram`] to a file.
    pub fn write_html_to_file<P: AsRef<std::path::Path>>(
        &self,
        file_path: P,
    ) -> std::io::Result<()> {
        std::fs::write(file_path, self.html_diagram())
    }
}

fn stage_diagram_id(index: usize) -> String {
    format!("stage:{index}")
}

fn slot_diagram_id(index: usize) -> String {
    format!("slot:{index}")
}
