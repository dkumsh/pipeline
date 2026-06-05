# `pipeline-graph` — Concepts & Design Decisions

`pipeline-graph` is a **dynamic, runtime-wired** counterpart to the static
`#[pipeline]` macro in `pipeline-dsl`. Where the macro derives a dataflow graph
from stage *function signatures* at compile time, this crate lets you assemble
the same shape of graph at **runtime** — decide which stages exist, pick their
implementations, and wire them — while keeping the wiring rules and the value
layer (`Value` / `Vector` / `Buckets` / `Reset`) identical.

This document introduces the core concepts and records the design decisions
behind them. The final chapter is a detailed treatment of type erasure.

## Contents

1. [The model: a bipartite dataflow graph](#1-the-model-a-bipartite-dataflow-graph)
2. [Core concepts (glossary)](#2-core-concepts-glossary)
3. [Lifecycle: build → validate → compute](#3-lifecycle-build--validate--compute)
4. [Design decisions](#4-design-decisions)
5. [Comparison with the static `#[pipeline]` DSL](#5-comparison-with-the-static-pipeline-dsl)
6. [Limitations & future work](#6-limitations--future-work)
7. [Chapter: type erasure](#7-chapter-type-erasure)

---

## 1. The model: a bipartite dataflow graph

The pipeline is a **bipartite directed graph** with two kinds of nodes:

- **Data nodes** — typed storage cells holding one value each (a `Vector<T>`, a
  `Value<T>`, …). These persist across cycles.
- **Function nodes** — the *stages*, each a closure or function that reads some
  data nodes and writes others.

Edges run data → stage (an input) and stage → data (an output). A stage never
reads or writes another stage directly; all dataflow goes *through* data nodes.
This is the same Petri-net-like "places and transitions" structure the static
macro produces — only assembled at runtime.

```text
  config(arg) ─┐                   ┌─> aggregate ─> fleet ─┐
  readings ────┴─> score ─> health ┤                       ├─> report ─> sink
  (external)                       └─> detect ───> alerts ─┘
```

(data nodes: `config`, `readings`, `health`, `fleet`, `alerts`, `sink`;
function nodes: `score`, `aggregate`, `detect`, `report`.)

---

## 2. Core concepts (glossary)

### `Slot<T>` — a typed handle (key)

```rust
pub struct Slot<T> { id: u32, _pd: PhantomData<fn() -> T> }
```

A `Slot<T>` is **not a reference and not a pointer** — it is a `u32` index plus
a compile-time-only type tag (`PhantomData` is zero-sized). It *names* a data
node and carries that node's value type `T` at the type level. It is `Copy`,
`'static`, borrows nothing, and can be freely stored and passed around (e.g. in
a `Handles` struct returned from a builder).

Think slotmap key / array index, but type-tagged. A `Slot<T>` becomes a real
reference only when **fetched against a `Store`**, and that reference is
transient — it is never stored inside the slot.

### `DataNode` — one erased value cell

```rust
struct DataNode {
    ptr: NonNull<u8>,                                   // a Box<T>, as raw bytes
    drop:  unsafe fn(NonNull<u8>),                      // monomorphized glue
    reset: unsafe fn(NonNull<u8>) -> Result<(), Error>,
    role: Role, writer: Option<usize>, name: String,
}
```

A data node owns exactly one heap-allocated value, type-erased to bytes, plus
the two type-specific operations the engine needs without a type in hand
(`drop`, `reset`). See the [type-erasure chapter](#7-chapter-type-erasure).

### `Store` — owns all the data nodes

```rust
pub struct Store { nodes: Vec<DataNode> }
```

The runtime container that owns every value. `Slot.id` indexes into
`Store.nodes`. The `Graph` owns the `Store` while building; the `Pipeline` owns
it afterward. `Store` is `pub` only because it appears in the `Port` trait
signature — it has no usable public surface.

### `Role` — how a data node relates to the caller and to reset

```rust
enum Role { Internal, External, Arg }
```

- **`Internal`** — produced by a stage (or unused scratch); reset each cycle iff
  it has a writer.
- **`External`** — caller-fed, produced by no stage, dirty state cleared each
  cycle. The analogue of the macro's `external = "..."`.
- **`Arg`** — caller-seeded constant, produced by no stage, **never reset**. The
  analogue of constructor `args` (e.g. a config). Its `T` need not be `Reset`.

### `Port` — a typed declaration of *use*

```rust
pub struct Input<T>(pub Slot<T>);   // stage reads this node  → &T
pub struct Output<T>(pub Slot<T>);  // stage writes this node → &mut T
```

A port wraps a `Slot<T>` **plus a direction**. Each port knows `id()`,
`access()` (`Input`/`Output`, which drives wiring), and `fetch(store)` (produce
the typed reference). **The declaration is the capability**: a stage body
receives exactly the references its ports declare and cannot reach any other
node.

### `Ports` — a stage's full port list

The tuple of a stage's ports, e.g. `(Input(cfg), Input(readings),
Output(health))`. Implemented for tuples of arity 1..=6. Provides `metadata()`
(the `(id, access)` list used for validation and topological sort) and
`fetch()` (fetch all references at once into the tuple handed to the body).

### Stage / `IntoStage` — the function nodes

A stage is registered with `Graph::stage(name, ports, body)`. The `body` is a
closure or free function that takes the port references as **separate
arguments** (like a static `#[stage]`): `fn score(cfg: &Config, readings:
&Vector<Reading>, health: &mut Vector<Health>) -> Result<Flow, Error>`. The
`IntoStage` trait (macro-implemented for arity 1..=6) adapts such a body into
the engine's internal `Runner`.

### `Graph` → `Pipeline`

- **`Graph`** — the runtime builder. `slot`/`arg`/`external` declare data nodes;
  `stage` registers stages. `build()` validates and produces a `Pipeline`.
- **`Pipeline`** — a validated, executable graph. `compute()` runs one cycle;
  `get`/`get_mut`/`set` feed inputs and inspect outputs between cycles; `dot()`
  emits the live graph as Graphviz.

### `Flow` — control flow

```rust
pub enum Flow { Continue, Break }
```

A stage returning `Flow::Break` halts the remaining stages this cycle (the
analogue of `controlflow_break`). Reset still runs afterward.

### The chain, end to end

```text
Slot<T>            id + type (a key; Copy; no data)
  │  Input(_) / Output(_)  ── adds direction → a Port
  ▼
Port::fetch(store) ── id → Store.nodes[id] → DataNode.ptr.cast::<T>() ──► &T / &mut T
  ▲                                                                       (transient)
Store              owns Vec<DataNode>; each DataNode owns one boxed T
```

---

## 3. Lifecycle: build → validate → compute

1. **Declare nodes.** `g.slot::<T>(name)` (internal), `g.arg(name, value)`
   (constant), `g.external(slot)` (caller-fed). Each allocates a `DataNode`.
2. **Register stages.** `g.stage(name, ports, body)` records the port metadata and
   a type-erased runner. Registration order does not matter (see [D5](#d5)).
3. **`build()`** runs four checks and topologically sorts (see [D3](#d3)),
   returning `Result<Pipeline, GraphError>`.
4. **`compute()`** runs stages in topological order, stopping early on
   `Flow::Break`, then clears the per-cycle dirty state of every written and
   external node. Between cycles the caller commits into external nodes and
   reads outputs.

---

## 4. Design decisions

### D1 — Static type-safety via typed handles (no `Any`)

Value type-correctness is a **compile-time** property of `Slot<T>`: the only way
to address a node is through a handle that carries its type, so access never
needs a runtime `TypeId`/downcast. The store holds bytes; the handle re-applies
the type. (Detail in the erasure chapter.)

### D2 — Declaration = capability; separate-arg bodies

A stage's ports *are* its access rights: the body receives exactly the
references it declared and physically cannot touch an undeclared node. Bodies
take their ports as **separate arguments** via `IntoStage`, so they read like
static `#[stage]` functions and plain `fn`s work directly (no captures needed).
Corollary, and a recommended style: **thread state through `arg`/input nodes
rather than closure captures** — then every stage can be a capture-free `fn`,
which is more testable and reusable.

### D3 — Build-time validation, off the hot path  <a id="d3"></a>

`build()` proves the wiring invariants once and returns `Result`:

1. **intra-stage disjointness** — a stage may not list the same node twice (nor
   both read and write it; use `Output` and read through the `&mut`);
2. **single-writer** — each node has ≤1 writing stage;
3. **missing-producer** — each read node has a writer or is `External`/`Arg`;
4. **acyclic** — topological sort must cover all stages, else `GraphError::Cycle`.

These are the same guarantees the macro enforces at compile time, moved to
startup. They also *justify the `unsafe`* on the value path: because aliasing
and single-writer are proven up front and execution is sequential, the per-stage
reference fetch is a plain pointer reborrow with no runtime borrow check.

### D4 — Reuse the value layer; node roles drive reset

The crate does not reinvent values: it reuses `Value` / `Vector` / `Buckets` and
the `Reset` trait from `pipeline-core` (imported as `pipeline`) unchanged. `Reset` clears per-cycle
**dirty** state (not contents/validity). After each `compute()`, the engine
resets every node that is `External` or has a writer; `Arg` nodes are never
reset. This makes the dirty-driven incremental recompute pattern (recompute only
what changed) work identically to the static pipeline.

### D5 — Topological sort, deterministic  <a id="d5"></a>

`build()` derives execution order from data dependencies (writer → reader edges
over nodes) via Kahn's algorithm, with ties broken by registration index
(`BinaryHeap<Reverse<usize>>`) for determinism. Stages can be registered in any
order — including leaf-first — and still execute correctly.

### D6 — `Box<dyn FnMut>` runner

A runtime-sized, heterogeneous list of stage bodies requires pointer-indirected
type erasure; `Box<dyn FnMut(&Store) -> Result<Flow, Error>>` is the minimal,
idiomatic form. The allocation is one-time at `build()`; the per-cycle cost is
one indirect call per stage, negligible next to the stage bodies. Avoiding it
would mean either a fixed (compile-time) stage set — i.e. the static design — or
a `fn`-pointer scheme that bans capturing bodies for little gain.

### D7 — Sequential execution; aliasing soundness

Stages run sequentially, so at any instant the only live references belong to
one stage and point to distinct nodes (guaranteed by single-writer +
intra-stage disjointness). That is what makes producing `&mut T` from `&Store`
sound. The build-time disjointness proof generalizes to "stages with disjoint
write-sets may run concurrently," so parallel scheduling is a possible future
extension without changing the model.

### D8 — Naming

- **`Slot`**, not `Node` — a `Slot<T>` is a *handle* to a data node, not the
  node itself (that's `DataNode`). Both data and function nodes are graph nodes,
  so a bare `Node` would be ambiguous.
- **`Input`/`Output`**, not `Read`/`Write` — this is a dataflow graph; ports are
  inputs and outputs. (`In`/`Out` was rejected: `In` collides visually with the
  `in` keyword and is a poor identifier.)

---

## 5. Comparison with the static `#[pipeline]` DSL

| Aspect | Static `#[pipeline]` | Dynamic `pipeline-graph` |
|---|---|---|
| Graph shape | fixed at compile time | chosen at runtime |
| Stage set / impls | fixed | runtime-selected, conditional |
| Wiring rules | enforced by the macro/compiler | enforced by `build()` (same rules) |
| Field/value types | struct fields | `DataNode` cells; typed via `Slot<T>` |
| Direction | `&T` / `&mut T` in signatures | `Input` / `Output` ports |
| Errors | compile errors | `GraphError` at `build()` |
| Diagram | compile-time (PUML/HTML) | runtime `dot()` |
| Value layer | `Value`/`Vector`/`Buckets`/`Reset` | identical (reused) |

**Preserved:** name/handle binding, direction-by-port, single-writer,
missing-producer, topological order, per-cycle reset, early-exit.
**Traded:** wiring errors surface at startup (not compile time); a small,
encapsulated core of `unsafe` replaces compile-time-checked field access.

---

## 6. Limitations & future work

- **Port arity** is capped at 6 by the `Ports`/`IntoStage` tuple impls; raise by
  adding macro invocations.
- **Function nodes are anonymous** — a stage is a name + closure, not a
  first-class `Stage` type. A named type could add symmetry.
- **`context`-style per-`compute` parameters** are not modeled; today everything
  flows through nodes.
- **Sequential only.** The model admits parallel scheduling (D7) but the engine
  doesn't yet do it.
- **`unsafe` validation.** Soundness reduces to the correctness of the
  `build()` validator — a small, pure, unit-testable graph algorithm. A Miri run
  over the suite would add assurance on the aliasing model.

---

## 7. Chapter: type erasure

There are **two independent type-erasure mechanisms**, and they meet in one
place.

### 7.1 Data-node erasure — erasing the *values*

The store holds nodes of different value types in one `Vec`, erased to bytes:

```rust
struct DataNode {
    ptr: NonNull<u8>,                                   // a Box<T>, as raw bytes
    drop:  unsafe fn(NonNull<u8>),                      // the ONLY type-aware ops we keep…
    reset: unsafe fn(NonNull<u8>) -> Result<(), Error>, // …captured where T was known
    role: Role, writer: Option<usize>, name: String,
}
pub struct Store { nodes: Vec<DataNode> }
```

**Forward (erase) — in `slot::<T>()`:**

```rust
let raw = Box::into_raw(Box::<T>::default());     // *mut T
let ptr = NonNull::new_unchecked(raw as *mut u8); // forget T → bytes
DataNode { ptr, drop: drop_glue::<T>, reset: reset_glue::<T>, … }
```

This is a **hand-rolled, two-entry vtable**: instead of a full `dyn Any` vtable,
we keep only the type-dependent operations the engine performs without a type in
hand — `drop` and `reset` — as monomorphized `fn` pointers captured where `T`
was still known.

**Backward (re-hydrate) — in `Store::get/get_mut`:**

```rust
unsafe fn get<T>(&self, id: u32) -> &T {
    &*self.nodes[id as usize].ptr.cast::<T>().as_ptr()
}
```

`T` is **not recovered** from the store — it is **supplied** by the caller, and
the only caller that names `T` is a `Slot<T>`. So accessing a node re-asserts
the type it was created with: no `Any`, no `TypeId`, no downcast.

The **drop path** is why the fn pointers exist: `Store::drop` calls
`(node.drop)(node.ptr)` for each node — the `Vec<DataNode>` has no idea what `T`
is, so each node carries glue that reconstructs `Box<T>` and drops it.

### 7.2 Stage-body erasure — erasing the *behavior*

```rust
type Runner = Box<dyn FnMut(&Store) -> Result<Flow, Error>>;
```

Every stage closure/`fn` is a distinct type; they live in one `Vec` via the
standard `dyn` trait object (a fat pointer with a compiler-synthesized vtable).
Unlike the data nodes, we hand-roll nothing here.

### 7.3 The bridge — where the two meet (`IntoStage`)

How does an erased `Runner` (knows no types) talk to an erased `Store` (knows no
types) and still produce correctly-typed `&T`/`&mut T`? **The type information
is captured inside the closure at registration, then erased along with it.**

```rust
fn into_runner(mut self, ports: (A, …)) -> Runner {
    Box::new(move |store: &Store| {
        let (a, …) = unsafe { ports.fetch(store) }; // typed fetch, baked in
        self(a, …)
    })
}
```

The `move` closure captures the typed body (`self`) and the typed port handles
(`ports`, each holding a `Slot<T>`). Inside, `ports.fetch(store)` calls
`Store::get::<T>(id)` with the `T` from each `Slot<T>`. So the per-stage
knowledge "fetch node #3 as `Vector<Reading>`" lives in the closure's captured
environment — type-checked at `into_runner` — and is then erased by `Box::new`.
At call time the closure re-applies its captured typed casts.

### 7.4 Putting it together

```text
  Slot<T>            carries T at compile time ─┐
                                                ├─ captured in the stage closure
  ports.fetch ── Store::get::<T>(id) ───────────┘   (typed)  ──┐
                                                               │ Box::new → erased Runner
  stage body (FnMut(&T, &mut U, …)) ───────────────(typed)─────┘

  Store: Vec<DataNode{ ptr: bytes, drop/reset: fn-ptrs }>   ← values erased to bytes
```

- **Values** → bytes + a 2-entry hand-rolled vtable (`drop`/`reset`); re-typed
  on access by `Slot<T>`.
- **Behaviors** → `Box<dyn FnMut>`; the types needed to un-erase the values are
  captured inside the behavior before it is erased.

### 7.5 Why this is sound

1. **Slot-type invariant (compile time):** a node is created as `T` and only a
   `Slot<T>` can address it, so `ptr.cast::<T>()` always matches — no runtime
   check needed.
2. **Aliasing / single-writer (build time):** `build()` proves each stage's
   ports are disjoint and each node has ≤1 writer, so the `&mut T` conjured from
   `&Store` (a raw pointer to a *separate* allocation) never aliases.

Hence neither `Any`/downcast on values nor `RefCell` borrow-checking is needed:
the erasure is un-done by static type info (handles + captured closures), and
its safety is discharged by the build-time proof rather than runtime tags.
