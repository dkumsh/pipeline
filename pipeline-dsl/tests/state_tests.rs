//! Integration tests for the `#[state]` per-parameter attribute: per-stage
//! private, persistent, plain-`T` scratch owned by the pipeline. Unlike a slot,
//! state is not on the dataflow graph — never reset, never read by another
//! stage.

use pipeline::value::Value;
use pipeline_dsl::pipeline;

/// A `Default` state type (the common case): a counter that accumulates across
/// `compute()` cycles.
#[derive(Default)]
pub struct Counter {
    pub calls: u32,
}

// === A pipeline whose only stage owns a single piece of state ===
#[pipeline(name = "CounterPipeline")]
mod counter_mod {
    use super::Counter;
    use pipeline_dsl::stage;

    #[stage]
    pub fn tick(#[state] counter: &mut Counter) {
        counter.calls += 1;
    }
}

#[test]
fn state_persists_across_cycles_and_is_not_reset() {
    let mut p = CounterPipeline::new();
    p.compute().unwrap();
    p.compute().unwrap();
    p.compute().unwrap();
    // State is owned by the pipeline and never reset, so it accumulates.
    assert_eq!(p.counter.calls, 3);
}

#[test]
fn pipeline_with_no_args_is_default() {
    // No `args` + auto constructor => the macro also derives `Default`.
    let mut p = CounterPipeline::default();
    p.compute().unwrap();
    assert_eq!(p.counter.calls, 1);
}

#[test]
fn state_can_be_seeded_after_new() {
    // The state field is private, but reachable from the module that defines the
    // pipeline (here, the test crate root), so in-crate code can seed it
    // post-construction. Downstream crates cannot.
    let mut p = CounterPipeline::new();
    p.counter.calls = 40;
    p.compute().unwrap();
    p.compute().unwrap();
    assert_eq!(p.counter.calls, 42);
}

// === State coexists with ordinary dataflow slots ===
// `produce` writes `out` (a slot) and owns private `counter`; `consume` reads
// `out` and owns its own private `seen`. The slot creates the produce->consume
// ordering; both states are private and persist.
#[pipeline(name = "MixedPipeline")]
mod mixed_mod {
    use super::Counter;
    use pipeline::value::Value;
    use pipeline_dsl::stage;

    #[stage]
    pub fn produce(#[state] counter: &mut Counter, out: &mut Value<u32>) {
        counter.calls += 1;
        out.set(counter.calls);
    }

    #[stage]
    pub fn consume(out: &Value<u32>, #[state] seen: &mut Counter) {
        seen.calls = *out.get_valid().expect("produce wrote out");
    }
}

#[test]
fn state_coexists_with_dataflow_slots() {
    let mut p = MixedPipeline::new();
    p.compute().unwrap();
    p.compute().unwrap();
    // Two independent private states, each owned by one stage, both persisting.
    assert_eq!(p.counter.calls, 2);
    assert_eq!(p.seen.calls, 2);
    // The slot itself round-trips its (reset-tracked) value each cycle.
    assert_eq!(p.out.get_valid().copied(), Some(2));
}

// === A non-`Default` state type with a manual constructor ===
pub struct Cap {
    pub limit: usize,
}

impl Cap {
    fn new(limit: usize) -> Self {
        Cap { limit }
    }
}

#[pipeline(name = "ManualPipeline", constructor = "manual")]
mod manual_mod {
    use super::Cap;
    use pipeline_dsl::stage;

    #[stage]
    pub fn refine(#[state] cap: &mut Cap) {
        cap.limit += 1;
    }
}

// `constructor = "manual"` suppresses the generated `new()`/`Default`, so we
// supply our own — required because `Cap` is not `Default`.
impl ManualPipeline {
    fn with_cap(limit: usize) -> Self {
        Self {
            pipeline_vars: ["cap"],
            cap: Cap::new(limit),
        }
    }
}

#[test]
fn manual_constructor_for_non_default_state() {
    let mut p = ManualPipeline::with_cap(5);
    p.compute().unwrap();
    p.compute().unwrap();
    assert_eq!(p.cap.limit, 7);
}
