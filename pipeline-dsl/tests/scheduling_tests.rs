//! `#[stage(skip_when_clean)]` demand-driven scheduling and the optional
//! `#[pipeline(stats)]` per-stage stats, in the static front-end.

use pipeline::value::Value;
use pipeline_dsl::pipeline;

// Stats enabled: a skippable stage runs only when its (external) input changed.
#[pipeline(name = "SkipPipe", external = "input", stats)]
mod skip_mod {
    use pipeline::value::Value;
    use pipeline_dsl::stage;

    #[stage(skip_when_clean)]
    pub fn double(input: &Value<u32>, #[unused] out: &mut Value<u32>) {
        out.set(*input.get_valid().expect("dirty => valid") * 2);
    }
}

#[test]
fn skip_when_clean_with_stats() {
    let mut p = SkipPipe::new();
    p.collect_stats(true);
    p.reset_stats();

    // Cycle 1: feed input -> dirty -> runs.
    p.input.set(10);
    p.compute().unwrap();
    assert_eq!(p.out.get_valid(), Some(&20));

    // Cycle 2: input untouched -> clean -> skipped, output unchanged.
    p.compute().unwrap();
    assert_eq!(p.out.get_valid(), Some(&20));

    // Cycle 3: feed again -> runs.
    p.input.set(5);
    p.compute().unwrap();
    assert_eq!(p.out.get_valid(), Some(&10));

    let stats = p.stats();
    assert_eq!(stats[0].name, "double");
    assert_eq!(stats[0].ran, 2);
    assert_eq!(stats[0].skipped, 1);
    assert!(p.stats_age().is_some());
}

// Same skipping behaviour, but WITHOUT `stats` — no stats fields/methods are
// generated, yet the skip guard still works (asserted via output values).
#[pipeline(name = "SkipOnly", external = "input")]
mod skip_only_mod {
    use pipeline::value::Value;
    use pipeline_dsl::stage;

    #[stage(skip_when_clean)]
    pub fn double(input: &Value<u32>, #[unused] out: &mut Value<u32>) {
        out.set(*input.get_valid().expect("dirty => valid") * 2);
    }
}

#[test]
fn skip_when_clean_without_stats() {
    let mut p = SkipOnly::new();

    p.input.set(3);
    p.compute().unwrap();
    assert_eq!(p.out.get_valid(), Some(&6));

    // Clean cycle -> skipped -> output frozen.
    p.compute().unwrap();
    assert_eq!(p.out.get_valid(), Some(&6));

    p.input.set(7);
    p.compute().unwrap();
    assert_eq!(p.out.get_valid(), Some(&14));
}
