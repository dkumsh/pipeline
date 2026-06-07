use pipeline::{Value, Vector};
use pipeline_graph::{Flow, Graph, GraphError, Input, Output};

#[test]
fn graph_new_uses_default_name() {
    let p = Graph::new().build().expect("empty graph is valid");
    assert_eq!(p.name(), "pipeline");
}

#[test]
fn graph_named_carries_name_to_pipeline() {
    let p = Graph::named("TelemetryMonitor")
        .build()
        .expect("empty graph is valid");
    assert_eq!(p.name(), "TelemetryMonitor");
}

#[test]
#[cfg_attr(
    miri,
    ignore = "html_diagram() stamps wall-clock time via chrono (clock_gettime), unavailable under Miri isolation"
)]
fn html_diagram_renders_runtime_graph() {
    let mut g = Graph::named("RuntimeDiagram");
    let input = g.arg("input", 7u32);
    let output = g.slot::<Value<u32>>("output");
    g.stage(
        "copy",
        (Input(input), Output(output)),
        |input: &u32, output: &mut Value<u32>| {
            output.set(*input);
            Ok(Flow::Continue)
        },
    );

    let p = g.build().expect("valid graph");
    let html = p.html_diagram();

    assert!(html.contains("Pipeline: RuntimeDiagram"));
    assert!(html.contains("sidebar-collapsed"));
    assert!(html.contains("toggleSidebar"));
    assert!(html.contains("network.setSize('100%', '100%')"));
    assert!(html.contains("\"label\":\"copy\""));
    assert!(html.contains("\"label\":\"input\""));
    assert!(html.contains("\"label\":\"output\""));

    let json = p.diagram_json();
    assert!(json.contains("\"pipeline_name\":\"RuntimeDiagram\""));
    assert!(json.contains("\"label\":\"copy\""));
}

#[test]
#[cfg_attr(miri, ignore = "does real filesystem I/O; Miri runs under isolation")]
fn write_html_to_file_writes_runtime_graph() {
    let p = Graph::named("FileDiagram")
        .build()
        .expect("empty graph is valid");
    let path = std::env::temp_dir().join(format!(
        "pipeline-graph-{}-diagram.html",
        std::process::id()
    ));

    p.write_html_to_file(&path).expect("write html");
    let html = std::fs::read_to_string(&path).expect("read html");
    let _ = std::fs::remove_file(&path);

    assert!(html.contains("Pipeline: FileDiagram"));
}

// leaves (external) -> synthesize -> synths -> total -> sum
#[test]
fn basic_dataflow_and_reset() {
    let mut g = Graph::new();
    let leaves = g.slot::<Vector<u32>>("leaves");
    let synths = g.slot::<Vector<u32>>("synths");
    let sum = g.slot::<Value<u32>>("sum");
    g.external(leaves);

    g.stage(
        "synthesize",
        (Input(leaves), Output(synths)),
        |l: &Vector<u32>, s: &mut Vector<u32>| {
            for i in 0..l.len() {
                if let Some(v) = l.get_valid(i) {
                    s.push_committed(*v * 10);
                }
            }
            Ok(Flow::Continue)
        },
    );

    g.stage(
        "total",
        (Input(synths), Output(sum)),
        |s: &Vector<u32>, total: &mut Value<u32>| {
            total.set(s.as_slice().iter().sum());
            Ok(Flow::Continue)
        },
    );

    let mut p = g.build().expect("valid graph");

    // Feed the external buffer and run.
    p.set(leaves, Vector::from_vec(vec![1, 2, 3]));
    p.get_mut(leaves).commit(0, 5); // mark a slot dirty
    assert!(p.get(leaves).is_updated_at(0));

    p.compute().expect("compute ok");

    assert_eq!(p.get(synths).as_slice(), &[50, 20, 30]);
    assert_eq!(p.get(sum).get_valid(), Some(&100));

    // External slot's per-cycle dirty flags were cleared by the pipeline.
    assert!(!p.get(leaves).is_updated_at(0));
    assert_eq!(p.get(leaves).len(), 3); // contents preserved
}

#[test]
fn runtime_chosen_implementation_and_conditional_stage() {
    // Build the same graph two ways at runtime: a different `synthesize` impl,
    // and an optionally-included `bonus` stage.
    fn run(fast: bool, with_bonus: bool) -> u32 {
        let mut g = Graph::new();
        let leaves = g.slot::<Vector<u32>>("leaves");
        let synths = g.slot::<Vector<u32>>("synths");
        let sum = g.slot::<Value<u32>>("sum");
        g.external(leaves);

        // Implementation chosen at runtime.
        if fast {
            g.stage(
                "synthesize",
                (Input(leaves), Output(synths)),
                |l: &Vector<u32>, s: &mut Vector<u32>| {
                    for i in 0..l.len() {
                        if let Some(v) = l.get_valid(i) {
                            s.push_committed(*v * 2);
                        }
                    }
                    Ok(Flow::Continue)
                },
            );
        } else {
            g.stage(
                "synthesize",
                (Input(leaves), Output(synths)),
                |l: &Vector<u32>, s: &mut Vector<u32>| {
                    for i in 0..l.len() {
                        if let Some(v) = l.get_valid(i) {
                            s.push_committed(*v * 100);
                        }
                    }
                    Ok(Flow::Continue)
                },
            );
        }

        // Stage conditionally added at runtime.
        if with_bonus {
            g.stage("bonus", (Output(sum),), |total: &mut Value<u32>| {
                total.set(1000);
                Ok(Flow::Continue)
            });
        } else {
            g.stage(
                "total",
                (Input(synths), Output(sum)),
                |s: &Vector<u32>, total: &mut Value<u32>| {
                    total.set(s.as_slice().iter().sum());
                    Ok(Flow::Continue)
                },
            );
        }

        let mut p = g.build().expect("valid");
        p.set(leaves, Vector::from_vec(vec![1, 2, 3]));
        p.compute().expect("ok");
        *p.get(sum).get_valid().unwrap()
    }

    assert_eq!(run(true, false), 12); // (1+2+3)*2
    assert_eq!(run(false, false), 600); // (1+2+3)*100
    assert_eq!(run(true, true), 1000); // bonus overrides total
}

// Stages registered in REVERSE dependency order must still execute in
// dependency order: c reads b, b reads a, a is the source. If `build()` did not
// topologically sort, running c/b before a would read stale/empty values.
#[test]
fn topological_sort_reorders_stages() {
    let mut g = Graph::new();
    let a = g.slot::<Value<u32>>("a");
    let b = g.slot::<Value<u32>>("b");
    let c = g.slot::<Value<u32>>("c");

    // Registered c, then b, then a — opposite of the data dependency.
    g.stage(
        "c",
        (Input(b), Output(c)),
        |b: &Value<u32>, c: &mut Value<u32>| {
            c.set(*b.get_valid().expect("b produced before c") + 1);
            Ok(Flow::Continue)
        },
    );
    g.stage(
        "b",
        (Input(a), Output(b)),
        |a: &Value<u32>, b: &mut Value<u32>| {
            b.set(*a.get_valid().expect("a produced before b") * 10);
            Ok(Flow::Continue)
        },
    );
    g.stage("a", (Output(a),), |a: &mut Value<u32>| {
        a.set(2);
        Ok(Flow::Continue)
    });

    let mut p = g.build().expect("valid DAG");
    p.compute().expect("compute ok");
    assert_eq!(p.get(a).get_valid(), Some(&2));
    assert_eq!(p.get(b).get_valid(), Some(&20)); // 2 * 10
    assert_eq!(p.get(c).get_valid(), Some(&21)); // 20 + 1
}

#[test]
fn rejects_multiple_writers() {
    let mut g = Graph::new();
    let x = g.slot::<Value<u32>>("x");
    g.stage("a", (Output(x),), |v: &mut Value<u32>| {
        v.set(1);
        Ok(Flow::Continue)
    });
    g.stage("b", (Output(x),), |v: &mut Value<u32>| {
        v.set(2);
        Ok(Flow::Continue)
    });
    assert!(matches!(g.build(), Err(GraphError::MultipleWriters { .. })));
}

#[test]
fn rejects_missing_producer() {
    let mut g = Graph::new();
    let x = g.slot::<Value<u32>>("x");
    g.stage("reader", (Input(x),), |_v: &Value<u32>| Ok(Flow::Continue));
    assert!(matches!(g.build(), Err(GraphError::MissingProducer { .. })));
}

#[test]
fn rejects_cycle() {
    let mut g = Graph::new();
    let x = g.slot::<Value<u32>>("x");
    let y = g.slot::<Value<u32>>("y");
    // a: reads y, writes x ; b: reads x, writes y  -> cycle
    g.stage(
        "a",
        (Input(y), Output(x)),
        |_y: &Value<u32>, x: &mut Value<u32>| {
            x.set(1);
            Ok(Flow::Continue)
        },
    );
    g.stage(
        "b",
        (Input(x), Output(y)),
        |_x: &Value<u32>, y: &mut Value<u32>| {
            y.set(1);
            Ok(Flow::Continue)
        },
    );
    assert!(matches!(g.build(), Err(GraphError::Cycle)));
}

// ---------------------------------------------------------------------------
// Adversarial tests — run under `cargo +nightly miri test` (`just miri`) to
// stress the unsafe store's aliasing model on the hard cases the basic tests
// don't reach: many simultaneous borrows, a panicking stage, and high arity.
// ---------------------------------------------------------------------------

// Maximum aliasing pressure in one stage: three live `&` references to distinct
// nodes plus one live `&mut`, all conjured from the same `&Store` at once.
#[test]
fn many_disjoint_borrows_in_one_stage() {
    let mut g = Graph::new();
    let a = g.arg("a", 2u32);
    let b = g.arg("b", 3u32);
    let c = g.arg("c", 5u32);
    let out = g.slot::<Value<u32>>("out");

    g.stage(
        "combine",
        (Input(a), Input(b), Input(c), Output(out)),
        |a: &u32, b: &u32, c: &u32, out: &mut Value<u32>| {
            // a, b, c (&) and out (&mut) are all alive simultaneously here.
            out.set(*a + *b + *c);
            Ok(Flow::Continue)
        },
    );

    let mut p = g.build().expect("valid graph");
    p.compute().unwrap();
    assert_eq!(p.get(out).get_valid(), Some(&10));
}

// A stage that panics mid-cycle must leave the store sound: unwinding drops the
// in-flight `&mut`, the value written before the panic survives, and dropping
// the pipeline frees every node exactly once (Miri verifies the drop glue).
#[test]
fn panicking_stage_leaves_store_sound() {
    let mut g = Graph::new();
    let x = g.slot::<Value<u32>>("x");
    g.stage("boom", (Output(x),), |x: &mut Value<u32>| {
        x.set(7);
        panic!("stage failure") // `!` coerces to the Result return type
    });
    let mut p = g.build().expect("valid graph");

    // Silence the panic backtrace this test deliberately triggers.
    let prev = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let res = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| p.compute()));
    std::panic::set_hook(prev);

    assert!(
        res.is_err(),
        "the stage panic should unwind out of compute()"
    );
    assert_eq!(p.get(x).get_valid(), Some(&7)); // value written before the panic
    // `p` drops here — Miri checks every node is dropped exactly once.
}

// High arity: exercise the arity-6 `Ports`/`IntoStage` impls (5 inputs + 1
// output) that the other tests never reach.
#[test]
fn high_arity_stage() {
    let mut g = Graph::new();
    let a = g.arg("a", 1u32);
    let b = g.arg("b", 2u32);
    let c = g.arg("c", 3u32);
    let d = g.arg("d", 4u32);
    let e = g.arg("e", 5u32);
    let out = g.slot::<Value<u32>>("out");

    g.stage(
        "sum6",
        (
            Input(a),
            Input(b),
            Input(c),
            Input(d),
            Input(e),
            Output(out),
        ),
        |a: &u32, b: &u32, c: &u32, d: &u32, e: &u32, out: &mut Value<u32>| {
            out.set(a + b + c + d + e);
            Ok(Flow::Continue)
        },
    );

    let mut p = g.build().expect("valid graph");
    p.compute().unwrap();
    assert_eq!(p.get(out).get_valid(), Some(&15));
}

// === skip_when_clean scheduling + per-stage stats ===

use std::cell::Cell;
use std::rc::Rc;

#[test]
fn skip_when_clean_runs_only_on_dirty_input() {
    // Miri-safe (no stats, so no clock): a captured Rc<Cell> counts runs — also
    // the closure-capture idiom that is the graph's analogue of #[state].
    let runs = Rc::new(Cell::new(0u32));
    let mut g = Graph::named("SkipBehaviour");
    let input = g.slot::<Value<u32>>("input");
    let output = g.slot::<Value<u32>>("output");
    g.external(input);
    let runs_c = Rc::clone(&runs);
    g.stage_skip_when_clean(
        "double",
        (Input(input), Output(output)),
        move |i: &Value<u32>, o: &mut Value<u32>| {
            runs_c.set(runs_c.get() + 1);
            o.set(*i.get_valid().expect("valid when run") * 2);
            Ok(Flow::Continue)
        },
    );
    let mut p = g.build().expect("valid graph");

    p.get_mut(input).set(10);
    p.compute().unwrap(); // dirty -> run
    p.compute().unwrap(); // clean -> skip
    p.compute().unwrap(); // clean -> skip
    p.get_mut(input).set(5);
    p.compute().unwrap(); // dirty -> run

    assert_eq!(runs.get(), 2);
    assert_eq!(p.get(output).get_valid(), Some(&10));
}

#[test]
fn invalidation_wakes_skip_when_clean_consumer() {
    // A valid->invalid transition is dirty, so a skip_when_clean consumer is
    // scheduled and sees the now-invalid input; an unchanged cycle skips.
    let runs = Rc::new(Cell::new(0u32));
    let mut g = Graph::named("Invalidate");
    let input = g.slot::<Value<u32>>("input");
    let saw_invalid = g.slot::<Value<bool>>("saw_invalid");
    g.external(input);
    let runs_c = Rc::clone(&runs);
    g.stage_skip_when_clean(
        "watch",
        (Input(input), Output(saw_invalid)),
        move |i: &Value<u32>, s: &mut Value<bool>| {
            runs_c.set(runs_c.get() + 1);
            s.set(i.get_valid().is_none());
            Ok(Flow::Continue)
        },
    );
    let mut p = g.build().expect("valid graph");

    p.get_mut(input).set(7);
    p.compute().unwrap(); // dirty -> run, input valid
    assert_eq!(p.get(saw_invalid).get_valid(), Some(&false));

    p.get_mut(input).invalidate();
    p.compute().unwrap(); // became invalid -> dirty -> run, input invalid
    assert_eq!(p.get(saw_invalid).get_valid(), Some(&true));

    p.compute().unwrap(); // unchanged -> skip

    assert_eq!(runs.get(), 2); // ran on set and on invalidate; skipped the quiet cycle
}

#[test]
#[cfg_attr(
    miri,
    ignore = "stats timing uses Instant (clock_gettime), unavailable under Miri isolation"
)]
fn stats_record_runs_and_skips_and_are_off_by_default() {
    let mut g = Graph::named("Stats");
    let input = g.slot::<Value<u32>>("input");
    let output = g.slot::<Value<u32>>("output");
    g.external(input);
    g.stage_skip_when_clean(
        "double",
        (Input(input), Output(output)),
        |i: &Value<u32>, o: &mut Value<u32>| {
            o.set(*i.get_valid().unwrap() * 2);
            Ok(Flow::Continue)
        },
    );
    let mut p = g.build().expect("valid graph");

    // Off by default: counters stay zero even though the stage runs/skips.
    p.get_mut(input).set(1);
    p.compute().unwrap();
    p.compute().unwrap();
    assert_eq!(p.stats()[0].ran, 0);
    assert_eq!(p.stats()[0].skipped, 0);

    // Enabled: counts run vs skip.
    p.collect_stats(true);
    p.get_mut(input).set(2);
    p.compute().unwrap(); // run
    p.compute().unwrap(); // skip
    p.compute().unwrap(); // skip
    let s = &p.stats()[0];
    assert_eq!(s.name, "double");
    assert_eq!(s.ran, 1);
    assert_eq!(s.skipped, 2);
}

#[test]
#[cfg_attr(
    miri,
    ignore = "reset_stats/stats_age use Instant (clock_gettime), unavailable under Miri isolation"
)]
fn reset_stats_zeros_counters_and_starts_window() {
    let mut g = Graph::named("ResetStats");
    let input = g.slot::<Value<u32>>("input");
    let output = g.slot::<Value<u32>>("output");
    g.external(input);
    g.stage_skip_when_clean(
        "double",
        (Input(input), Output(output)),
        |i: &Value<u32>, o: &mut Value<u32>| {
            o.set(*i.get_valid().unwrap() * 2);
            Ok(Flow::Continue)
        },
    );
    let mut p = g.build().expect("valid graph");

    // No window until the first reset.
    assert!(p.stats_age().is_none());

    p.collect_stats(true);
    p.reset_stats();
    assert!(p.stats_age().is_some());

    p.get_mut(input).set(1);
    p.compute().unwrap(); // run
    p.compute().unwrap(); // skip
    assert_eq!(p.stats()[0].ran, 1);
    assert_eq!(p.stats()[0].skipped, 1);

    // Reset zeros the counters and restarts the window.
    p.reset_stats();
    assert_eq!(p.stats()[0].ran, 0);
    assert_eq!(p.stats()[0].skipped, 0);
    assert_eq!(p.stats()[0].time, std::time::Duration::ZERO);
    assert!(p.stats_age().is_some());
}
