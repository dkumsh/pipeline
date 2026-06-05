use pipeline::{Value, Vector};
use pipeline_graph::{Flow, Graph, GraphError, Input, Output};

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
