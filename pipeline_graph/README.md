# pipeline-graph

The **dynamic**, runtime-wired front-end of the
[`pipeline`](https://github.com/dkumsh/pipeline) family. Where
[`pipeline-dsl`] derives the dataflow graph from function signatures at compile
time, `pipeline-graph` lets you assemble the same shape of graph **at runtime** —
decide which stages exist, pick their implementations, and wire them — while
reusing the same value layer and the same wiring rules.

## Setup

```toml
[dependencies]
pipeline-core  = "0.3"   # the value layer, used as `pipeline::...`
pipeline-graph = "0.3"   # the runtime graph builder
```

## Quick start

```rust
use pipeline::{Value, Vector};
use pipeline_graph::{Flow, Graph, Input, Output};

let mut g = Graph::new();
let leaves = g.slot::<Vector<u32>>("leaves");
let synths = g.slot::<Vector<u32>>("synths");
let total  = g.slot::<Value<u32>>("total");
g.external(leaves); // caller-fed; its dirty flags are cleared each cycle

// Stages take their ports as separate args — like a static `#[stage]`.
g.add("synthesize", (Input(leaves), Output(synths)),
    |l: &Vector<u32>, s: &mut Vector<u32>| {
        for i in 0..l.len() {
            if let Some(v) = l.get_valid(i) { s.push_committed(*v * 10); }
        }
        Ok(Flow::Continue)
    });
g.add("sum", (Input(synths), Output(total)),
    |s: &Vector<u32>, t: &mut Value<u32>| {
        t.set(s.as_slice().iter().sum());
        Ok(Flow::Continue)
    });

let mut p = g.build().expect("valid graph"); // validates + topologically sorts
p.set(leaves, Vector::from_vec(vec![1, 2, 3]));
p.compute().unwrap();
assert_eq!(p.get(total).get_valid(), Some(&60));
```

## What you get

- **Typed handles (`Slot<T>`)** — value type-safety is a compile-time property;
  no `Any`, no downcast.
- **`Input`/`Output` ports** — a stage receives exactly the references it
  declares, as separate args (closures *and* free `fn`s).
- **`build()`** — validates single-writer, missing-producer, intra-stage
  disjointness, and acyclicity, then topologically sorts (registration order
  doesn't matter).
- **Per-cycle reset**, **`Flow::Break`** early-exit, **runtime reconfiguration**
  (choose stages/implementations/wiring at runtime), and a `dot()` diagram.

See `examples/telemetry_monitor.rs` for a complete worked example and
`doc/design.md` for concepts, design decisions, and the type-erasure chapter.

## Related crates

Part of the **pipeline** family — a shared value layer with two front-ends:

| Crate | What it is |
|---|---|
| [`pipeline-core`] | the value layer (`Value`/`Vector`/`Buckets` + `Reset`), imported as `pipeline` |
| [`pipeline-dsl`] | **static** front-end: derive the graph at compile time with `#[pipeline]`/`#[stage]` |
| [`pipeline-graph`] | **dynamic** front-end: wire the graph at runtime (`Graph`, `Input`/`Output`) |

Graph fixed and known at compile time? Prefer [`pipeline-dsl`] — its compile-time
checks and zero runtime wiring cost.

[`pipeline-core`]: https://crates.io/crates/pipeline-core
[`pipeline-dsl`]: https://crates.io/crates/pipeline-dsl
[`pipeline-graph`]: https://crates.io/crates/pipeline-graph

## License

MIT OR Apache-2.0.
