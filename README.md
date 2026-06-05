# pipeline

A family of crates for **graph-shaped, incremental computation** in Rust: write
stages that read and write tracked values, and let the framework wire them into
a dependency DAG and run them in topological order — recomputing only what
changed each cycle.

There are two front-ends over one shared value layer. Pick by **when the graph
is decided**:

| Crate | Import | What it is | Use when |
|---|---|---|---|
| **[`pipeline-core`](pipeline-core)** | `pipeline` | the value layer: `Value` / `Vector` / `Buckets` + `Reset` | always (both front-ends build on it) |
| **[`pipeline-dsl`](pipeline-dsl)** | `pipeline_dsl` | **static** front-end — `#[pipeline]`/`#[stage]` macros derive the graph from function signatures | the graph is fixed and known at compile time |
| **[`pipeline-graph`](pipeline-graph)** | `pipeline_graph` | **dynamic** front-end — assemble the graph at runtime (`Graph`, `Input`/`Output`) | which stages/implementations/wiring exist is decided at runtime |
| [`pipeline-macros`](pipeline-macros) | — | internal proc-macro crate behind `pipeline-dsl` | (don't depend on directly) |

Both front-ends depend on `pipeline-core` and refer to the value types by one
canonical name, `pipeline::Vector`, `pipeline::Value`, etc.

## Which one?

- **Compile-time graph, maximum checking, zero runtime wiring cost** →
  [`pipeline-dsl`](pipeline-dsl). The macro enforces single-writer, missing-producer,
  and ordering as *compile errors*.
- **Runtime graph — choose stages, implementations, or wiring dynamically** →
  [`pipeline-graph`](pipeline-graph). The same rules are enforced by `build()` at
  startup; supports runtime reconfiguration and a live `dot()` diagram.

Both share `pipeline-core`'s dirty/validity-tracking values, so the
"recompute only what changed" model works identically in either.

## Quick taste

Static (`pipeline-dsl`):

```rust
use pipeline_dsl::{pipeline, stage};

#[pipeline(name = "App", context = "db, cache")]
mod app {
    use super::*;
    #[stage] pub fn tick(db: &mut Db) { db.count += 1; }
    #[stage] pub fn sum(cache: &mut Cache, db: &Db) { cache.total += db.count; }
}
```

Dynamic (`pipeline-graph`):

```rust
use pipeline::{Value, Vector};
use pipeline_graph::{Flow, Graph, Input, Output};

let mut g = Graph::new();
let xs  = g.slot::<Vector<u32>>("xs");
let sum = g.slot::<Value<u32>>("sum");
g.external(xs);
g.add("sum", (Input(xs), Output(sum)),
    |xs: &Vector<u32>, s: &mut Value<u32>| { s.set(xs.as_slice().iter().sum()); Ok(Flow::Continue) });
let mut p = g.build().unwrap();
```

See each crate's README and docs for full guides.

## License

MIT OR Apache-2.0.
