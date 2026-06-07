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
| [`pipeline-diagram`](pipeline-diagram) | — | shared HTML diagram renderer used by both front-ends | (don't depend on directly) |

Each front-end **re-exports** the value layer, so you depend on just the one
front-end crate (`pipeline_dsl::Vector` / `pipeline_graph::Vector`). If you also
depend on `pipeline-core` directly, the same types are available under the
shared `pipeline::Vector` name — they're the identical re-exported types.

## Which one?

- **Compile-time graph, maximum checking, zero runtime wiring cost** →
  [`pipeline-dsl`](pipeline-dsl). The macro enforces single-writer, missing-producer,
  and ordering as *compile errors*.
- **Runtime graph — choose stages, implementations, or wiring dynamically** →
  [`pipeline-graph`](pipeline-graph). The same rules are enforced by `build()` at
  startup; supports runtime reconfiguration and live DOT/HTML diagrams.

Both share `pipeline-core`'s dirty/validity-tracking values, so the
"recompute only what changed" model works identically in either.

**Demand-driven scheduling (opt-in, both front-ends).** Mark a stage
`skip_when_clean` and it's skipped in any cycle where none of its inputs changed
— a skipped stage doesn't run or write, so "unchanged" propagates to its
readers. Opt into per-stage stats (`#[pipeline(stats)]` / `Pipeline::collect_stats`)
to see how often each stage actually does work (run/skip counts + timing). See
the runnable [`demand_driven`](pipeline-example/examples/demand_driven.rs) example.

**Safety asymmetry (deliberate):** the static front-end is fully compile-time
checked and contains no `unsafe`. The dynamic front-end trades that for runtime
flexibility — wiring is validated at `build()` instead of by the compiler, and
its type-erased store has a small encapsulated `unsafe` core (checked under
Miri). Stage code is safe Rust either way. Prefer the static front-end when a
fixed, compile-time graph fits.

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
use pipeline_graph::{Flow, Graph, Input, Output, Value, Vector};

let mut g = Graph::new();
let xs  = g.slot::<Vector<u32>>("xs");
let sum = g.slot::<Value<u32>>("sum");
g.external(xs);
g.stage("sum", (Input(xs), Output(sum)),
    |xs: &Vector<u32>, s: &mut Value<u32>| { s.set(xs.as_slice().iter().sum()); Ok(Flow::Continue) });
let mut p = g.build().unwrap();
```

See each crate's README and docs for full guides.

## Visualize the graph

Either front-end can render its wired graph as a standalone, self-contained
interactive HTML page (via [`pipeline-diagram`](pipeline-diagram)) — pan/zoom,
click a node for details, and a slide-out pane for layout/physics controls.
Here's the [`telemetry_monitor`](pipeline-graph/examples/telemetry_monitor.rs)
example (green boxes are stages, blue ellipses are values):

![Interactive pipeline dependency graph](https://raw.githubusercontent.com/dkumsh/pipeline/main/pipeline-graph/doc/telemetry_monitor.png)

## License

MIT OR Apache-2.0.
