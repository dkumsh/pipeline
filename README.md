# pipeline — Procedural macro for graph-shaped computation.

> **Functions + names ⇒ DAG ⇒ deterministic compute**  
> Write ordinary functions, let the macro derive the dependency graph, and get a topologically sorted `compute()` with crisp diagnostics.

---

## Table of contents

- [What is this?](#what-is-this)
- [Why?](#why)
- [Conceptual stance](#conceptual-stance-functions--names--dag--deterministic-compute)
- [Features](#features)
- [Quick start](#quick-start)
- [Stages and binding rules](#stages-and-binding-rules)
- [Multiple contexts](#multiple-contexts)
- [Attributes](#attributes)
- [Compile-time guarantees & diagnostics](#compile-time-guarantees--diagnostics)
- [Diagrams](#diagrams)
- [Testing](#testing)
- [Roadmap (non-goals vs goals)](#roadmap-non-goals-vs-goals)
- [FAQ](#faq)
- [License](#license)

---

## What is this?

`pipeline` is a procedural macro that turns a set of **plain Rust functions** into a **computational DAG**. It infers edges by **name-based parameter binding** and **mutability**, enforces **single-writer** rules, and generates a **deterministically ordered** `compute()` for you.

You keep writing normal Rust; the macro does the graph work.

```rust
use pipeline::{pipeline, stage};

#[derive(Default)]
struct Db { count: i32 }
#[derive(Default)]
struct Cache { total: i32 }

#[pipeline(name="App", context="db, cache")]
mod app {
    use super::*;

    #[stage]
    pub fn tick(db: &mut Db) { db.count += 1; }

    #[stage]
    pub fn sum(cache: &mut Cache, db: &Db) {
        cache.total += db.count;
    }
}

fn main() -> Result<(), pipeline::Error> {
    let mut app = App::new();
    let mut db = Db::default();
    let mut cache = Cache::default();
    app.compute(&mut db, &mut cache)?;
    Ok(())
}
```

---

## Why?

- **Tiny DSL without a DSL.** Signatures are the wiring. No builders or config languages.
- **Testable by construction.** Each stage is just a function you can unit test in isolation.
- **Deterministic.** The macro topologically sorts the graph with a stable tiebreaker.
- **Compositional.** Functions are reusable across pipelines; pipelines can be composed.

---

## Conceptual stance: functions + names ⇒ DAG ⇒ deterministic compute

**Core move.** A procedural macro reads ordinary Rust functions (“stages”) and **derives a computational DAG** from their signatures. **Names drive binding** (with normalization and optional `#[rename]`), and **mutability signals direction**: `&T` is a read, `&mut T` is a write. From this, the macro:

- checks **single-writer** invariants;
- builds the **dependency graph** (writer → readers);
- emits a **topologically sorted `compute()`** with deterministic ties.

### Why this framing is powerful

1. **Compact, idiomatic surface.** No bespoke language; just Rust.
2. **Trivial unit testing.** Stages are plain functions.
3. **Determinism.** Predictable, reproducible order.
4. **Context as the sanctioned escape hatch.** Shared mutable state (I/O, caches, metrics) flows through explicit `&mut Context` parameters. If any stage needs `&mut Ctx`, the generated `compute` requires `&mut Ctx`; otherwise it accepts `&Ctx`.
5. **Reusability & composition.** Helpers and stages can be reused; a pipeline can conceptually be embedded as a stage (namespaced, override via `#[rename]`).

### Not the core

`Value<T>`, `Vector<T>`, or any concrete containers are **examples** to demonstrate change-detection/clearing; they’re not essential to the concept. The essence is **graph from signatures**.

### How the ideas deepen (without bloating the surface)

- **Stronger compile-time guarantees** with precise diagnostics (missing producer, multi-writer, cycles with the small involved set).
- **Incremental/dirty semantics** as a model (pluggable change detection).
- **Capabilities within context** (`ctx.store`, `ctx.clock`, `ctx.log`) to keep effects honest and future-proof.
- **Pipelines-as-stages** for hierarchical graphs with automatic namespacing.

---

## Features

- Attribute macro `#[pipeline]` → generates a pipeline struct and `compute()`.
- Attribute `#[stage]` → marks a function as a stage.
- **Name-based binding**: parameter names bind to pipeline fields.
- **Normalization**: leading `_` is ignored for binding (`_db` binds to `db`).
- **`#[rename]`**: override binding name per parameter.
- **Contexts**: one or more **context parameters** passed to `compute()`; mutability escalates if any stage requires it.
- **Single-writer enforcement** (&mut targets).
- **Missing-producer detection** (readers must have a producer or be constructor args/contexts).
- **Deterministic topo order** (stable tie-breaking).
- Optional **diagram emitters** (PlantUML string and HTML graph data).

---

## Quick start

Add to your workspace and import `pipeline` where you define your stages.

```rust
use pipeline::{pipeline, stage};

#[derive(Default)] struct Db { count: i32 }
#[derive(Default)] struct Cache { total: i32 }

#[pipeline(name="App", context="db, cache")]
mod app {
    use super::*;

    #[stage]
    pub fn tick(db: &mut Db) { db.count += 1; }

    #[stage]
    pub fn sum(cache: &mut Cache, db: &Db) {
        cache.total += db.count;
    }
}

#[test]
fn it_runs() {
    let mut app = App::new();
    let mut db = Db::default();
    let mut cache = Cache::default();
    app.compute(&mut db, &mut cache).unwrap();
    assert_eq!(db.count, 1);
    assert_eq!(cache.total, 1);
}
```

---

## Stages and binding rules

- **Readers:** `&T` parameter → read edge from `T` to the stage.
- **Writers:** `&mut T` parameter → write edge from the stage to `T`.
- **Binding by name:** The parameter name (post-normalization) is the field name.
- **Normalization:** leading `_` is stripped for matching; use `#[rename = "name"]` to bind a different field name.
- **Constructor args:** You can mark certain fields as constructor inputs via `args = "foo, bar"`; these are not required to be produced by any stage.

---

## Multiple contexts

Declare contexts in the pipeline attribute: `context = "db, cache"`.

- Parameters named `db` or `_db` (normalization applies) are recognized as context and are **not** turned into pipeline fields.
- The macro inspects **underlying types** across stages (ignoring `&`/`&mut`) to ensure consistency.
- If any stage requests `&mut` for a given context, the generated `compute(&mut db, …)` uses `&mut` for that context; otherwise it uses `&`.

```rust
#[pipeline(name="TwoCtxPipeline", context="db, cache")]
mod two_ctx {
    use super::*;

    #[stage]
    pub fn increment(db: &mut Db) { db.count += 1; }

    #[stage]
    pub fn accumulate(cache: &mut Cache, db: &Db) {
        cache.total += db.count;
    }
}
```

---

## Attributes

- `#[pipeline(name="TypeName", args="…", context="…", error="…", controlflow_break="…", clear_updated_on_break="true|false")]`
  - `name`: pipeline struct name (required).
  - `args`: comma-separated constructor field names.
  - `context`: comma-separated context parameter names.
  - `error`: custom error type; defaults to `pipeline::Error`.
  - `controlflow_break`: enable early-exit `ControlFlow<BreakTy>` support.
  - `clear_updated_on_break`: clear mutated fields when breaking (optional).
- `#[stage]`: marks a function as a stage. Supports parameter attributes:
  - `#[rename = "field"]` or `#[rename("field")]`
  - `#[skip_clear]` for outputs excluded from clearing
  - `#[unused]` to silence “read but never produced/used” lints

---

## Compile-time guarantees & diagnostics

- **Single writer** per field; error lists the competing stages.
- **Missing producer** for a reader (not a constructor arg or context); error points to the parameter span.
- **Cycles**: topological sort error with a concise cycle summary.
- **Context issues**:
  - Missing contexts: listed by name.
  - Type inconsistencies: shows the set of **underlying** types seen per context.
  - Mutability escalation happens automatically; the generated `compute` signature reflects the maximum requirement observed across stages.

Example error (for multiple writers):

```
variable 'price' is written by multiple stages: 'quote_mid' and 'fair_value'
```

Example error (for context mismatch):

```
Context type inconsistencies detected:
  - db: seen underlying types [&Db, &mut Db, &OtherDb]
Underlying types must match across all stages (mutability may differ).
```

---

## Diagrams

The macro can emit:

- **PlantUML** text: `YourPipeline::puml_diagram()`
- **HTML** diagram data (nodes/edges): `YourPipeline::html_diagram()`

These are optional views over the same DAG, useful in reviews and debugging.

---

## Testing

- Test stages as **plain functions** with standard Rust tests/mocks.
- Test pipelines end-to-end by constructing the pipeline, contexts, and initial field values, then calling `compute()` and asserting outcomes.
- Because order is deterministic, flaky “sometimes different order” issues are avoided.

---

## Roadmap (non-goals vs goals)

**Core (stay minimal):**
- Attribute macro → DAG (name-bound, single-writer, topo sort).
- Deterministic scheduling; crisp diagnostics.
- Context as the explicit effect channel.

**Layered additions (opt-in):**
- Incremental/dirty recomputation with pluggable change detection.
- Pipelines-as-stages (hierarchical composition, namespacing).
- Tracing/timing hooks; diagram exporters; optional memoization/parallelism.

---

## FAQ

**Q: Do I have to use special container types?**  
No. `Value<T>`/`Vector<T>` in the repo are examples. The core idea is independent of storage choices.

**Q: How does the macro know who reads/writes what?**  
By reference mutability in the signature: `&T` = read; `&mut T` = write.

**Q: What if two stages must both “update” a value?**  
Model it as a single reducer stage that takes both inputs and writes once. The pipeline enforces the **single-writer** rule.

**Q: Can I use multiple contexts?**  
Yes. List them in `context = "…"`; normalization and type-checking apply per context name.

---

## License

MIT OR Apache-2.0