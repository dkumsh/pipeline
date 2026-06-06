# pipeline-core

The shared **value layer** for the [`pipeline`](https://github.com/dkumsh/pipeline)
family. This crate is imported as `pipeline` and provides the dirty- and
validity-tracking value types plus the `Reset` trait the engines use to clear
per-cycle state.

You usually don't depend on this crate directly — pick a front-end
([`pipeline-dsl`] for the static macro, or [`pipeline-graph`] for the dynamic
runtime graph); each **re-exports** these types, so a single front-end
dependency is enough (`pipeline_dsl::Vector`, `pipeline_graph::Vector`). Depend
on `pipeline-core` directly only if you want to refer to the types under the
shared `pipeline::Vector` name across crates — they're the same types either
way.

## What's here

- **`Value<T>`** — a single dirty/validity-tracked cell.
- **`Vector<T>`** — a `Vec`-like container with per-slot dirty *and* validity
  bits (incremental, cycle-based recompute).
- **`Buckets<T>`** — indexed accumulation.
- **`Reset`** — clears per-cycle *dirty* state (contents/validity are preserved).
- **`Error`** — the shared error type.

```rust
use pipeline::Vector;

let mut v: Vector<u32> = Vector::with_invalid_slots(3); // 3 slots, all invalid
v.commit(0, 10);                 // slot 0 now valid + dirty
assert!(v.is_updated_at(0));
assert_eq!(v.get_valid(0), Some(&10));
```

## Related crates

Part of the **pipeline** family — a shared value layer with two front-ends:

| Crate | What it is |
|---|---|
| [`pipeline-core`] | the value layer (`Value`/`Vector`/`Buckets` + `Reset`), imported as `pipeline` |
| [`pipeline-dsl`] | **static** front-end: derive the graph at compile time with `#[pipeline]`/`#[stage]` |
| [`pipeline-graph`] | **dynamic** front-end: wire the graph at runtime (`Graph`, `Input`/`Output`) |

Fixed pipeline known at compile time → [`pipeline-dsl`]. Wiring decided at
runtime → [`pipeline-graph`].

[`pipeline-core`]: https://crates.io/crates/pipeline-core
[`pipeline-dsl`]: https://crates.io/crates/pipeline-dsl
[`pipeline-graph`]: https://crates.io/crates/pipeline-graph

## License

MIT OR Apache-2.0.
