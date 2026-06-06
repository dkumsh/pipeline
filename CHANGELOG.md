## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.0] - 2026-06-06

### Added

- **`pipeline-graph`** — a new crate: a dynamic, runtime-wired counterpart to the
  static `#[pipeline]` macro. Assemble the dataflow graph at runtime (`Graph`,
  typed `Slot<T>` handles, `Input`/`Output` ports, `stage`), with the same wiring
  rules enforced at `build()` (single-writer, missing-producer, disjointness,
  acyclicity) and the same `pipeline-core` value layer. Type-erased node store
  with a small encapsulated `unsafe` core, validated under Miri (`just miri`).
  Includes a `telemetry_monitor` example and `doc/design.md`.

### Changed (breaking — workspace restructure)

- Split into a layered set of crates over a shared value layer:
  - **`pipeline-core`** (lib `pipeline`) — the value layer (`Value`/`Vector`/
    `Buckets`) and `Reset`/`Error`. Now also root-re-exports the value types, so
    `pipeline::Vector` works alongside `pipeline::value::Vector`.
  - **`pipeline-macros`** — the `#[pipeline]`/`#[stage]` proc macros (renamed
    from `pipeline-dsl-macros`).
  - **`pipeline-dsl`** (lib `pipeline_dsl`) — the static front-end. **Macros
    import moves: `use pipeline::{pipeline, stage}` → `use pipeline_dsl::{pipeline,
    stage}`.** Self-contained: it re-exports the value layer, and the macro's
    generated code resolves its runtime support through `pipeline-dsl`, so a
    single `pipeline-dsl` dependency suffices (`pipeline_dsl::Vector`).
  - **`pipeline-graph`** (lib `pipeline_graph`) — the dynamic front-end; depends
    only on `pipeline-core` (no macros pulled in) and re-exports the value layer
    (`pipeline_graph::Vector`), so it too is a single dependency.
- Either front-end can be used alone. If you *also* depend on `pipeline-core`
  directly, the value types are available under the shared `pipeline::` name
  (`pipeline::Vector`) — the same re-exported types.

## [0.3.4] - 2026-06-04

### Added

- `Vector::with_invalid_slots(len)` (where `V: Default`) — bulk constructor that
  pre-allocates `len` slots all starting **invalid** and **clean**. The backing
  storage is allocated so `commit(i, ..)` for `i < len` won't panic, but each
  slot reads as `None` via `get_valid` until committed. Mirror of `from_fill`,
  whose slots start valid; useful for an externally-fed buffer of known size
  populated incrementally.

## [0.3.3] - 2026-06-04

### Fixed

- `external = "..."` fields now have their per-cycle dirty state cleared at the
  end of each `compute()`, like stage outputs. In 0.3.2 they were excluded from
  reset, so dirty flags set by the caller accumulated across cycles instead of
  being cleared. (0.3.2 is yanked.)

## [0.3.2] - 2026-06-04

### Added

- `external = "a, b"` pipeline-header attribute — declares externally-fed
  fields. Each becomes a `Default`-initialized `pub` member that no stage writes
  and the caller populates between `compute()` runs, satisfying missing-producer
  detection without the misleading `#[unused]` or forcing the field into `new()`
  via `args`. A field declared `external` that is also written by a stage is
  rejected at compile time.

## [0.3.1] - 2026-06-03

### Added

- `Vector::is_updated_at(index) -> bool` — per-slot dirty test. O(1)
  bit read against the internal dirty bitmap. Mirrors `is_valid`'s
  shape and returns `false` for out-of-bounds indices. Lets downstream
  code answer "is slot N dirty this cycle?" without materialising the
  `iter_updated_indices()` output into a parallel set.

## [0.3.0] - 2026-05-30

### Added

- `Vector::update<F: FnOnce(&mut V)>(index, f)` — in-place mutation
  closure that marks the slot valid + dirty. Useful for large `V`
  to avoid the full-struct move that `Vector::commit(i, value)` does.
- `Vector::iter_updated_indices()` — yields only the `usize` indices
  of dirty slots, in ascending order. Skips the validity check and
  data lookup that `iter_updated_valid` performs; useful for driving
  a parallel walk over another `Vector` or external array keyed by
  the same slot.
- `Vector::from_vec(Vec<V>)` and `Vector::from_fill(value, len)` —
  bulk constructors. Every slot starts **valid** and **clean** (no
  dirty bits set), matching the "loaded from saved state" pattern.
- `Vector::as_slice() -> &[V]` — returns the full underlying slice
  regardless of per-slot validity / dirty state.

### Changed

- `Vector` internals now use bare `Vec<u64>` bitsets for dirty + valid
  tracking, plus a `dirty_count: usize` for O(1) `is_updated`. The
  previous design carried a parallel `Vec<usize>` of dirty indices
  plus an `all_updated` short-circuit; both are gone. No public API
  changes.
- `Vector::iter_updated_valid` now always yields slots in **ascending
  index order**. Previously it yielded in insertion order when only
  some slots were dirty, and in index order once every slot was
  dirty — an inconsistency the new implementation removes.

### Removed

- `bitvec` dependency. Pipeline no longer pulls it in.
