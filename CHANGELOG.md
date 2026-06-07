## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `pipeline-graph`: opt-in **demand-driven scheduling**. `Graph::stage_skip_when_clean`
  registers a stage the engine may skip in any cycle where none of its declared
  `Input` slots changed (`is_updated()`); a skipped stage doesn't run and doesn't
  write, so "unchanged" propagates to dependents (and a valid→invalid transition,
  now dirty, wakes consumers). The body must be a pure function of its declared
  inputs. Plain `Graph::stage` is unchanged (always runs).
- `pipeline-graph`: optional per-stage stats. `Pipeline::collect_stats(bool)`
  toggles collection (off by default — no counter writes, no clock reads on the
  hot path; `compute` monomorphizes into a stats / no-stats flavor), and
  `Pipeline::stats() -> &[StageStats]` returns a borrowed slice (no per-call
  allocation) of per-stage `{ name, ran, skipped, time }` in registration order.
  `Pipeline::reset_stats()` zeroes the counters and starts a fresh window;
  `Pipeline::stats_age()` reports the monotonic time since that reset (for
  turning counts into rates / utilization).
- `pipeline-core`: `Updated` trait (`is_updated()`) — the read-side mirror of
  `Reset`, implemented by `Value` / `Vector` / `Buckets`, letting an engine query
  dirtiness generically.

### Changed

- `pipeline-dsl`: `args` (constructor config) and `#[state]` fields of the
  generated struct are now **private** instead of `pub` — they're
  pipeline-internal. Stage outputs and `external` inputs stay `pub`. In-crate
  code (the module defining the pipeline) can still reach them, e.g. to seed
  state post-`new()`; **potentially breaking** only for downstream crates that
  read/wrote those fields.
- `pipeline-core`: `Value` now tracks **two orthogonal bits** — validity
  (`Option<T>`) and dirtiness — mirroring a `Vector` slot, instead of the old
  three/four-way `State` enum (the public `State` type is removed; nothing in
  the API surfaced it). Consequently `Value::invalidate()` now marks the cell
  **dirty** (`is_updated()` reports the valid→invalid transition), matching
  `Vector::invalidate`, so a "became invalid" signal propagates to readers that
  schedule on dirtiness. `reset()` just clears the dirty bit (validity persists);
  invalidating an already-empty cell stays a no-op (no spurious dirty).

## [0.7.0] - 2026-06-07

### Changed

- `pipeline-dsl`: a pipeline with no `args` now also gets a generated
  `impl Default`. **Potentially breaking**: if you already hand-wrote
  `impl Default` for such a pipeline, it now conflicts (E0119) — remove your impl
  or use `#[pipeline(constructor = "manual")]` (which suppresses both the
  generated `new()` and `Default`).

### Added

- `pipeline-dsl`: `#[state]` stage-parameter attribute — per-stage-private,
  persistent, plain-`T` scratch the pipeline owns and exactly one stage mutates
  (`&mut T`). Unlike a slot it is off the dataflow graph: never reset, never read
  by another stage, and hidden from `dot()`/`html_diagram()`. Requires `&mut T`;
  sharing one state across stages, or colliding with an arg/context/external/slot
  name, is a compile error. (The dynamic `pipeline-graph` front-end needs no
  equivalent — its closure stages capture private state directly.)
- `pipeline-dsl`: smarter constructor generation. The generated `new()` now
  bounds each `Default`-initialized field by `Default` (a non-`Default` field
  gives a clear `T: Default is not satisfied` error instead of one buried in
  generated code), a pipeline with no `args` also derives `Default`, and
  `#[pipeline(constructor = "manual")]` suppresses the generated `new()`/`Default`
  so you can write your own (e.g. for a field whose type is not `Default`).

## [0.6.0] - 2026-06-06

### Added

- Added `pipeline-diagram`, a shared, standalone HTML renderer (`render_html`,
  plus an optional `graph_json` helper) used by both the static macro front-end
  and the dynamic runtime graph front-end.
- `pipeline-graph` now exposes runtime HTML diagram generation via
  `Pipeline::diagram_json()`, `Pipeline::html_diagram()`, and
  `Pipeline::write_html_to_file(...)`.
- The rendered diagram is interactive (vis-network 9: pan/zoom, click-for-detail,
  a slide-out layout/physics controls pane) and records an optional
  `metadata.generated_at` timestamp in its footer.

## [0.5.0] - 2026-06-06

### Changed (front-ends are now single-dependency)

- A `#[pipeline]` / `pipeline-graph` user now depends on **only the front-end
  crate**; each re-exports the value layer.
  - `pipeline-macros` now emits `pipeline_dsl::…` paths (was `pipeline::…`), so
    the macro's generated code resolves its runtime support through
    `pipeline-dsl` rather than requiring a direct `pipeline-core` dependency.
  - `pipeline-dsl` re-exports the value layer + `Reset`/`Error`
    (`pipeline_dsl::Vector`); `pipeline-graph` re-exports the value layer
    (`pipeline_graph::Vector`).
  - The shared `pipeline::Vector` name still works **if** you also depend on
    `pipeline-core` directly — the same re-exported types.
- `pipeline-dsl` now exact-pins `pipeline-macros` (`=0.5.0`): the macro's emitted
  paths are coupled to the front-end's re-exports, so they move in lockstep.

> Minor bump (not 0.4.1) on purpose: the changed macro emission is incompatible
> with the already-published `pipeline-dsl 0.4.0` (which doesn't re-export the
> needed items). A minor bump isolates the new, self-consistent crate set;
> `0.4.0` users on `^0.4` are unaffected.

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
  - **`pipeline-dsl`** (lib `pipeline_dsl`) — the static front-end; re-exports the
    macros. **Macros import moves: `use pipeline::{pipeline, stage}` → `use
    pipeline_dsl::{pipeline, stage}`.** Value types come from `pipeline`
    (`pipeline-core`), which a `#[pipeline]` user must depend on directly.
  - **`pipeline-graph`** (lib `pipeline_graph`) — the dynamic front-end; depends
    only on `pipeline-core` (no macros pulled in); value types from
    `pipeline::{Value, Vector}`.

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
