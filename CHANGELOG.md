## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
