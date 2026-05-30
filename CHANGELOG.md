## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `Vector::update<F: FnOnce(&mut V)>(index, f)` — in-place mutation
  closure that marks the slot valid + dirty. Useful for large `V`
  to avoid the full-struct move that `Vector::commit(i, value)` does.

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
