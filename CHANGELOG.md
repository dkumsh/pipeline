## Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- `Vector::update<F: FnOnce(&mut V)>(index, f)` — in-place mutation
  closure that marks the slot valid + dirty. Useful for large `V`
  to avoid the full-struct move that `Vector::commit(i, value)` does.
