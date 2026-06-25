//! Core value layer shared by the pipeline front-ends.
//!
//! This crate (imported as `pipeline`) provides the dirty/validity-tracking
//! value types — [`value::Value`], [`value::Vector`], [`value::Buckets`] — and
//! the [`Reset`] trait the engines use to clear per-cycle state. It contains no
//! macros and no graph machinery; the static (`pipeline-dsl`) and dynamic
//! (`pipeline-graph`) front-ends are built on top of it.
#![warn(missing_docs)]

pub mod value;

#[cfg(feature = "parallel")]
pub use value::par_update2;
/// Convenience root re-exports so callers can write `pipeline::Vector` etc.
/// (the canonical paths under `pipeline::value::*` remain available too).
pub use value::{Buckets, Value, Vector};

use thiserror::Error;

/// Errors produced by the value layer.
#[derive(Error, Debug, PartialEq)]
pub enum Error {
    /// A value was read while uninitialised (no value held).
    #[error("accessing uninitialised value")]
    UninitialisedValue,
}

/// Clears a value's **per-cycle dirty state** (contents and validity are
/// preserved). The pipeline engines call this at the end of each `compute()`
/// on written and externally-fed nodes.
pub trait Reset {
    /// Error type returned by [`Reset::reset`].
    type Error;
    /// Clear per-cycle dirty state.
    fn reset(&mut self) -> Result<(), Self::Error>;
}

/// Query a value's **per-cycle dirty state**: whether it changed this cycle
/// (written *or* invalidated). The pipeline engines use this to decide whether a
/// stage has any fresh input. The read-side mirror of [`Reset`].
pub trait Updated {
    /// Whether the value changed this cycle.
    fn is_updated(&self) -> bool;
}

/// Per-stage execution counters, shared by both front-ends' optional stats
/// support (`pipeline-graph`'s `Pipeline::stats`, `pipeline-dsl`'s
/// `#[pipeline(stats)]`). Stored contiguously and handed out by reference, so
/// reading them costs nothing regardless of how this struct grows.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct StageStats {
    /// Stage name.
    pub name: String,
    /// Number of cycles the stage actually ran.
    pub ran: u64,
    /// Number of cycles the stage was skipped because no input was dirty
    /// (only possible for `skip_when_clean` stages).
    pub skipped: u64,
    /// Total wall-clock time spent running the stage.
    pub time: std::time::Duration,
}
