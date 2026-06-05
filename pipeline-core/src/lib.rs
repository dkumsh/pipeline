//! Core value layer shared by the pipeline front-ends.
//!
//! This crate (imported as `pipeline`) provides the dirty/validity-tracking
//! value types — [`value::Value`], [`value::Vector`], [`value::Buckets`] — and
//! the [`Reset`] trait the engines use to clear per-cycle state. It contains no
//! macros and no graph machinery; the static (`pipeline-dsl`) and dynamic
//! (`pipeline-graph`) front-ends are built on top of it.
#![warn(missing_docs)]

pub mod value;

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
