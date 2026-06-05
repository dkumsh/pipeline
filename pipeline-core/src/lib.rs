//! Core value layer shared by the pipeline front-ends.
//!
//! This crate (imported as `pipeline`) provides the dirty/validity-tracking
//! value types — [`value::Value`], [`value::Vector`], [`value::Buckets`] — and
//! the [`Reset`] trait the engines use to clear per-cycle state. It contains no
//! macros and no graph machinery; the static (`pipeline-dsl`) and dynamic
//! (`pipeline-graph`) front-ends are built on top of it.

pub mod value;

/// Convenience root re-exports so callers can write `pipeline::Vector` etc.
/// (the canonical paths under `pipeline::value::*` remain available too).
pub use value::{Buckets, Value, Vector};

use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("accessing uninitialised value")]
    UninitialisedValue,
}

pub trait Reset {
    type Error;
    fn reset(&mut self) -> Result<(), Self::Error>;
}
