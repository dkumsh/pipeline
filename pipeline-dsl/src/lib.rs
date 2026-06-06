//! Static pipeline DSL — the `#[pipeline]` and `#[stage]` attribute macros.
//!
//! This crate is **self-contained**: it re-exports the value layer from
//! [`pipeline-core`], so a single `pipeline-dsl` dependency is enough. Refer to
//! the value types as `pipeline_dsl::Vector`, `pipeline_dsl::Value`, etc., and
//! the macro's generated code resolves its runtime support (`Reset`, `Error`)
//! through this crate too.
//!
//! If you also depend on [`pipeline-core`] directly, the same types are equally
//! available under the shared `pipeline::` name (`pipeline::Vector`) — they are
//! the identical re-exported types.
//!
//! [`pipeline-core`]: https://crates.io/crates/pipeline-core
pub use pipeline_macros::{pipeline, stage};

// Re-export the value layer so `#[pipeline]` users need only this one crate.
// `Reset` and `Error` are what the generated code references; the value types
// are re-exported for caller convenience (`pipeline_dsl::Vector`).
pub use pipeline::{Buckets, Error, Reset, Value, Vector, value};
