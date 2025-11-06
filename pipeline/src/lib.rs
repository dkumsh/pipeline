pub use pipeline_dsl_macros::{pipeline, stage};

pub mod value;
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
