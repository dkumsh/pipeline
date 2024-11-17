pub mod value;
use thiserror::Error;
#[derive(Error, Debug, PartialEq)]
pub enum Error {
    #[error("accessing uninitialised value")]
    UninitialisedValue,
}
pub use crate::value::Reset as Reset;
