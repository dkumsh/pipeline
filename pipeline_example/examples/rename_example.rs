use pipeline::pipeline;
use pipeline::value::Vector;
use thiserror::Error;

/// Custom error type that converts pipeline::Error via `From`
#[derive(Debug, Error)]
pub enum AppError {
    #[error(transparent)]
    Pipeline(#[from] pipeline::Error),
}

#[pipeline(name = "MyPipeline", args = "config", error = "AppError")]
#[allow(unused_variables)]
#[allow(clippy::disallowed_names)]
pub mod example {
    use super::{AppError, Config, Foo};
    use pipeline::value::Vector;
    use pipeline_derive::stage;

    #[stage]
    // The parameter is named `my_foo` in this function,
    // but binds to the field `foo` in the generated pipeline struct
    pub fn load_foo(
        config: &Config,
        #[rename = "foo"] _my_foo: &mut Vector<Foo>,
    ) -> Result<(), AppError> {
        // populate my_foo here...
        Ok(())
    }

    #[pipeline::stage]
    pub fn print_foo(config: &Config, foo: &Vector<Foo>) -> Result<(), AppError> {
        // read foo here...
        Ok(())
    }
}

/// Dummy types for the example
#[derive(Default)]
pub struct Config;
#[derive(Default)]
pub struct Foo;

fn main() -> Result<(), AppError> {
    let config = Config;
    // Note: MyPipeline is at crate root, not under `example::`
    let mut pipeline = MyPipeline::new(config);
    pipeline.compute()?; // no &mut context param, because we removed `context = "ctx"`
    Ok(())
}
