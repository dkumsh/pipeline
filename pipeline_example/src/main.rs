use pipeline::value::Vector;
#[allow(unused_imports)]
use pipeline_derive::{pipeline, stage};
use std::fs;

#[derive(Debug, thiserror::Error)]
pub enum AppError {
    #[error(transparent)]
    Pipeline(#[from] pipeline::Error), // so Reset errors map into AppError
    #[error(transparent)]
    Io(#[from] std::io::Error), // used by write_html_to_file in main()
    #[error("bad data: {0}")]
    BadData(String),
}

#[allow(unused_variables)]
#[pipeline(
    name = "MyPipeline",
    args = "config",
    context = "ctx",
    error = "AppError"
)]
pub mod calculator {
    use super::AppError;
    use crate::{Config, Context, Mid, TopOfBook};
    use pipeline::value::Vector;

    #[allow(unused_imports)]
    use pipeline_derive::{pipeline, stage};

    #[stage]
    pub fn print_mid(config: &Config, mid: &Vector<Mid>) -> Result<(), AppError> {
        // stage logic here
        Ok(())
    }

    #[stage]
    pub fn mid(
        #[unused] mid_state: &(),
        config: &Config,
        ctx: &mut Context,
        #[unused] tob: &Vector<TopOfBook>,
        mid: &mut Vector<Mid>,
    ) -> Result<(), AppError> {
        // stage logic here
        Ok(())
    }
}

#[derive(Default)]
pub struct Config();

#[derive(Default)]
pub struct Context();

#[derive(Default)]
pub struct TopOfBook();
#[derive(Default)]
pub struct Mid();

fn main() -> Result<(), AppError> {
    let config = Config::default();
    let mut context = Context::default();
    let pipeline = MyPipeline::new(config);

    // Get the PUML content
    // let puml_content = MyPipeline::puml_diagram();

    // Write the PUML content to a file
    // std::fs::write("pipeline_diagram.puml", puml_content).unwrap();

    // Write the HTML content to a
    fs::create_dir_all("target/graph/")?;
    MyPipeline::write_html_to_file("target/graph/pipeline_graph.html")?;

    // You can now use PlantUML to generate the diagram:
    // java -jar plantuml.jar pipeline_diagram.puml

    // Run the pipeline
    let mut pipeline = pipeline;
    pipeline.compute(&mut context)?;
    Ok(())
}
