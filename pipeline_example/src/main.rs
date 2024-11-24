use anyhow::Error;
use pipeline::value::Vector;
#[allow(unused_imports)]
use pipeline_derive::{pipeline, stage};
use price_calc::*;
use std::fs;
fn reset<T>(_: &mut T) {}
#[allow(unused_variables)]
#[pipeline(name = "MyPipeline", args = "config", context = "ctx")]
pub mod price_calc {
    use crate::{Config, Context, Mid, TopOfBook};
    use pipeline::value::Vector;

    #[allow(unused_imports)]
    use pipeline_derive::{pipeline, stage};

    #[stage]
    pub fn print_mid(config: &Config, mid: &Vector<Mid>) {}

    #[stage]
    pub fn mid(
        mid_state: &(),
        config: &Config,
        ctx: &mut Context,
        tob: &Vector<TopOfBook>,
        mid: &mut Vector<Mid>,
    ) {
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

fn main() -> anyhow::Result<(), Error> {
    let config = Config::default();
    let mut context = Context::default();
    let pipeline = MyPipeline::new(config);

    // Get the PUML content
    // let puml_content = MyPipeline::puml_diagram();

    // Write the PUML content to a file
    // std::fs::write("pipeline_diagram.puml", puml_content).unwrap();

    // Write the HTML content to a
    fs::create_dir_all("target/graph/")?;
    MyPipeline::write_html_to_file("target/graph/pipeline_graph.html").unwrap();

    // You can now use PlantUML to generate the diagram:
    // java -jar plantuml.jar pipeline_diagram.puml

    // Run the pipeline
    let mut pipeline = pipeline;
    pipeline.compute(&mut context).unwrap();
    Ok(())
}
