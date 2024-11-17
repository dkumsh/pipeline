use pipeline::value::Vector;
#[allow(unused_imports)]
use pipeline_derive::{pipeline, stage};
use price_calc::*;
fn reset<T>(_: &mut T) {}
#[allow(unused_variables)]
#[pipeline(name = "MyPipeline", args = "config", context = "ctx: Context")]
pub mod price_calc {
    use crate::{Context, Mid, TopOfBook, Config};
    use pipeline::value::Vector;

    #[allow(unused_imports)]
    use pipeline_derive::{pipeline, stage};

    #[stage]
    pub fn print_mid(config: &Config, ctx: &mut crate::Context, mid: &Vector<Mid>) {}

    #[stage]
    pub fn mid(mid_state: &(), config: &Config, ctx: &mut Context, tob: &Vector<TopOfBook>, mid: &mut Vector<Mid>) {}
}

#[derive(Default)]
pub struct Config();

#[derive(Default)]
pub struct Context();

#[derive(Default)]
pub struct TopOfBook();
#[derive(Default)]
pub struct Mid();

fn main() {
    let config = Config::default();
    let context = Context::default();
    let pipeline = MyPipeline::new(config,context);

    // Get the PUML content
    let puml_content = MyPipeline::puml_diagram();

    // Write the PUML content to a file
    std::fs::write("pipeline_diagram.puml", puml_content).unwrap();

    // Write the HTML content to a file
    MyPipeline::write_html_to_file("pipeline_graph.html").unwrap();

    // You can now use PlantUML to generate the diagram:
    // java -jar plantuml.jar pipeline_diagram.puml

    // Run the pipeline
    let mut pipeline = pipeline;
    pipeline.compute().unwrap();
}
