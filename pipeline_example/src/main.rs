use pipeline::value::Vector;
use pipeline_derive::pipeline;
use price_calc::*;
fn reset<T>(_:&mut T){}
#[pipeline(name = "MyPipeline", args = "keys")]
pub mod price_calc {
    use pipeline_derive::stage;
use pipeline::value::Vector;
    use pipeline_derive::pipeline;
    use crate::{Keys, Mid, TopOfBook};

    #[stage]
    pub fn print_mid(mid: &Vector<Mid>) {
    }

    #[stage]
    pub fn mid(mid_state: &(), keys: &Keys, tob: &Vector<TopOfBook>, mid: &mut Vector<Mid>) {

    }

}


#[derive(Default)]
struct Keys();

#[derive(Default)]
struct TopOfBook();
#[derive(Default)]
struct Mid();

fn main() {
    let keys = Keys::default();
    let pipeline = MyPipeline::new(keys);

    // Get the PUML content
    let puml_content = MyPipeline::puml_diagram();

    // Write the PUML content to a file
    std::fs::write("pipeline_diagram.puml", puml_content).unwrap();

    // You can now use PlantUML to generate the diagram:
    // java -jar plantuml.jar pipeline_diagram.puml

    // Run the pipeline
    let mut pipeline = pipeline;
    pipeline.compute().unwrap();
}
