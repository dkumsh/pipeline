### Current state

  - Error propagation: The core pipeline crate defines a simple Error enum with an UninitialisedValue variant and uses the thiserror derive to implement std::error::Error
raw.githubusercontent.com
. The Reset trait now returns Result<(), Error>
raw.githubusercontent.com
 and is implemented by both Value<T> (tracking if a value is set/updated)
raw.githubusercontent.com
 and Vector<T>, allowing reset failures to propagate.

- Macro improvements: The #[pipeline] macro in pipeline_derive now accepts an error = "YourErrorType" argument. It generates compute() and reset() methods returning Result<(), YourErrorType>, mapping any pipeline::Error into the user‑specified type. It also only calls Reset::reset() on fields that are written by some stage (outputs), so inputs or context types aren’t forced to implement Reset.

- Diagram separation: The HTML template used to visualise the pipeline’s DAG has been moved out of the macro and lives in pipeline_derive/assets/pipeline_graph.html. The macro includes this at compile time and performs placeholder substitution for the pipeline name and the JSON data for nodes and edges
github.com
. The PlantUML generator still produces a static &'static str for quick UML diagrams.

### High‑level roadmap for gradual improvements

  1. _**Polish the API and documentation**_

     * Add full crate‑level docs and usage examples for pipeline, pipeline_derive and the vector types. Explain the semantics of State, Value, Vector, and the Reset trait. Document the attribute arguments (name, args, context, error) and the conventions for stage functions.

     * Provide a dedicated examples folder with multiple real‑world scenarios—e.g. a simple arithmetic pipeline, a financial data pipeline and an asynchronous I/O pipeline (see below).

     * Fill in metadata in Cargo.toml (description, license, authors) and ensure the workspace builds cleanly with cargo publish --dry-run.

  2. _**Compile‑time safety and ergonomics**_

     * Improve diagnostic messages further: report unused outputs, missing inputs and ambiguous variable names with spans pointing to parameters.

     * ~~Allow optional parameter attributes (e.g. #[rename = "field_name"]) for cases where variable names in functions differ from field names in the pipeline context.~~

     * Consider enforcing a canonical ordering of stage execution when multiple valid topological orders exist (lexical order of stage names) to avoid nondeterministic behaviour.

  3. _**Optional diagram feature and configurable build flags**_

     * Move the HTML/PlantUML generation behind a feature flag (e.g. diagram) so users who don’t need diagrams can disable it and avoid pulling in serde_json.

     * Provide a CLI or a cargo subcommand (cargo pipeline diagram) to generate diagrams without changing code.

  4. _**Asynchronous and parallel pipelines**_

     * Investigate supporting async fn stages. For example, if a stage returns impl Future<Output = Result<(), E>> then the generated compute() could become async fn compute(&mut self, &mut Context) -> Result<(), E> and await each stage. This would allow pipelines that perform network I/O or disk reads without blocking.

     * In the longer term, explore running independent stages concurrently (e.g. with tokio::join!) when the DAG indicates no dependencies between them.

  5. _**Change‑detection semantics**_

     * Make incremental recomputation a core concept: define a trait ChangeDetect (or similar) so users can customise how updates are detected. For example, allow floating‑point updates with tolerances or domain‑specific “is changed” logic.

     * Integrate this trait into Value and Vector so that touch() and set() can use ChangeDetect rather than always marking values updated.

  6. _**Pipeline composition and reuse**_

     * Allow pipelines themselves to act as stages in another pipeline. Generated structs could implement Reset and have compute() that runs the subgraph. This would enable hierarchical composition without repeating code.

     * Provide utilities or macros to embed a pipeline as a stage and namespace its outputs automatically, with an option to rename.

  7. _**Testing and continuous integration**_

     * Add unit tests for the derive macro to ensure it produces expected code for various stage signatures, including error propagation, context inference and cycle detection.

     * Set up GitHub Actions for cargo test, cargo clippy and rustfmt to maintain code quality.

     * Consider fuzzing the macro with tools like trybuild to ensure resilience against edge cases.