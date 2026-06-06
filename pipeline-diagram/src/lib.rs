//! Render a small directed dataflow graph — described as a JSON **spec** — into
//! a standalone, interactive HTML page (vis-network: pan/zoom, collapsible
//! details sidebar, fills the viewport).
//!
//! The whole API is [`render_html`]: hand it a graph JSON, get back an HTML
//! document. Callers build the JSON however they like (`serde_json`, `json!`, a
//! literal); this crate only renders it. It knows nothing about pipelines — any
//! graph in the shape below works — and it's the shared diagram layer for the
//! [`pipeline`](https://github.com/dkumsh/pipeline) family.
//!
//! # JSON shape
//!
//! ```json
//! {
//!   "pipeline_name": "MyPipeline",
//!   "nodes": [
//!     { "id": "score",   "label": "score",  "group": "stage",    "full_label": "Stage: score" },
//!     { "id": "$leaves", "label": "leaves", "group": "variable" }
//!   ],
//!   "edges": [ { "from": "$leaves", "to": "score" } ]
//! }
//! ```
//!
//! - `pipeline_name` — shown in the diagram header.
//! - `nodes[].id` — unique, referenced by edges. `label` — short display text.
//!   `group` — `"stage"` (function node) or `"variable"` (data node); drives
//!   styling. `full_label` — optional; shown in the details panel on click
//!   (falls back to `label`).
//! - `edges[].from` / `to` — node ids (arrowheads are applied by the template).
//!
//! Unknown fields are ignored, so a producer may add more without breaking an
//! older renderer.
#![warn(missing_docs)]

use serde_json::{Map, Value, json};

/// The HTML page template. `{{PIPELINE_NAME}}`, `{{NODES_JSON}}` and
/// `{{EDGES_JSON}}` are substituted at render time.
const TEMPLATE: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/assets/diagram.html"));

/// Node kind, serialized as the `group` field (`"stage"` / `"variable"`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Group {
    /// A function node (a stage).
    Stage,
    /// A data node (a value / slot).
    Variable,
}

impl Group {
    fn as_str(self) -> &'static str {
        match self {
            Group::Stage => "stage",
            Group::Variable => "variable",
        }
    }
}

/// A node, as primitive parts — input to the optional [`graph_json`] helper.
pub struct Node {
    /// Unique node id (referenced by [`Edge`] endpoints).
    pub id: String,
    /// Short display label.
    pub label: String,
    /// Stage or data node.
    pub group: Group,
    /// Optional details-panel label (falls back to `label`).
    pub full_label: Option<String>,
}

/// A directed edge between node ids — input to the optional [`graph_json`] helper.
pub struct Edge {
    /// Source node id.
    pub from: String,
    /// Destination node id.
    pub to: String,
}

/// Optional convenience: assemble a graph JSON (the shape [`render_html`] expects)
/// from typed parts, so producers don't hand-write the field names / `group`
/// values. Building the JSON yourself and calling [`render_html`] works equally
/// well — this is just a shared helper.
pub fn graph_json(pipeline_name: &str, nodes: &[Node], edges: &[Edge]) -> String {
    let nodes: Vec<Value> = nodes
        .iter()
        .map(|n| {
            let mut m = Map::new();
            m.insert("id".into(), json!(n.id));
            m.insert("label".into(), json!(n.label));
            m.insert("group".into(), json!(n.group.as_str()));
            if let Some(full) = &n.full_label {
                m.insert("full_label".into(), json!(full));
            }
            Value::Object(m)
        })
        .collect();
    let edges: Vec<Value> = edges
        .iter()
        .map(|e| json!({ "from": e.from, "to": e.to }))
        .collect();
    json!({ "pipeline_name": pipeline_name, "nodes": nodes, "edges": edges }).to_string()
}

/// Render a graph (see the crate-level docs for the JSON shape) into a
/// self-contained interactive HTML page.
///
/// Errors only if `graph_json` is not valid JSON. Missing `pipeline_name` /
/// `nodes` / `edges` are treated as empty.
pub fn render_html(graph_json: &str) -> Result<String, serde_json::Error> {
    let spec: Value = serde_json::from_str(graph_json)?;
    let name = spec
        .get("pipeline_name")
        .and_then(Value::as_str)
        .unwrap_or("");
    let nodes = serde_json::to_string(spec.get("nodes").unwrap_or(&Value::Array(Vec::new())))?;
    let edges = serde_json::to_string(spec.get("edges").unwrap_or(&Value::Array(Vec::new())))?;
    Ok(TEMPLATE
        .replace("{{PIPELINE_NAME}}", name)
        .replace("{{NODES_JSON}}", &nodes)
        .replace("{{EDGES_JSON}}", &edges))
}

#[cfg(test)]
mod tests {
    use super::*;

    const SPEC: &str = r#"{
        "pipeline_name": "Demo",
        "nodes": [ { "id": "s0", "label": "copy", "group": "stage", "full_label": "Stage: copy" } ],
        "edges": [ { "from": "n0", "to": "s0" } ]
    }"#;

    #[test]
    fn renders_name_data_and_template_markers() {
        let html = render_html(SPEC).unwrap();
        assert!(html.contains("Pipeline: Demo"));
        assert!(html.contains("\"label\":\"copy\""));
        assert!(html.contains("network.setSize('100%', '100%')"));
        assert!(html.contains("toggleSidebar"));
    }

    /// Producer/consumer contract: the field the template reads for the details
    /// panel must match the documented spec field (`full_label`).
    #[test]
    fn template_reads_documented_full_label_field() {
        assert!(
            TEMPLATE.contains(".full_label"),
            "template must read the documented `full_label` node field"
        );
    }

    #[test]
    fn invalid_json_errors() {
        assert!(render_html("not json").is_err());
    }

    #[test]
    fn missing_sections_default_to_empty() {
        let html = render_html(r#"{ "pipeline_name": "Empty" }"#).unwrap();
        assert!(html.contains("Pipeline: Empty"));
        assert!(html.contains("new vis.DataSet([])"));
    }
}
