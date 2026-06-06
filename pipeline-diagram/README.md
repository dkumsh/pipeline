# pipeline-diagram

Render a small **directed dataflow graph** — described as a JSON **spec** — into
an interactive, self-contained **HTML page** (vis-network: pan/zoom, collapsible
details sidebar, fills the viewport). The whole API is one function:

```rust
let html: String = pipeline_diagram::render_html(graph_json)?; // graph_json: &str
std::fs::write("graph.html", html)?;
```

It's a **standalone, general-purpose** renderer — it knows nothing about
pipelines, and you build the JSON however you like (`serde_json`, a literal,
anything). It's also the shared diagram layer for the
[`pipeline`](https://github.com/dkumsh/pipeline) family (its primary consumer),
which is why the shape and rendering live here, in one place — but it has no
dependency on those crates.

The rendered page — title overlay, pan/zoom canvas, a slide-out controls pane,
and a footer timestamp:

![Example rendered diagram](https://raw.githubusercontent.com/dkumsh/pipeline/main/pipeline-graph/doc/telemetry_monitor.png)

## JSON shape

```json
{
  "pipeline_name": "MyPipeline",
  "metadata": { "generated_at": "2026-06-06 14:30:00" },
  "nodes": [
    { "id": "score",   "label": "score",  "group": "stage",    "full_label": "Stage: score" },
    { "id": "$leaves", "label": "leaves", "group": "variable" }
  ],
  "edges": [ { "from": "$leaves", "to": "score" } ]
}
```

- `pipeline_name` — shown in the header.
- `metadata.generated_at` — optional; free-form string shown in the footer
  (when the graph was generated). Omitted from the footer if absent.
- `nodes[].id` — unique, referenced by edges. `label` — short text. `group` —
  `"stage"` (function node) or `"variable"` (data node); drives styling.
  `full_label` — optional; shown in the details panel on click (falls back to
  `label`).
- `edges[].from` / `to` — node ids (arrowheads applied by the template).

Unknown fields are ignored, so a producer may add more without breaking an older
renderer.

## Optional: build the JSON with a helper

You can build the graph JSON any way you like, but `graph_json` saves you from
hand-writing the field names and `group` values:

```rust
use pipeline_diagram::{graph_json, Node, Edge, Group};

let json = graph_json(
    "MyPipeline",
    &[
        Node { id: "score".into(),   label: "score".into(),  group: Group::Stage,
               full_label: Some("Stage: score".into()) },
        Node { id: "$leaves".into(), label: "leaves".into(), group: Group::Variable,
               full_label: None },
    ],
    &[Edge { from: "$leaves".into(), to: "score".into() }],
    Some("2026-06-06 14:30:00"), // footer "generated at"; None to omit
);
let html = pipeline_diagram::render_html(&json)?;
```

`render_html` is the only required entry point; `graph_json` is just a
convenience both pipeline front-ends share.

## Used by the pipeline front-ends

The snippet above is standalone use — that's the baseline. The pipeline crates
are the primary consumers; each builds this JSON for you:

- **[`pipeline-dsl`](https://crates.io/crates/pipeline-dsl)** (static) at compile
  time; your pipeline type gets `html_diagram()` / `diagram_json()`.
- **[`pipeline-graph`](https://crates.io/crates/pipeline-graph)** (dynamic) at
  runtime; `Pipeline` gets `html_diagram()` / `diagram_json()` /
  `write_html_to_file(...)`.

## License

MIT OR Apache-2.0.
