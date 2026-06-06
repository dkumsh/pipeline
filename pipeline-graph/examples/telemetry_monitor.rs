//! A complete, self-contained example: a **fleet telemetry monitor** built as a
//! dynamic pipeline graph.
//!
//! Each cycle the caller commits new sensor readings for whichever machines
//! reported; the pipeline incrementally rescores health, aggregates fleet stats,
//! raises alerts, and publishes a report — recomputing only what the changed
//! readings actually affect (per-slot dirty tracking).
//!
//! ```text
//!  config(arg) ─┐                 ┌─> aggregate ─> fleet ─┐
//!  readings ────┴─> score ─> health ┤                     ├─> report ─> sink
//!  (external)                       └─> detect ───> alerts ┘
//! ```
//!
//! It exercises essentially the whole crate:
//!
//! * node kinds — `arg` (config, never reset), `external` (readings,
//!   dirty-reset each cycle), and internal outputs (`health`, `fleet`,
//!   `alerts`, `sink`);
//! * stages as **free functions** (`score`, `aggregate`, `detect`) *and* a
//!   **closure** (`report`), each taking its ports as separate arguments;
//! * fan-out (`config` and `health` feed two stages) and fan-in (`report`);
//! * **topological sort** — stages are registered leaf-first, out of dependency
//!   order, and still execute correctly;
//! * `Vector` dirty tracking — `commit` / `iter_updated_indices` /
//!   `is_updated_at` / `is_updated` / `get_valid` / `with_invalid_slots`;
//! * `Value` for scalar aggregates and the report sink;
//! * **per-cycle reset** — an idle cycle (no commits) recomputes nothing;
//! * **`Flow::Break`** — a critical fault halts the cycle before publishing;
//! * **runtime reconfiguration** — an optional EMA-smoothing stage is inserted
//!   and `aggregate`/`detect` are re-wired to read it, all decided at runtime;
//! * `dot()` — the live graph as a Graphviz diagram;
//! * **build-time validation** — a deliberately bad graph is rejected.

use pipeline::{Error, Value, Vector};
use pipeline_graph::{Flow, Graph, Input, Output, Pipeline, Slot};

// --------------------------------------------------------------------------
// Domain types
// --------------------------------------------------------------------------

/// A raw sensor sample for one machine.
#[derive(Clone, Default, Debug)]
struct Reading {
    temp_c: f64,
    load: f64,
}

/// Derived health score in `0.0..=100.0` (higher is healthier).
#[derive(Clone, Default, Debug)]
struct Health {
    score: f64,
}

/// Alert severity per machine.
#[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
enum Severity {
    #[default]
    Ok,
    Warn,
    Crit,
}

/// Fleet-wide rollup.
#[derive(Clone, Default, Debug, PartialEq)]
struct FleetStats {
    avg_health: f64,
    worst: f64,
    reporting: usize,
}

/// Pipeline configuration — the `arg` node. Read by several stages, never
/// mutated, never reset.
struct Config {
    n_machines: usize,
    warn_temp: f64,
    temp_weight: f64,
    load_weight: f64,
    warn_health: f64,
    crit_health: f64,
}

/// Terminal sink. `Reset` is a no-op so published output accumulates across
/// cycles until the operator drains it.
#[derive(Default)]
struct Report {
    events: Vec<String>,
    snapshots: Vec<FleetStats>,
}
impl pipeline::Reset for Report {
    type Error = pipeline::Error;
    fn reset(&mut self) -> Result<(), pipeline::Error> {
        Ok(())
    }
}

// --------------------------------------------------------------------------
// Stages (free functions — take their ports as separate args)
// --------------------------------------------------------------------------

/// Rescore only the machines whose reading changed this cycle.
fn score(
    cfg: &Config,
    readings: &Vector<Reading>,
    health: &mut Vector<Health>,
) -> Result<Flow, Error> {
    for i in readings.iter_updated_indices() {
        if let Some(r) = readings.get_valid(i) {
            let temp_pen = (r.temp_c - cfg.warn_temp).max(0.0) * cfg.temp_weight;
            let load_pen = r.load * cfg.load_weight;
            let s = (100.0 - temp_pen - load_pen).clamp(0.0, 100.0);
            health.commit(i, Health { score: s });
        }
    }
    Ok(Flow::Continue)
}

/// Optional smoothing: exponential moving average of health, in place. Reads
/// `health`, writes `ema`; uses `ema`'s previous value (output nodes persist
/// across cycles — only their dirty bits are reset).
fn smooth(health: &Vector<Health>, ema: &mut Vector<Health>) -> Result<Flow, Error> {
    const ALPHA: f64 = 0.5;
    for i in health.iter_updated_indices() {
        if let Some(h) = health.get_valid(i) {
            let prev = ema.get_valid(i).map(|e| e.score).unwrap_or(h.score);
            ema.commit(
                i,
                Health {
                    score: ALPHA * h.score + (1.0 - ALPHA) * prev,
                },
            );
        }
    }
    Ok(Flow::Continue)
}

/// Roll up fleet stats. Skips work entirely when nothing moved.
fn aggregate(src: &Vector<Health>, fleet: &mut Value<FleetStats>) -> Result<Flow, Error> {
    if !src.is_updated() {
        return Ok(Flow::Continue);
    }
    let (mut sum, mut n, mut worst) = (0.0, 0usize, 100.0_f64);
    for i in 0..src.len() {
        if let Some(h) = src.get_valid(i) {
            sum += h.score;
            n += 1;
            worst = worst.min(h.score);
        }
    }
    if n > 0 {
        fleet.set(FleetStats {
            avg_health: sum / n as f64,
            worst,
            reporting: n,
        });
    }
    Ok(Flow::Continue)
}

/// Classify each changed machine. A critical fault halts the cycle via
/// `Flow::Break` so `report` does not publish during an emergency.
fn detect(
    cfg: &Config,
    src: &Vector<Health>,
    alerts: &mut Vector<Severity>,
) -> Result<Flow, Error> {
    let mut critical = false;
    for i in src.iter_updated_indices() {
        if let Some(h) = src.get_valid(i) {
            let sev = if h.score < cfg.crit_health {
                Severity::Crit
            } else if h.score < cfg.warn_health {
                Severity::Warn
            } else {
                Severity::Ok
            };
            alerts.commit(i, sev);
            critical |= sev == Severity::Crit;
        }
    }
    Ok(if critical {
        Flow::Break
    } else {
        Flow::Continue
    })
}

mod telemetry_monitor {
    // --------------------------------------------------------------------------
    // Graph assembly
    // --------------------------------------------------------------------------

    use super::*;

    /// Handles the caller keeps to feed inputs and inspect outputs.
    pub struct Handles {
        pub(crate) readings: Slot<Vector<Reading>>,
        pub(crate) health: Slot<Vector<Health>>,
        pub(crate) fleet: Slot<Value<FleetStats>>,
        pub(crate) alerts: Slot<Vector<Severity>>,
        pub(crate) sink: Slot<Value<Report>>,
    }

    /// Build the monitor. `smoothing` decides — at runtime — whether an EMA stage
    /// is inserted and whether `aggregate`/`detect` read raw or smoothed health.
    pub(crate) fn build(config: Config, smoothing: bool) -> (Pipeline, Handles) {
        let n = config.n_machines;

        let mut g = Graph::named(if smoothing {
            "TelemetryMonitorSmoothing"
        } else {
            "TelemetryMonitor"
        });
        let cfg = g.arg("config", config);
        let readings = g.slot::<Vector<Reading>>("readings");
        let health = g.slot::<Vector<Health>>("health");
        let fleet = g.slot::<Value<FleetStats>>("fleet");
        let alerts = g.slot::<Vector<Severity>>("alerts");
        let sink = g.slot::<Value<Report>>("sink");
        g.external(readings);

        // Registered leaf-first / out of dependency order on purpose — `build()`
        // topologically sorts them, so execution order is derived, not declared.
        g.stage(
            "report",
            (Input(readings), Input(alerts), Input(fleet), Output(sink)),
            |readings: &Vector<Reading>,
             alerts: &Vector<Severity>,
             fleet: &Value<FleetStats>,
             sink: &mut Value<Report>| {
                let report = sink.get_mut()?;
                for i in alerts.iter_updated_indices() {
                    if let Some(sev) = alerts.get_valid(i)
                        && *sev != Severity::Ok
                    {
                        let t = readings.get_valid(i).map(|r| r.temp_c).unwrap_or(f64::NAN);
                        report
                            .events
                            .push(format!("machine {i}: {sev:?} (temp {t:.0}C)"));
                    }
                }
                if fleet.is_updated()
                    && let Some(f) = fleet.get_valid()
                {
                    report.snapshots.push(f.clone());
                }
                Ok(Flow::Continue)
            },
        );

        // `aggregate` before `detect`: when a critical fault breaks the cycle in
        // `detect`, the fleet rollup has already been produced.
        // The `ema` node only exists when smoothing is enabled — otherwise it
        // would be an unwired, orphan node in the graph/diagram.
        let mut ema_slot = None;
        let src = if smoothing {
            let ema = g.slot::<Vector<Health>>("ema");
            g.stage("smooth", (Input(health), Output(ema)), smooth);
            ema_slot = Some(ema);
            ema
        } else {
            health
        };
        g.stage("aggregate", (Input(src), Output(fleet)), aggregate);
        g.stage("detect", (Input(cfg), Input(src), Output(alerts)), detect);
        g.stage(
            "score",
            (Input(cfg), Input(readings), Output(health)),
            score,
        );

        let mut p = g.build().expect("valid graph");
        // Pre-size the dirty-tracked buffers; slots start invalid until committed.
        p.set(readings, Vector::with_invalid_slots(n));
        p.set(health, Vector::with_invalid_slots(n));
        if let Some(ema) = ema_slot {
            p.set(ema, Vector::with_invalid_slots(n));
        }
        p.set(alerts, Vector::with_invalid_slots(n));
        p.set(sink, {
            let mut v = Value::new();
            v.set(Report::default());
            v
        });
        (
            p,
            Handles {
                readings,
                health,
                fleet,
                alerts,
                sink,
            },
        )
    }

    pub(crate) fn config(n: usize) -> Config {
        Config {
            n_machines: n,
            warn_temp: 70.0,
            temp_weight: 1.0,
            load_weight: 20.0,
            warn_health: 60.0,
            crit_health: 30.0,
        }
    }

    /// `build()` rejects bad wiring up front — demonstrated here on a two-writer graph.
    pub(crate) fn demo_build_rejects_bad_wiring() {
        let mut g = Graph::new();
        let x = g.slot::<Value<u32>>("x");
        g.stage("a", (Output(x),), |v: &mut Value<u32>| {
            v.set(1);
            Ok(Flow::Continue)
        });
        g.stage("b", (Output(x),), |v: &mut Value<u32>| {
            v.set(2);
            Ok(Flow::Continue)
        });
        match g.build() {
            Err(e) => println!("build() rejected bad wiring: {e}"),
            Ok(_) => unreachable!("two writers should not validate"),
        }
    }
}

fn main() {
    std::fs::create_dir_all("target/graph").expect("create graph output dir");

    let (mut p, h) =
        telemetry_monitor::build(telemetry_monitor::config(4), /* smoothing */ false);
    println!("--- runtime graph (DOT) ---\n{}", p.dot());
    let html_path = "target/graph/telemetry_monitor.html";
    p.write_html_to_file(html_path)
        .expect("write runtime graph html");
    println!(
        "HTML diagram written to: {}",
        std::fs::canonicalize(html_path)
            .map(|p| p.display().to_string())
            .unwrap_or_else(|_| html_path.to_string())
    );

    // ---- Cycle 1: machines 0,1,2 report (machine 3 stays silent) ----------
    p.get_mut(h.readings).commit(
        0,
        Reading {
            temp_c: 65.0,
            load: 0.1,
        },
    );
    p.get_mut(h.readings).commit(
        1,
        Reading {
            temp_c: 75.0,
            load: 0.3,
        },
    );
    p.get_mut(h.readings).commit(
        2,
        Reading {
            temp_c: 95.0,
            load: 0.8,
        },
    );
    p.compute().unwrap();

    let fleet = p.get(h.fleet).get_valid().unwrap().clone();
    println!("cycle 1: fleet={fleet:?}");
    assert_eq!(fleet.reporting, 3);
    assert!((fleet.avg_health - 82.0).abs() < 1e-9); // (98 + 89 + 59)/3
    assert_eq!(*p.get(h.alerts).get_valid(2).unwrap(), Severity::Warn); // 59 < 60
    let events_after_1 = p.get(h.sink).get_valid().unwrap().events.len();
    assert_eq!(events_after_1, 1); // only machine 2 (Warn)
    println!(
        "cycle 1 events: {:?}",
        p.get(h.sink).get_valid().unwrap().events
    );

    // ---- Cycle 2: idle — no commits, so nothing recomputes ----------------
    p.compute().unwrap();
    assert_eq!(
        p.get(h.sink).get_valid().unwrap().events.len(),
        events_after_1
    );
    assert_eq!(p.get(h.sink).get_valid().unwrap().snapshots.len(), 1);
    println!("cycle 2 (idle): no new events or snapshots");

    // ---- Cycle 3: machine 0 goes critical -> detect breaks, report skipped -
    p.get_mut(h.readings).commit(
        0,
        Reading {
            temp_c: 130.0,
            load: 1.0,
        },
    );
    p.compute().unwrap();
    let fleet3 = p.get(h.fleet).get_valid().unwrap().clone();
    println!("cycle 3: fleet={fleet3:?}");
    assert_eq!(*p.get(h.alerts).get_valid(0).unwrap(), Severity::Crit); // score 20 < 30
    assert!((fleet3.avg_health - 56.0).abs() < 1e-9); // aggregate ran: (20 + 89 + 59)/3
    // report was skipped by the Break, so no new event/snapshot was published.
    assert_eq!(
        p.get(h.sink).get_valid().unwrap().events.len(),
        events_after_1
    );
    assert_eq!(p.get(h.sink).get_valid().unwrap().snapshots.len(), 1);
    println!("cycle 3: critical fault halted publishing (events still {events_after_1})");

    // ---- Runtime reconfiguration: same logic, EMA-smoothed, re-wired ------
    let (mut ps, hs) =
        telemetry_monitor::build(telemetry_monitor::config(4), /* smoothing */ true);
    println!("\n--- reconfigured with smoothing (DOT) ---\n{}", ps.dot());
    ps.write_html_to_file("target/graph/telemetry_monitor_smoothing.html")
        .expect("write smoothed runtime graph html");
    ps.get_mut(hs.readings).commit(
        0,
        Reading {
            temp_c: 95.0,
            load: 0.8,
        },
    );
    ps.compute().unwrap();
    // First smoothed sample equals the raw score (no prior EMA): 59.
    assert!((ps.get(hs.fleet).get_valid().unwrap().worst - 59.0).abs() < 1e-9);
    let _ = (hs.health, hs.alerts, hs.sink);
    println!(
        "smoothed variant ran; fleet={:?}",
        ps.get(hs.fleet).get_valid().unwrap()
    );

    telemetry_monitor::demo_build_rejects_bad_wiring();
    println!("\nOK");
}
