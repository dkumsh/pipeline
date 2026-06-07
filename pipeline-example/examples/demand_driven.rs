//! Demand-driven scheduling + per-stage stats in the static (`#[pipeline]`)
//! front-end.
//!
//! A sensor feeds `reading` only occasionally. `normalize` is marked
//! `#[stage(skip_when_clean)]`, so the pipeline **skips it on cycles where the
//! reading didn't change** — it does work only when there's fresh input. A
//! skipped stage doesn't write `scaled`, so `scaled` holds its last value and
//! `report` (a plain stage, runs every cycle) keeps republishing it. With the
//! `#[pipeline(stats)]` flag we record per-stage run/skip counts and print them.
//!
//! Run: `cargo run --example demand_driven`

use pipeline_dsl::{Value, pipeline};

#[pipeline(name = "Monitor", external = "reading", stats)]
mod monitor {
    use super::*;
    use pipeline_dsl::stage;

    /// Recompute the scaled value — only when `reading` changed this cycle.
    #[stage(skip_when_clean)]
    pub fn normalize(reading: &Value<f64>, scaled: &mut Value<f64>) {
        let x = *reading.get_valid().expect("dirty => valid");
        scaled.set(x / 100.0);
    }

    /// Always runs: republishes the latest scaled value (held across the cycles
    /// where `normalize` was skipped).
    #[stage]
    pub fn report(scaled: &Value<f64>, #[unused] out: &mut Value<f64>) {
        out.set(scaled.get_valid().copied().unwrap_or(0.0));
    }
}

fn main() {
    let mut p = Monitor::new();
    p.collect_stats(true);
    p.reset_stats();

    const CYCLES: usize = 12;
    for cycle in 0..CYCLES {
        // A fresh reading arrives only every 3rd cycle.
        if cycle % 3 == 0 {
            p.reading.set(cycle as f64 + 1.0);
        }
        p.compute().unwrap();
    }

    println!("latest output: {:?}", p.out.get_valid());
    println!("after {CYCLES} cycles (reading fed every 3rd):");
    let age = p.stats_age().expect("window started at reset_stats()");
    for s in p.stats() {
        let rate = s.ran as f64 / age.as_secs_f64();
        println!(
            "  {:<10} ran={:>2} skipped={:>2}  ({:>8.0} runs/s, {:?})",
            s.name, s.ran, s.skipped, rate, s.time
        );
    }
    // Expect: normalize ran=4 skipped=8 (worked only on fed cycles),
    //         report    ran=12 skipped=0 (always runs).
}
