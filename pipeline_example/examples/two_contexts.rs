//! Multiple contexts + mutability escalation:
//! - `load` writes to `db` (so `compute` needs `&mut Db`)
//! - `measure` reads `db` and writes `metrics`
//!
//! Run: `cargo run --example two_contexts`

use pipeline::{pipeline, stage};

#[derive(Default, Debug)]
struct Db {
    items: Vec<i32>,
}

#[derive(Default, Debug)]
struct Metrics {
    count: usize,
}

#[pipeline(name = "TwoCtx", context = "db, metrics")]
mod two_ctx {
    use super::*;

    #[stage]
    pub fn load(db: &mut Db) {
        db.items.extend_from_slice(&[1, 2, 3]);
    }

    #[stage]
    pub fn measure(metrics: &mut Metrics, db: &Db) {
        metrics.count = db.items.len();
    }
}

fn main() {
    let mut db = Db::default();
    let mut metrics = Metrics::default();
    let mut p = TwoCtx::new();
    p.compute(&mut db, &mut metrics).unwrap();
    assert_eq!(metrics.count, 3);
    println!(
        "[two_ctx] items={}, count={}",
        db.items.len(),
        metrics.count
    );
}
