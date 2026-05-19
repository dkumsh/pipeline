//! Demonstrates per-slot validity tracking on `Vector<V>` (v0.2.0).
//!
//! A two-stage pipeline:
//!
//! 1. `ingest` writes raw integers into `inputs`, marking some slots
//!    invalid (e.g. "this reading was rejected upstream").
//! 2. `doubler` reads `inputs` via `iter_updated_valid()` — it sees
//!    `Some(&v)` for valid slots and `None` for invalid ones. It
//!    commits a doubled value when the input is valid and
//!    invalidates the corresponding output slot otherwise, so the
//!    downstream consumer cannot mistake a held-stale value for a
//!    fresh one.
//!
//! Run: `cargo run --example validity`
//!
//! Expected output (one line per cycle):
//!   cycle 1 -> [Some(2), None, Some(8)]
//!   cycle 2 -> [Some(20), Some(40), None]

use pipeline::value::vector::Vector;
use pipeline::{pipeline, stage};

#[derive(Default)]
struct Tick {
    cycle: u32,
}

#[pipeline(name = "ValidityDemo", context = "tick")]
mod demo {
    use super::*;

    /// Cycle 1: commit slots 0 and 2 with raw readings, invalidate
    /// slot 1 (e.g. a sensor that dropped out). Cycle 2: rotate —
    /// commit 0 and 1, invalidate 2.
    #[stage]
    pub fn ingest(tick: &Tick, inputs: &mut Vector<i32>) {
        // First tick: size the vector. push() leaves slots invalid.
        if inputs.is_empty() {
            inputs.push(0);
            inputs.push(0);
            inputs.push(0);
        }
        match tick.cycle {
            1 => {
                inputs.commit(0, 1);
                inputs.invalidate(1);
                inputs.commit(2, 4);
            }
            2 => {
                inputs.commit(0, 10);
                inputs.commit(1, 20);
                inputs.invalidate(2);
            }
            _ => {}
        }
    }

    /// Reads inputs via the validity-aware iterator and routes the
    /// decision to either `commit` or `invalidate` on the output,
    /// using a `SlotWriter` so "forgot to choose" is a compile-time
    /// `#[must_use]` warning and a debug-build assertion.
    #[stage]
    pub fn doubler(inputs: &Vector<i32>, outputs: &mut Vector<i32>) {
        // Size outputs once.
        while outputs.len() < inputs.len() {
            outputs.push(0);
        }
        for (i, input_opt) in inputs.iter_updated_valid() {
            let writer = outputs.slot_writer(i);
            match input_opt {
                Some(v) => writer.commit(v * 2),
                None => writer.invalidate(),
            }
        }
    }

    #[stage]
    pub fn print(tick: &Tick, outputs: &Vector<i32>) {
        let row: Vec<Option<i32>> = (0..outputs.len())
            .map(|i| outputs.get_valid(i).copied())
            .collect();
        println!("cycle {} -> {:?}", tick.cycle, row);
    }
}

fn main() {
    let mut p = ValidityDemo::new();
    let tick = Tick { cycle: 1 };
    p.compute(&tick).unwrap();
    p.reset_all().unwrap();

    let tick = Tick { cycle: 2 };
    p.compute(&tick).unwrap();
}
