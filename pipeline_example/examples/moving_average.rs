//! Small data pipeline: compute a simple moving average over an input series.
//! Shows a pipeline field (`ma`) that is **written** and marked `#[skip_reset]`
//! so it does not require a `Reset` impl.
//!
//! Run: `cargo run --example moving_average`

use pipeline::{pipeline, stage};

#[derive(Default, Debug)]
struct Series {
    data: Vec<f64>,
}

#[pipeline(name = "MovingAvg", args = "window", context = "series")]
mod mav {
    use super::*;

    /// Writer for the pipeline field `ma`. We mark it `#[skip_reset]` so
    /// the pipeline doesn't try to call `Reset` on `Vec<f64>`.
    #[stage]
    pub fn moving_average(#[skip_reset] ma: &mut Vec<f64>, series: &Series, window: &usize) {
        ma.clear(); // keep semantics deterministic even without `reset()`
        let w = *window;
        if w == 0 || series.data.len() < w {
            return;
        }
        for i in 0..=series.data.len() - w {
            let slice = &series.data[i..i + w];
            let sum: f64 = slice.iter().copied().sum();
            ma.push(sum / w as f64);
        }
    }

    #[stage]
    pub fn print(ma: &Vec<f64>) {
        println!("[moving_avg] {:?}", ma);
    }
}

fn main() {
    let mut p = MovingAvg::new(3);
    let series = Series {
        data: vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0],
    };
    p.compute(&series).unwrap();
}
