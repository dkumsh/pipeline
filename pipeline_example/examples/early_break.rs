//! Early exit via `ControlFlow::Break`, showing `controlflow_break` and the
//! dependency between a writer (`tick`) and a reader (`stop_if_reached`).
//!
//! Run: `cargo run --example early_break`

use pipeline::{pipeline, stage};

#[derive(Default, Debug)]
struct State {
    count: u32,
}

#[derive(Default, Debug)]
struct Env {
    limit: u32,
}

#[derive(Debug)]
struct Stop;

#[pipeline(
    name = "Breaker",
    context = "state, env",
    controlflow_break = "Stop",
    reset_on_break = "true"
)]
mod breaker {
    use super::*;

    #[stage]
    pub fn tick(state: &mut State) {
        state.count += 1;
    }

    #[stage]
    pub fn stop_if_reached(state: &State, env: &Env) -> std::ops::ControlFlow<Stop> {
        if state.count >= env.limit {
            std::ops::ControlFlow::Break(Stop)
        } else {
            std::ops::ControlFlow::Continue(())
        }
    }
}

fn main() {
    let mut state = State::default();
    let env = Env { limit: 1 };
    let mut p = Breaker::new();
    match p.compute(&mut state, &env).unwrap() {
        std::ops::ControlFlow::Continue(()) => {
            println!("[breaker] continued: {}", state.count)
        }
        std::ops::ControlFlow::Break(_) => {
            println!("[breaker] stopped at {}", state.count)
        }
    }
}
