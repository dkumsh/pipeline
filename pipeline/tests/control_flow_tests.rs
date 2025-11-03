//! ControlFlow integration tests for the #[pipeline] macro.
use pipeline::pipeline;
use std::ops::ControlFlow;

/// A tiny helper we can mutate and then detect whether `clear_updated_all()`
/// was called after a run (or upon early `Break` when clear_updated_on_break = true).
#[derive(Default, Debug, Clone, Copy)]
struct Flag {
    // Anything you mutate makes it an output in the pipeline graph.
    ticks: u32,
    // We flip this inside `ClearUpdated::clear_updated`.
    cleared: bool,
}

// Implement the pipeline crate’s clearing trait so the macro-generated code
// can call `ClearUpdated::clear_updated(&mut Flag)`.
impl pipeline::ClearUpdated for Flag {
    type Error = pipeline::Error;
    fn clear_updated(&mut self) -> Result<(), Self::Error> {
        self.cleared = true;
        Ok(())
    }
}

// ---------- Test 1: Early Break with Result<ControlFlow<..>> and clear_updated_on_break = true ----------

#[derive(Debug)]
enum MyBreak {
    ConvergedAt(u32),
}

#[derive(thiserror::Error, Debug)]
enum AppError {
    #[error(transparent)]
    Pipe(#[from] pipeline::Error),
}

#[pipeline(
    name = "ConvergePipe",
    error = "AppError",
    controlflow_break = "MyBreak",
    clear_updated_on_break = "true"
)]
mod m1 {
    use super::{AppError, Flag, MyBreak};
    use std::ops::ControlFlow::{self, Break, Continue};

    // This stage increments and requests an early Break when limit is hit.
    // Note the Result<ControlFlow<..>> ok-type—this exercises that branch.
    #[pipeline::stage]
    pub fn step(
        #[unused] counter: &mut Flag,
        #[unused] limit: &u32,
    ) -> Result<ControlFlow<MyBreak, ()>, AppError> {
        counter.ticks += 1;
        if counter.ticks >= *limit {
            return Ok(Break(MyBreak::ConvergedAt(counter.ticks)));
        }
        Ok(Continue(()))
    }
}

#[test]
fn controlflow_break_returns_break_and_clears_when_reset_on_break_true() -> Result<(), AppError> {
    let mut p = ConvergePipe::new();
    // Make the variables visible on the pipeline struct:
    p.counter = Flag::default();
    p.limit = 3;

    // With controlflow_break present, compute() returns Result<ControlFlow<_, _>, _>.
    // Drive until Break (or cap).
    const CAP: usize = 16;
    for _ in 0..CAP {
        match p.compute()? {
            ControlFlow::Continue(()) => continue,
            ControlFlow::Break(MyBreak::ConvergedAt(n)) => {
                assert_eq!(n, 3, "should break exactly at the limit");
                // clear_updated_on_break="true" → clear_updated_all() was called by macro
                assert!(p.counter.cleared, "clear_updated_all() must run on Break");
                return Ok(());
            }
        }
    }
    panic!("expected early Break within {} iterations", CAP);
}

// ---------- Test 2: Continue path runs to completion (no Break observed) ----------

#[pipeline(
    name = "RunToEndPipe",
    error = "AppError",
    controlflow_break = "MyBreak",   // break type declared…
    clear_updated_on_break = "true"   // …but stage never returns Break
)]
mod m2 {
    use super::{AppError, Flag, MyBreak};
    use std::ops::ControlFlow::{self, Continue};

    #[pipeline::stage]
    pub fn step(
        #[unused] counter: &mut Flag,
        #[unused] limit: &u32,
    ) -> Result<ControlFlow<MyBreak, ()>, AppError> {
        if counter.ticks + 1 < *limit {
            counter.ticks += 1;
            return Ok(Continue(()));
        }
        // One last tick then stop continuing; still not a Break.
        counter.ticks = *limit;
        Ok(Continue(()))
    }
}

#[test]
fn controlflow_continue_completes_and_clears_at_end() -> Result<(), AppError> {
    let mut p = RunToEndPipe::new();
    p.counter = Flag::default();
    p.limit = 5;

    // Drive until the internal state reaches the limit, expecting only Continue.
    const CAP: usize = 32;
    for _ in 0..CAP {
        match p.compute()? {
            ControlFlow::Continue(()) => {
                if p.counter.ticks >= p.limit {
                    // On the *last* run (still Continue), the macro clears flags.
                    assert_eq!(p.counter.ticks, 5);
                    assert!(p.counter.cleared, "completed run must clear flags");
                    return Ok(());
                }
            }
            ControlFlow::Break(_) => panic!("did not expect Break in RunToEndPipe"),
        }
    }
    panic!("did not reach limit {} within {} iterations", p.limit, CAP);
}

// ---------- Test 3: clear_updated_on_break = false keeps flags uncleared on early Break ----------

#[pipeline(
    name = "NoClearOnBreakPipe",
    error = "AppError",
    controlflow_break = "MyBreak",
    clear_updated_on_break = "false"
)]
mod m3 {
    use super::{AppError, Flag, MyBreak};
    use std::ops::ControlFlow::{self, Break, Continue};

    #[pipeline::stage]
    pub fn step(#[unused] counter: &mut Flag) -> Result<ControlFlow<MyBreak, ()>, AppError> {
        counter.ticks += 1;
        if counter.ticks == 1 {
            // Break immediately.
            return Ok(Break(MyBreak::ConvergedAt(1)));
        }
        Ok(Continue(()))
    }
}

#[test]
fn controlflow_break_does_not_clear_when_reset_on_break_false() -> Result<(), AppError> {
    let mut p = NoClearOnBreakPipe::new();
    p.counter = Flag::default();

    // First compute should Break immediately; ensure no clearing happens.
    match p.compute()? {
        ControlFlow::Break(MyBreak::ConvergedAt(1)) => {
            assert!(
                !p.counter.cleared,
                "flags must remain uncleared on Break when clear_updated_on_break=false"
            );
        }
        _ => panic!("expected Break(MyBreak::ConvergedAt(1))"),
    }
    Ok(())
}

// ---------- Test 4: Pipeline without `controlflow_break = ...` returns Result<(), E> ----------

#[pipeline(name = "PlainPipe", error = "AppError")]
mod m4 {
    use super::{AppError, Flag};

    // No ControlFlow here; classic Result<(), _> stage.
    #[pipeline::stage]
    pub fn step(#[unused] counter: &mut Flag) -> Result<(), AppError> {
        counter.ticks += 1;
        Ok(())
    }
}

#[test]
fn no_break_attribute_yields_plain_result_unit() -> Result<(), AppError> {
    let mut p = PlainPipe::new();
    p.counter = Flag::default();

    // Type should be Result<(), AppError>.
    let r: Result<(), AppError> = p.compute();
    r?;
    assert_eq!(p.counter.ticks, 1);
    assert!(p.counter.cleared, "completed run must clear flags");
    Ok(())
}
