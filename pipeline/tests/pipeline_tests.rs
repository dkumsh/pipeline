//! Integration tests for the pipeline and pipeline_derive crates.
//!
//! These tests exercise the main features of the pipeline system:
//! - Topological ordering of stages and correct value propagation.
//! - Support for optional rename attributes on stage parameters.
//! - Passing a mutable context parameter between stages.
//! - Proper error propagation when a stage returns an error.

use pipeline::pipeline;
use pipeline::value::Vector;

use std::cell::RefCell;

// Capture the order in which stages are executed.  We use a thread‑local
// `RefCell<Vec<&'static str>>` so that each test has its own call stack.
thread_local! {
    static CALLS: RefCell<Vec<&'static str>> = const { RefCell::new(Vec::new()) };
}

/// A simple error type used in these tests.  It converts from the base
/// `pipeline::Error` and provides a custom variant for demonstration.
#[derive(Debug, PartialEq)]
enum TestErr {
    /// Error propagated from the pipeline runtime (e.g. uninitialised value).
    Pipeline(pipeline::Error),
    /// A custom error to simulate a failure in a stage function.
    Custom,
}

impl From<pipeline::Error> for TestErr {
    fn from(e: pipeline::Error) -> Self {
        TestErr::Pipeline(e)
    }
}

// === Test 1: rename attribute ===
// Define a pipeline where a parameter is renamed via `#[rename = "my_field"]`.
// The first stage initialises the field; the second stage exists to exercise
// reading the renamed field.
#[pipeline(name = "RenamePipeline", error = "TestErr")]
mod rename_mod {
    use super::TestErr;
    // no explicit imports needed in this module; macros are resolved via `pipeline` re-export

    // Stage initialising the renamed field.
    #[pipeline::stage]
    pub fn init(
        #[rename = "my_field"]
        #[skip_clear]
        val: &mut u32,
    ) -> Result<(), TestErr> {
        *val = 42;
        Ok(())
    }

    // Stage reading the renamed field.  We don’t perform any assertion inside
    // the stage; the test will verify the value after compute() returns.
    #[pipeline::stage]
    pub fn read(my_field: &u32) -> Result<(), TestErr> {
        let _ = *my_field;
        Ok(())
    }
}

#[test]
fn test_rename_attribute() {
    // Create the pipeline with no constructor arguments.
    // The generated pipeline struct lives at the crate root, not inside the module.
    let mut p = RenamePipeline::new();
    p.compute().expect("pipeline should run without error");
    // The renamed field should have been initialised to 42 by the `init` stage.
    assert_eq!(p.my_field, 42);
}

// === Test 2: execution order and value propagation ===
// This pipeline consists of three stages chained in sequence.  Each stage
// records its name in the global CALLS vector and computes values based on
// the previous stage’s output.  We verify both the call order and the final
// computed values.
#[pipeline(name = "OrderPipeline", error = "TestErr")]
mod order_mod {
    use super::{CALLS, TestErr};
    // no explicit imports needed in this module; macros are resolved via `pipeline` re-export

    #[pipeline::stage]
    pub fn first(#[skip_clear] x: &mut u32) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("first"));
        *x = 1;
        Ok(())
    }

    #[pipeline::stage]
    pub fn second(x: &u32, #[skip_clear] y: &mut u32) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("second"));
        *y = *x * 2;
        Ok(())
    }

    #[pipeline::stage]
    pub fn third(
        y: &u32,
        #[unused]
        #[skip_clear]
        z: &mut u32,
    ) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("third"));
        *z = *y + 1;
        Ok(())
    }
}

#[test]
fn test_order_and_values() {
    // Clear the call log before running the pipeline.
    CALLS.with(|c| c.borrow_mut().clear());
    let mut p = OrderPipeline::new();
    p.compute().expect("pipeline should run without error");
    // Verify that the stages were executed in topological order.
    CALLS.with(|c| {
        let calls = c.borrow().clone();
        assert_eq!(calls.as_slice(), &["first", "second", "third"]);
    });
    // The pipeline should contain the computed values: x=1, y=2, z=3.
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
    assert_eq!(p.z, 3);
}

// === Test 3: context parameter ===
// This pipeline demonstrates passing a mutable context between stages.
// Each stage appends a message to the context.  No pipeline fields are used.
#[pipeline(name = "ContextPipeline", context = "ctx", error = "TestErr")]
mod ctx_mod {
    use super::TestErr;
    // no explicit imports needed in this module; macros are resolved via `pipeline` re-export

    #[pipeline::stage]
    pub fn stage1(ctx: &mut String) -> Result<(), TestErr> {
        ctx.push_str("stage1");
        Ok(())
    }

    #[pipeline::stage]
    pub fn stage2(ctx: &mut String) -> Result<(), TestErr> {
        ctx.push_str(":stage2");
        Ok(())
    }
}

#[test]
fn test_context_param() {
    let mut ctx = String::new();
    let mut p = ContextPipeline::new();
    p.compute(&mut ctx)
        .expect("pipeline should run without error");
    // The context should have been modified by both stages.
    assert_eq!(ctx, "stage1:stage2");
}

// === Test 4: error propagation ===
// This pipeline contains a stage that deliberately returns an error.  We
// verify that `compute()` returns that error and does not panic.
#[pipeline(name = "ErrorPipeline", error = "TestErr")]
mod err_mod {
    use super::TestErr;
    // no explicit imports needed in this module; macros are resolved via `pipeline` re-export

    #[pipeline::stage]
    pub fn fail(
        #[unused]
        #[skip_clear]
        _x: &mut u32,
    ) -> Result<(), TestErr> {
        Err(TestErr::Custom)
    }
}

#[test]
fn test_error_propagation() {
    let mut p = ErrorPipeline::new();
    let res = p.compute();
    // The pipeline should return our custom error.
    assert!(matches!(res, Err(TestErr::Custom)));
}

// === Test 5: complex dependency and topological order ===
// This pipeline has three stages: `a` and `b` produce independent values `x` and `y`.
// A third stage `c` consumes both and produces `z = x + y`.  The expected call order
// respects the definition order `a`, `b`, `c` because `a` and `b` are independent.
#[pipeline(name = "ComplexPipeline", error = "TestErr")]
mod complex_mod {
    use super::{CALLS, TestErr};

    #[pipeline::stage]
    pub fn a(#[skip_clear] x: &mut u32) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("a"));
        *x = 1;
        Ok(())
    }

    #[pipeline::stage]
    pub fn b(#[skip_clear] y: &mut u32) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("b"));
        *y = 10;
        Ok(())
    }

    #[pipeline::stage]
    pub fn c(
        x: &u32,
        y: &u32,
        #[unused]
        #[skip_clear]
        z: &mut u32,
    ) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("c"));
        *z = *x + *y;
        Ok(())
    }
}

#[test]
fn test_complex_order() {
    // Clear call log and run the pipeline
    CALLS.with(|c| c.borrow_mut().clear());
    let mut p = ComplexPipeline::new();
    p.compute().expect("pipeline should run without error");
    // Expect definition order because a and b are independent
    CALLS.with(|c| {
        let calls = c.borrow().clone();
        assert_eq!(calls.as_slice(), &["a", "b", "c"]);
    });
    // Check computed values
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 10);
    assert_eq!(p.z, 11);
}

// === Test 6: skip_clear behaviour with Vector ===
// This pipeline demonstrates that `#[skip_clear]` allows a counter to accumulate across runs
// while a `Vector` of values is reset by the pipeline.  The first stage pushes the current
// count into a vector, then increments it.  We call `compute()` twice and verify that
// the counter increments while the vector does not accumulate old values.
#[pipeline(name = "MixedPipeline", error = "TestErr")]
mod mixed_mod {
    use super::TestErr;
    use pipeline::value::Vector;

    #[pipeline::stage]
    pub fn generate(
        #[unused]
        #[skip_clear]
        count: &mut u32,
        #[unused] data: &mut Vector<u32>,
    ) -> Result<(), TestErr> {
        // push the current count then increment it
        data.push(*count);
        *count += 1;
        Ok(())
    }

    #[pipeline::stage]
    pub fn read(#[unused] _data: &Vector<u32>) -> Result<(), TestErr> {
        // do nothing; ensures the pipeline reads the vector
        Ok(())
    }
}

#[test]
fn test_skip_clear_behavior() {
    let mut p = MixedPipeline::new();
    // First run: count starts at 0, becomes 1.  The vector receives one element (0).
    p.compute().expect("first compute should succeed");
    assert_eq!(p.count, 1);
    assert_eq!(p.data.len(), 1);
    assert_eq!(p.data.get(0).unwrap(), &0_u32);
    // Second run: count starts at 1, becomes 2.  The vector now contains the previous
    // value 0 and the new value 1; it is not cleared between runs.
    p.compute().expect("second compute should succeed");
    assert_eq!(p.count, 2);
    assert_eq!(p.data.len(), 2);
    assert_eq!(p.data.get(0).unwrap(), &0_u32);
    assert_eq!(p.data.get(1).unwrap(), &1_u32);
}

// === Test 7: multi‑producer order and values ===
// This pipeline tests topological ordering with multiple producers of independent fields.
// Stage `produce_xy` writes two independent fields x and y; stage `produce_z` writes z;
// stage `consume_yz` reads y and z and writes w.  The pipeline should call stages
// in the definition order because `produce_xy` and `produce_z` are independent.
#[pipeline(name = "MultiProducerPipeline", error = "TestErr")]
mod mp_mod {
    use super::{CALLS, TestErr};

    #[pipeline::stage]
    pub fn produce_xy(
        #[unused]
        #[skip_clear]
        x: &mut u32,
        #[skip_clear] y: &mut u32,
    ) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("produce_xy"));
        *x = 2;
        *y = 3;
        Ok(())
    }

    #[pipeline::stage]
    pub fn produce_z(#[skip_clear] z: &mut u32) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("produce_z"));
        *z = 5;
        Ok(())
    }

    #[pipeline::stage]
    pub fn consume_yz(
        y: &u32,
        z: &u32,
        #[unused]
        #[skip_clear]
        w: &mut u32,
    ) -> Result<(), TestErr> {
        CALLS.with(|c| c.borrow_mut().push("consume_yz"));
        *w = *y + *z;
        Ok(())
    }
}

#[test]
fn test_multi_producer() {
    // Clear call log and run the pipeline
    CALLS.with(|c| c.borrow_mut().clear());
    let mut p = MultiProducerPipeline::new();
    p.compute().expect("pipeline should run without error");
    // Expect definition order because produce_xy and produce_z are independent
    CALLS.with(|c| {
        let calls = c.borrow().clone();
        assert_eq!(calls.as_slice(), &["produce_xy", "produce_z", "consume_yz"]);
    });
    // Check computed values
    assert_eq!(p.x, 2);
    assert_eq!(p.y, 3);
    assert_eq!(p.z, 5);
    assert_eq!(p.w, 8);
}

// === Test 8: multiple rename attributes and custom field names ===
// This pipeline demonstrates renaming multiple parameters to distinct field names.
// Stage `init_fields` writes fields x and y via differently named parameters.
// Stage `sum_fields` reads x and y and writes the result to a renamed field sum.
#[pipeline(name = "RenameMultiPipeline", error = "TestErr")]
mod ren_multi_mod {
    use super::TestErr;

    #[pipeline::stage]
    pub fn init_fields(
        #[rename = "x"]
        #[skip_clear]
        x_alias: &mut u32,
        #[rename = "y"]
        #[skip_clear]
        y_alias: &mut u32,
    ) -> Result<(), TestErr> {
        *x_alias = 4;
        *y_alias = 5;
        Ok(())
    }

    #[pipeline::stage]
    pub fn sum_fields(
        x: &u32,
        y: &u32,
        #[unused]
        #[rename = "sum"]
        #[skip_clear]
        s_alias: &mut u32,
    ) -> Result<(), TestErr> {
        *s_alias = *x + *y;
        Ok(())
    }
}

#[test]
fn test_multiple_rename() {
    let mut p = RenameMultiPipeline::new();
    p.compute().expect("pipeline should run without error");
    assert_eq!(p.x, 4);
    assert_eq!(p.y, 5);
    assert_eq!(p.sum, 9);
}
