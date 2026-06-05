// Define a custom Error type for simplicity
#[derive(Debug)]
pub struct Error;

// Define the Reset trait
pub trait Reset {
    fn reset(&mut self) -> Result<(), Error>;
}

// Define the MaybeReset trait without a default method
pub trait MaybeReset {
    fn maybe_reset(&mut self);
}

// Implement MaybeReset for types that implement Reset
impl<T: Reset> MaybeReset for T {
    fn maybe_reset(&mut self) {
        let _ = self.reset(); // Handle the Result as needed
    }
}

// Implement MaybeReset for String
impl MaybeReset for String {
    fn maybe_reset(&mut self) {
        // Default behavior: do nothing
    }
}

// Generic function that calls `maybe_reset`
pub fn reset<T: MaybeReset>(v: &mut T) {
    v.maybe_reset();
}

// Example struct that implements Reset
struct MyStruct;

impl Reset for MyStruct {
    fn reset(&mut self) -> Result<(), Error> {
        println!("MyStruct has been reset!");
        Ok(())
    }
}

// Now, we use String instead of AnotherStruct

fn main() {
    let mut a = MyStruct;

    // Calls the reset method on MyStruct
    reset(&mut a); // Output: "MyStruct has been reset!"

    let mut b = String::from("Hello, world!");

    // Does nothing for String
    reset(&mut b); // No output
}
