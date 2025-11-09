//! Demonstrates pipeline-level generics used inside **context types** and
//! a constructor arg (`seed`) used as a read-only pipeline field.
//!
//! Run: `cargo run --example generics_contexts`

use pipeline::{pipeline, stage};

#[derive(Default, Debug)]
struct Db<T> {
    values: Vec<T>,
}

#[derive(Default, Debug)]
struct Cache<T> {
    total: T,
}

#[pipeline(
    name = "GenericApp",
    generics = "<T: Copy + Default + core::ops::AddAssign>",
    context = "db, cache",
    args = "seed"
)]
mod generic_app {
    use super::*;

    #[stage]
    pub fn push_seed<T: Copy>(db: &mut Db<T>, seed: &T) {
        db.values.push(*seed);
    }

    #[stage]
    pub fn accumulate<T: Copy + core::ops::AddAssign>(cache: &mut Cache<T>, db: &Db<T>) {
        if let Some(&last) = db.values.last() {
            cache.total += last;
        }
    }
}

fn main() {
    let mut db = Db::<u32>::default();
    let mut cache = Cache::<u32>::default();
    let mut app = GenericApp::<u32>::new(5);
    app.compute(&mut db, &mut cache).unwrap();
    assert_eq!(cache.total, 5);
    println!("[generics] total in cache = {}", cache.total);
}
