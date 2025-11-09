#![allow(dead_code)]

use pipeline::{pipeline, stage};

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Db<T> {
    count: T,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
struct Cache<T> {
    total: T,
}

#[pipeline(
    name = "App",
    generics = "<T: Copy + Default + core::ops::AddAssign + From<u8>, const N: usize>",
    context = "db, cache"
)]
mod app {
    use super::*;

    #[stage]
    pub fn tick<T: Copy + Default + core::ops::AddAssign + From<u8>>(db: &mut Db<T>) {
        db.count += T::from(1);
    }

    #[stage]
    pub fn sum<T: Copy + core::ops::AddAssign>(cache: &mut Cache<T>, db: &Db<T>) {
        cache.total += db.count;
    }

    // To show const generics are permitted in types, one could later use `N` inside stage types;
    // we don't need it here for the test to be meaningful.
}

#[test]
fn generic_contexts_flow() {
    let mut db = Db::<u32>::default();
    let mut cache = Cache::<u32>::default();
    let mut app = App::<u32, 8>::new();

    app.compute(&mut db, &mut cache).unwrap();

    assert_eq!(db.count, 1);
    assert_eq!(cache.total, 1);
}
