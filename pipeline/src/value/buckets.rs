//! Indexed-bucket container with cycle-scoped accumulation semantics.
//!
//! A [`Buckets<T>`] is a fixed-index collection of `Vec<T>` buckets. Each
//! `push(i, v)` appends `v` to bucket `i` in place; the first push into an
//! empty bucket marks the bucket *touched* for the current compute cycle.
//! [`Buckets::reset`] clears each touched bucket's contents (preserving its
//! allocated capacity for the next cycle) and clears the touched list, so the
//! container ends a cycle with empty-but-allocated buckets ready to be filled
//! again.
//!
//! This is the natural sibling of [`crate::value::vector::Vector`]:
//!
//! - `Vector<T>` is **single value per slot, validity-aware**: `commit` /
//!   `invalidate` write semantics, validity persists across `reset`.
//! - `Buckets<T>` is **accumulating events per slot, dirty-only**: each cycle
//!   may push many items into a bucket, `reset` drains them.
//!
//! `Buckets` does not carry a validity bit because its absence semantic is
//! already perfectly encoded by the touched-list: a bucket not in `touched`
//! has no events for the current cycle, period. Adding a separate validity bit
//! would shadow the dirty bit with the same answer.

use crate::{Error, Reset};

/// Indexed-bucket container; see module-level docs.
pub struct Buckets<T> {
    buckets: Vec<Vec<T>>,
    /// Indices of buckets that had at least one push in the current cycle.
    /// Ordered by first-touch; used as the fast-iter path and as the reset
    /// drain list (so reset is O(touched.len()) rather than O(len())).
    touched: Vec<usize>,
}

impl<T> Default for Buckets<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Buckets<T> {
    /// Creates an empty container with zero buckets. Use
    /// [`Buckets::push_bucket`] or [`Buckets::resize_buckets`] to add slots.
    pub fn new() -> Self {
        Self {
            buckets: Vec::new(),
            touched: Vec::new(),
        }
    }

    /// Creates a container pre-sized with `count` empty buckets.
    pub fn with_buckets(count: usize) -> Self {
        let mut b = Self {
            buckets: Vec::with_capacity(count),
            touched: Vec::new(),
        };
        b.resize_buckets(count);
        b
    }

    /// Returns the number of buckets.
    pub fn len(&self) -> usize {
        self.buckets.len()
    }

    /// Returns `true` if the container has no buckets.
    pub fn is_empty(&self) -> bool {
        self.buckets.is_empty()
    }

    /// Returns `true` if any bucket was touched in the current cycle.
    pub fn is_updated(&self) -> bool {
        !self.touched.is_empty()
    }

    /// Appends a new empty bucket.
    pub fn push_bucket(&mut self) {
        self.buckets.push(Vec::new());
    }

    /// Resizes the number of buckets, preserving contents for retained slots
    /// and leaving the container clean (no touched buckets) afterwards.
    pub fn resize_buckets(&mut self, count: usize) {
        self.buckets.resize_with(count, Vec::new);
        self.touched.clear();
    }

    /// Returns the bucket at `index`, or `None` if out of bounds. The returned
    /// `&Vec<T>` may be empty if the bucket has not been touched this cycle
    /// (or any prior cycle); use [`Buckets::is_touched`] to disambiguate.
    pub fn get(&self, index: usize) -> Option<&Vec<T>> {
        self.buckets.get(index)
    }

    /// Returns `true` if bucket `index` has been touched in the current cycle.
    pub fn is_touched(&self, index: usize) -> bool {
        self.touched.contains(&index)
    }

    /// Pushes `value` into bucket `index`. If the bucket was empty before the
    /// push, the bucket is added to the touched list. Returns `Err(value)`
    /// (giving ownership back to the caller) if `index` is out of bounds.
    pub fn push(&mut self, index: usize, value: T) -> Result<(), T> {
        let Some(bucket) = self.buckets.get_mut(index) else {
            return Err(value);
        };
        if bucket.is_empty() {
            self.touched.push(index);
        }
        bucket.push(value);
        Ok(())
    }

    /// Iterates over touched buckets in first-touch order, yielding
    /// `(index, &Vec<T>)` for each. An untouched bucket is not yielded.
    pub fn iter_updated(&self) -> impl Iterator<Item = (usize, &Vec<T>)> {
        self.touched
            .iter()
            .filter_map(move |&i| self.buckets.get(i).map(|b| (i, b)))
    }
}

impl<T> Reset for Buckets<T> {
    type Error = Error;

    /// Drains the contents of every touched bucket (preserving allocated
    /// capacity) and clears the touched list. Untouched buckets are left
    /// alone.
    fn reset(&mut self) -> Result<(), Self::Error> {
        for index in self.touched.drain(..) {
            if let Some(bucket) = self.buckets.get_mut(index) {
                bucket.clear();
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_is_empty() {
        let b: Buckets<i32> = Buckets::new();
        assert_eq!(b.len(), 0);
        assert!(b.is_empty());
        assert!(!b.is_updated());
    }

    #[test]
    fn with_buckets_starts_clean() {
        let b: Buckets<i32> = Buckets::with_buckets(3);
        assert_eq!(b.len(), 3);
        assert!(!b.is_updated());
        assert!(!b.is_touched(0));
        assert_eq!(b.iter_updated().count(), 0);
    }

    #[test]
    fn push_marks_touched_and_appends() {
        let mut b = Buckets::with_buckets(3);
        b.push(0, 10).unwrap();
        b.push(2, 20).unwrap();
        b.push(0, 11).unwrap(); // second push into bucket 0

        assert!(b.is_touched(0));
        assert!(!b.is_touched(1));
        assert!(b.is_touched(2));

        let collected: Vec<(usize, Vec<i32>)> =
            b.iter_updated().map(|(i, v)| (i, v.clone())).collect();
        assert_eq!(collected, vec![(0, vec![10, 11]), (2, vec![20])]);
    }

    #[test]
    fn push_out_of_bounds_returns_err_with_value() {
        let mut b: Buckets<i32> = Buckets::with_buckets(2);
        let err = b.push(5, 99).unwrap_err();
        assert_eq!(err, 99);
        assert!(!b.is_updated());
    }

    #[test]
    fn reset_clears_only_touched_preserves_capacity() -> Result<(), Error> {
        let mut b: Buckets<i32> = Buckets::with_buckets(3);
        b.push(0, 10).unwrap();
        b.push(2, 20).unwrap();

        // Capture pre-reset capacity to confirm preservation.
        let cap_before_0 = b.get(0).unwrap().capacity();
        let cap_before_2 = b.get(2).unwrap().capacity();

        b.reset()?;

        assert!(!b.is_updated());
        assert_eq!(b.get(0).unwrap(), &Vec::<i32>::new());
        assert_eq!(b.get(1).unwrap(), &Vec::<i32>::new());
        assert_eq!(b.get(2).unwrap(), &Vec::<i32>::new());
        assert_eq!(b.iter_updated().count(), 0);

        assert!(b.get(0).unwrap().capacity() >= cap_before_0);
        assert!(b.get(2).unwrap().capacity() >= cap_before_2);

        // After reset, pushing again still works and marks touched again.
        b.push(2, 30).unwrap();
        let collected: Vec<(usize, Vec<i32>)> =
            b.iter_updated().map(|(i, v)| (i, v.clone())).collect();
        assert_eq!(collected, vec![(2, vec![30])]);
        Ok(())
    }

    #[test]
    fn resize_preserves_retained_data_and_clears_dirty() {
        let mut b = Buckets::with_buckets(2);
        b.push(1, 20).unwrap();

        b.resize_buckets(4);
        assert_eq!(b.len(), 4);
        assert_eq!(b.get(1).unwrap(), &vec![20]);
        assert!(!b.is_updated()); // resize clears touched list

        b.resize_buckets(1);
        assert_eq!(b.len(), 1);
        assert!(!b.is_updated());
    }

    #[test]
    fn second_push_into_touched_bucket_does_not_double_count() {
        let mut b = Buckets::with_buckets(2);
        b.push(0, 1).unwrap();
        b.push(0, 2).unwrap();
        b.push(0, 3).unwrap();

        // Even with three pushes, touched should only contain index 0 once.
        assert_eq!(b.iter_updated().count(), 1);
        let (idx, vec) = b.iter_updated().next().unwrap();
        assert_eq!(idx, 0);
        assert_eq!(vec, &vec![1, 2, 3]);
    }
}
