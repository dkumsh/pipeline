use crate::Error;
use crate::Reset;
use bitvec::prelude::*;
use std::vec::Vec;

/// A `Vec`-like container with per-slot **dirty** *and* **validity**
/// tracking.
///
/// # Two independent bits per slot
///
/// - **Dirty** (`update_flags`): per-cycle bit, set when a slot is
///   written via [`Vector::commit`] / [`Vector::invalidate`] /
///   [`Vector::push`] / [`Vector::push_committed`], cleared by
///   [`Vector::reset`]. Drives [`Vector::iter_updated_valid`].
/// - **Validity** (`valid_flags`): multi-cycle bit, set by
///   [`Vector::commit`] / [`Vector::push_committed`], cleared by
///   [`Vector::invalidate`]. **Not** touched by [`Vector::reset`] —
///   a slot that was valid at end of cycle stays valid at start of
///   the next cycle. Drives [`Vector::get_valid`] and
///   [`Vector::iter_updated_valid`].
///
/// The validity bit gives pipeline stages an `Option`-like read API:
/// a downstream stage that reads via [`Vector::get_valid`] cannot
/// reach the inner value without first matching on `Some` / `None`,
/// so the consistency / freshness check is enforced at the type
/// system level. See [`SlotWriter`] for the symmetric write side.
///
/// # Breaking change in 0.2.1
///
/// The legacy bypass methods `get`, `get_updated`, `get_mut`,
/// `iter_updated` were removed in 0.2.1. All access paths now engage
/// with the validity bit. Callers that genuinely need indexed
/// accumulation rather than single-value-per-slot should use
/// [`crate::value::buckets::Buckets`] instead.
pub struct Vector<V> {
    data: Vec<V>,         // Stores the actual values
    update_flags: BitVec, // Per-slot dirty bit (cleared by `reset`)
    valid_flags: BitVec,  // Per-slot validity bit (persists across `reset`)
    indices: Vec<usize>,  // Indices of updated elements for efficient iteration
    is_updated: bool,     // Flag to indicate if any element is updated
}

impl<V> Default for Vector<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> Vector<V> {
    /// Creates a new, empty `Vector`.
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            update_flags: BitVec::new(),
            valid_flags: BitVec::new(),
            indices: Vec::new(),
            is_updated: false,
        }
    }

    /// Creates a `Vector` with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            update_flags: BitVec::with_capacity(capacity),
            valid_flags: BitVec::with_capacity(capacity),
            indices: Vec::with_capacity(capacity),
            is_updated: false,
        }
    }

    /// Returns `true` if any element in the `Vector` is updated.
    pub fn is_updated(&self) -> bool {
        self.is_updated
    }

    /// Returns `true` if all elements in the `Vector` are updated.
    pub fn all_updated(&self) -> bool {
        self.is_updated && self.indices.is_empty()
    }

    /// Clears the `Vector`, removing all values.
    pub fn clear(&mut self) {
        self.data.clear();
        self.update_flags.clear();
        self.valid_flags.clear();
        self.indices.clear();
        self.is_updated = false;
    }

    /// Returns the number of elements in the `Vector`.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the `Vector` contains no elements.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns an iterator over the elements of the `Vector`.
    pub fn iter(&self) -> std::slice::Iter<'_, V> {
        self.data.iter()
    }

    /// Returns a mutable iterator over the elements of the `Vector`.
    /// Marks all elements as updated.
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, V> {
        if !self.all_updated() {
            self.is_updated = true;
            self.indices.clear();
            self.update_flags.clear();
        }
        self.data.iter_mut()
    }

    /// Returns a reference to the element at the given index, or `None` if out of bounds.
    /// Internal: mark slot `index` as dirty. Caller is responsible
    /// for ensuring `index < self.data.len()`.
    fn mark_dirty_internal(&mut self, index: usize) {
        if self.all_updated() {
            // All elements are already updated
            return;
        }
        // Ensure `update_flags` is large enough
        if index >= self.update_flags.len() {
            self.update_flags.resize(self.data.len(), false);
        }
        // Mark this index as updated
        if self.update_flags.get(index).as_deref() != Some(&true) {
            self.update_flags.set(index, true);
            self.indices.push(index);
            self.is_updated = true;

            // Check if all elements are now updated
            if self.indices.len() == self.data.len() {
                // All elements are updated; optimize storage
                self.indices.clear();
                self.update_flags.clear();
            }
        }
    }

    /// Returns a mutable reference to the element at the given index without
    /// updating dirty tracking.
    ///
    /// This is useful when callers want to reuse or clear previously touched
    /// storage between compute cycles without reporting a fresh update to
    /// downstream stages.
    #[inline]
    pub fn get_mut_untracked(&mut self, index: usize) -> Option<&mut V> {
        self.data.get_mut(index)
    }

    /// Appends an element to the back of the `Vector`, marking it as updated.
    ///
    /// The new slot is marked **invalid**: the pushed `value` is
    /// treated as a placeholder rather than a meaningful committed
    /// value. Use [`Vector::push_committed`] when the initial value
    /// is meaningful, or call [`Vector::commit`] on the new index
    /// afterwards.
    pub fn push(&mut self, value: V) {
        self.data.push(value);
        self.valid_flags.push(false);
        if self.all_updated() {
            // All elements are updated; no need to track individually
        } else {
            // Mark new element as updated
            self.update_flags.push(true);
            self.indices.push(self.data.len() - 1);
            self.is_updated = true;

            // Check if all elements are now updated
            if self.indices.len() == self.data.len() {
                // All elements are updated; optimize storage
                self.indices.clear();
                self.update_flags.clear();
            }
        }
    }

    /// Like [`Vector::push`], but the new slot is marked **valid**.
    ///
    /// Use this when you genuinely have a meaningful starting value
    /// for the slot; otherwise prefer `push` and explicitly
    /// [`Vector::commit`] later, which makes the moment the slot
    /// becomes valid explicit.
    pub fn push_committed(&mut self, value: V) {
        self.data.push(value);
        self.valid_flags.push(true);
        if self.all_updated() {
            // All elements are updated; no need to track individually
        } else {
            self.update_flags.push(true);
            self.indices.push(self.data.len() - 1);
            self.is_updated = true;

            if self.indices.len() == self.data.len() {
                self.indices.clear();
                self.update_flags.clear();
            }
        }
    }

    /// Removes the last element from the `Vector` and returns it, or `None` if empty.
    pub fn pop(&mut self) -> Option<V> {
        let value = self.data.pop();
        if value.is_some() {
            let index = self.data.len(); // Index of the removed element

            // Always pop the validity bit alongside data.
            self.valid_flags.pop();

            if self.all_updated() {
                // All elements are updated; no need to modify `indices` or `update_flags`
            } else {
                self.update_flags.pop();

                // Remove index from `indices` if present
                if let Some(pos) = self.indices.iter().position(|&i| i == index) {
                    let last = self.indices.len() - 1;
                    if pos != last {
                        // Swap the element at 'pos' with the last element
                        self.indices.swap(pos, last);
                    }
                    // Remove the last element
                    self.indices.pop();
                }
            }

            // **Update `is_updated` flag appropriately**
            if self.data.is_empty() {
                // If the vector is empty, there are no updates
                self.is_updated = false;
            } else {
                // Otherwise, update based on remaining elements
                self.is_updated = self.all_updated() || !self.indices.is_empty();
            }
        }
        value
    }

    // -------------------------------------------------------------
    // Validity-aware API
    // -------------------------------------------------------------

    /// Returns `true` if slot `index` is valid (has a committed value
    /// that has not been invalidated). Returns `false` if the index
    /// is out of bounds.
    #[inline]
    pub fn is_valid(&self, index: usize) -> bool {
        self.valid_flags
            .get(index)
            .as_deref()
            .copied()
            .unwrap_or(false)
    }

    /// Returns a reference to slot `index` if (and only if) it is
    /// **valid**. The Option-like read accessor: mirrors
    /// `Option::as_ref` and forces callers to match on `Some` /
    /// `None` before reaching the inner value.
    #[inline]
    pub fn get_valid(&self, index: usize) -> Option<&V> {
        if self.is_valid(index) {
            self.data.get(index)
        } else {
            None
        }
    }

    /// Write `value` into slot `index`, mark the slot **valid**, and
    /// mark it **dirty**. Panics if `index` is out of bounds.
    pub fn commit(&mut self, index: usize, value: V) {
        assert!(
            index < self.data.len(),
            "Vector::commit: index {} out of bounds (len = {})",
            index,
            self.data.len()
        );
        self.data[index] = value;
        self.valid_flags.set(index, true);
        self.mark_dirty_internal(index);
    }

    /// Apply `f` to slot `index` **in place**, then mark the slot
    /// **valid** and **dirty**.
    ///
    /// The point of this method is to mutate a large `V` without
    /// moving in a fresh value the way [`Vector::commit`] does. For
    /// a wide value type, `commit(i, fresh)` copies the whole struct
    /// in; `update(i, |v| v.field = new_field)` writes only the bytes
    /// that changed.
    ///
    /// The closure sees whatever the slot currently stores — the
    /// prior committed value, the placeholder from a non-
    /// [`Vector::push_committed`] [`Vector::push`], or stale bytes
    /// from a slot that was [`Vector::invalidate`]d. Callers that
    /// care about the prior validity should check
    /// [`Vector::is_valid`] / [`Vector::get_valid`] first.
    ///
    /// Panics if `index` is out of bounds.
    pub fn update<F>(&mut self, index: usize, f: F)
    where
        F: FnOnce(&mut V),
    {
        assert!(
            index < self.data.len(),
            "Vector::update: index {} out of bounds (len = {})",
            index,
            self.data.len()
        );
        f(&mut self.data[index]);
        self.valid_flags.set(index, true);
        self.mark_dirty_internal(index);
    }

    /// Mark slot `index` **invalid** and **dirty**. The slot retains
    /// its prior data; it just becomes invisible to readers using
    /// [`Vector::get_valid`]. Panics if `index` is out of bounds.
    pub fn invalidate(&mut self, index: usize) {
        assert!(
            index < self.data.len(),
            "Vector::invalidate: index {} out of bounds (len = {})",
            index,
            self.data.len()
        );
        self.valid_flags.set(index, false);
        self.mark_dirty_internal(index);
    }

    /// Acquire a one-shot writer for slot `index`. The returned
    /// [`SlotWriter`] is `#[must_use]` and **must** be consumed via
    /// [`SlotWriter::commit`] or [`SlotWriter::invalidate`]; dropping
    /// it without consumption triggers a `debug_assert!` in debug
    /// builds.
    ///
    /// Panics if `index` is out of bounds.
    #[must_use = "the returned SlotWriter must be consumed via commit() or invalidate()"]
    pub fn slot_writer(&mut self, index: usize) -> SlotWriter<'_, V> {
        assert!(
            index < self.data.len(),
            "Vector::slot_writer: index {} out of bounds (len = {})",
            index,
            self.data.len()
        );
        SlotWriter {
            vector: self,
            index,
            consumed: false,
        }
    }
}

impl<V> Reset for Vector<V> {
    /// Clears the dirty bits and indices. **Does not** touch
    /// validity bits: a slot that was valid before `reset` stays
    /// valid after `reset`. Dirty is per-cycle; validity is
    /// multi-cycle.
    type Error = Error;
    fn reset(&mut self) -> Result<(), Error> {
        self.update_flags.clear();
        self.update_flags.resize(self.data.len(), false);
        self.indices.clear();
        self.is_updated = false;
        Ok(())
    }
}

/// A one-shot, must-use writer for a single slot in a [`Vector`].
///
/// Acquired by [`Vector::slot_writer`]. Consume by exactly one of
/// [`SlotWriter::commit`] (write a value and mark the slot valid +
/// dirty) or [`SlotWriter::invalidate`] (mark the slot invalid +
/// dirty without changing its stored data).
///
/// Dropping a `SlotWriter` without consuming it triggers
/// `debug_assert!` in debug builds and is a silent no-op in release
/// builds. The `#[must_use]` annotation makes "forgot to acquire and
/// then forgot to use" a compile-time warning in `-D warnings`
/// projects.
#[must_use = "SlotWriter must be consumed via commit() or invalidate()"]
pub struct SlotWriter<'a, V> {
    vector: &'a mut Vector<V>,
    index: usize,
    consumed: bool,
}

impl<'a, V> SlotWriter<'a, V> {
    /// Write `value` into the target slot and mark it valid + dirty.
    pub fn commit(mut self, value: V) {
        self.vector.commit(self.index, value);
        self.consumed = true;
    }

    /// Mark the target slot invalid + dirty. The slot retains its
    /// prior stored data; it just becomes invisible via
    /// [`Vector::get_valid`].
    pub fn invalidate(mut self) {
        self.vector.invalidate(self.index);
        self.consumed = true;
    }
}

impl<'a, V> Drop for SlotWriter<'a, V> {
    fn drop(&mut self) {
        if !self.consumed {
            debug_assert!(
                false,
                "SlotWriter at index {} dropped without commit() or invalidate()",
                self.index
            );
        }
    }
}

impl<V> Vector<V> {
    /// Iterate dirty slots; yield `(index, Some(&V))` for slots that are
    /// also valid, `(index, None)` for dirty slots that are invalid.
    /// Equivalent in observable behaviour to walking the dirty set and
    /// applying `get_valid` per element, but skips the bounds re-check.
    pub fn iter_updated_valid(&self) -> IterUpdatedValidItems<'_, V> {
        if self.all_updated() {
            IterUpdatedValidItems {
                vector: self,
                indices_iter: None,
                current_index: 0,
                all_updated: true,
            }
        } else {
            IterUpdatedValidItems {
                vector: self,
                indices_iter: Some(self.indices.iter()),
                current_index: 0,
                all_updated: false,
            }
        }
    }
}

/// Iterator returned by [`Vector::iter_updated_valid`]. Yields
/// `(usize, Option<&V>)` for every **dirty** slot, where the `Option`
/// reflects the slot's **validity**.
pub struct IterUpdatedValidItems<'a, V> {
    vector: &'a Vector<V>,
    indices_iter: Option<std::slice::Iter<'a, usize>>,
    current_index: usize,
    all_updated: bool,
}

impl<'a, V> Iterator for IterUpdatedValidItems<'a, V> {
    type Item = (usize, Option<&'a V>);

    fn next(&mut self) -> Option<Self::Item> {
        let index = if self.all_updated {
            if self.current_index < self.vector.data.len() {
                let i = self.current_index;
                self.current_index += 1;
                i
            } else {
                return None;
            }
        } else if let Some(ref mut indices_iter) = self.indices_iter {
            *indices_iter.next()?
        } else {
            return None;
        };
        let opt = if self.vector.is_valid(index) {
            Some(&self.vector.data[index])
        } else {
            None
        };
        Some((index, opt))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_vector() {
        let vec: Vector<i32> = Vector::new();
        assert_eq!(vec.len(), 0);
        assert!(vec.is_empty());
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());
    }

    #[test]
    fn test_with_capacity() {
        let mut vec: Vector<i32> = Vector::with_capacity(10);
        assert_eq!(vec.len(), 0);
        assert_eq!(vec.data.capacity(), 10);
        assert_eq!(vec.update_flags.len(), 0);
        assert_eq!(vec.valid_flags.len(), 0);
        assert_eq!(vec.indices.capacity(), 10);
        assert!(!vec.is_valid(0));

        vec.push_committed(42);
        assert_eq!(vec.get_valid(0), Some(&42));

        assert_eq!(vec.pop(), Some(42));
        assert_eq!(vec.len(), 0);
        assert!(!vec.is_valid(0));
    }

    #[test]
    fn get_mut_untracked_does_not_mark_updated() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push_committed(String::from("a"));
        vec.push_committed(String::from("b"));
        vec.reset()?;

        if let Some(value) = vec.get_mut_untracked(1) {
            value.clear();
        }

        assert_eq!(vec.get_valid(1).map(String::as_str), Some(""));
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());
        assert!(vec.iter_updated_valid().next().is_none());
        Ok(())
    }

    #[test]
    fn reset_clears_dirty_preserves_data() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);
        vec.push_committed(3);

        assert!(vec.is_updated());

        vec.reset()?;
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());
        assert_eq!(vec.iter_updated_valid().count(), 0);
        // Reset preserves data + validity, just clears dirty.
        assert_eq!(vec.get_valid(0), Some(&1));
        assert_eq!(vec.get_valid(1), Some(&2));
        assert_eq!(vec.get_valid(2), Some(&3));

        vec.commit(1, 20);
        assert!(vec.is_updated());
        assert_eq!(vec.get_valid(1), Some(&20));
        Ok(())
    }

    #[test]
    fn clear_drops_all_state() {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);
        vec.push_committed(3);

        vec.clear();
        assert_eq!(vec.len(), 0);
        assert!(vec.is_empty());
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());
    }

    #[test]
    fn pop_drops_last_slot_and_its_bits() {
        let mut vec = Vector::new();
        vec.push_committed(10);
        vec.push_committed(20);
        vec.push_committed(30);

        let val = vec.pop();
        assert_eq!(val, Some(30));
        assert_eq!(vec.len(), 2);
        assert!(vec.is_updated());

        vec.pop();
        vec.pop();
        assert!(vec.is_empty());
        assert!(!vec.is_updated());
    }

    #[test]
    fn out_of_bounds_returns_none() {
        let mut vec = Vector::new();
        vec.push_committed(1);

        assert_eq!(vec.get_valid(1), None);
        assert!(!vec.is_valid(1));
    }

    #[test]
    fn iter_and_iter_mut_walk_raw_data() {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);
        vec.push_committed(3);

        let collected: Vec<&i32> = vec.iter().collect();
        assert_eq!(collected, vec![&1, &2, &3]);

        for val in vec.iter_mut() {
            *val *= 2;
        }

        let collected: Vec<&i32> = vec.iter().collect();
        assert_eq!(collected, vec![&2, &4, &6]);
    }

    #[test]
    fn push_after_all_updated_extends_correctly() {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);

        // Now push a third element — should mark dirty + invalid.
        vec.push(3);
        assert!(vec.is_updated());

        let updated: Vec<(usize, Option<i32>)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, v.copied()))
            .collect();
        assert_eq!(updated, vec![(0, Some(1)), (1, Some(2)), (2, None)]);
    }

    #[test]
    fn commit_marks_all_slots_dirty() {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);
        vec.push_committed(3);

        vec.commit(0, 10);
        vec.commit(1, 20);
        vec.commit(2, 30);

        let updated: Vec<(usize, i32)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, *v.unwrap()))
            .collect();
        assert_eq!(updated, vec![(0, 10), (1, 20), (2, 30)]);
    }

    #[test]
    fn reset_after_committed_pushes_keeps_validity() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);

        vec.reset()?;
        assert!(!vec.is_updated());
        assert_eq!(vec.iter_updated_valid().count(), 0);
        assert_eq!(vec.get_valid(0), Some(&1));
        assert_eq!(vec.get_valid(1), Some(&2));

        vec.commit(0, 10);
        let updated: Vec<(usize, i32)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, *v.unwrap()))
            .collect();
        assert_eq!(updated, vec![(0, 10)]);
        Ok(())
    }

    #[test]
    fn push_committed_reads_back_via_get_valid() {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);
        vec.push_committed(3);

        assert_eq!(vec.len(), 3);
        assert_eq!(vec.get_valid(0), Some(&1));
        assert_eq!(vec.get_valid(1), Some(&2));
        assert_eq!(vec.get_valid(2), Some(&3));
        assert_eq!(vec.get_valid(3), None);

        assert!(vec.is_updated());
    }

    // -----------------------------------------------------------------
    // Validity-tracking tests
    // -----------------------------------------------------------------

    #[test]
    fn validity_push_is_invalid_by_default() {
        let mut vec = Vector::new();
        vec.push(42);
        assert!(!vec.is_valid(0));
        assert_eq!(vec.get_valid(0), None);
        // Out-of-bounds index is also "not valid".
        assert!(!vec.is_valid(1));
        assert_eq!(vec.get_valid(1), None);
    }

    #[test]
    fn validity_push_committed_is_valid() {
        let mut vec = Vector::new();
        vec.push_committed(42);
        assert!(vec.is_valid(0));
        assert_eq!(vec.get_valid(0), Some(&42));
        assert!(vec.is_updated());
    }

    #[test]
    fn validity_commit_roundtrip() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push(0); // invalid placeholder
        assert!(!vec.is_valid(0));

        vec.commit(0, 42);
        assert!(vec.is_valid(0));
        assert_eq!(vec.get_valid(0), Some(&42));
        assert!(vec.is_updated());

        vec.reset()?;
        // Reset clears dirty, keeps valid.
        assert!(!vec.is_updated());
        assert!(vec.is_valid(0));
        assert_eq!(vec.get_valid(0), Some(&42));
        Ok(())
    }

    #[test]
    fn validity_invalidate_marks_dirty_and_clears_valid() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push_committed(42);
        vec.reset()?;
        assert!(!vec.is_updated());

        vec.invalidate(0);
        assert!(!vec.is_valid(0));
        assert_eq!(vec.get_valid(0), None);
        // Slot retains its prior raw data internally (we don't drop V
        // on invalidate), but it's invisible through any public API.
        assert!(vec.is_updated());
        Ok(())
    }

    #[test]
    fn validity_reset_preserves_validity_clears_dirty() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push(0);
        vec.commit(0, 100);
        vec.reset()?;
        assert!(!vec.is_updated());
        assert!(vec.is_valid(0)); // <-- key property
        assert_eq!(vec.get_valid(0), Some(&100));
        Ok(())
    }

    #[test]
    fn validity_slot_writer_commit_consumes_without_panic() {
        let mut vec = Vector::new();
        vec.push(0);
        let writer = vec.slot_writer(0);
        writer.commit(99);
        assert_eq!(vec.get_valid(0), Some(&99));
    }

    #[test]
    fn validity_slot_writer_invalidate_consumes_without_panic() {
        let mut vec = Vector::new();
        vec.push_committed(123);
        let writer = vec.slot_writer(0);
        writer.invalidate();
        assert_eq!(vec.get_valid(0), None);
        assert!(vec.is_updated());
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "SlotWriter at index 0 dropped without commit() or invalidate()")]
    fn validity_slot_writer_drop_without_consume_panics_in_debug() {
        let mut vec: Vector<i32> = Vector::new();
        vec.push(0);
        let _writer = vec.slot_writer(0);
        // drop without commit/invalidate
    }

    #[test]
    fn validity_iter_updated_valid_yields_option_per_slot() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push(0); // index 0, invalid placeholder, dirty
        vec.push(0); // index 1, invalid placeholder, dirty
        vec.push(0); // index 2, invalid placeholder, dirty
        vec.commit(0, 10);
        vec.commit(1, 20);
        // index 2 stays invalid

        let collected: Vec<(usize, Option<i32>)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, v.copied()))
            .collect();
        assert_eq!(
            collected,
            vec![(0, Some(10)), (1, Some(20)), (2, None)],
            "all three slots are dirty; only the two committed ones expose Some(v)"
        );

        vec.reset()?;
        let collected: Vec<(usize, Option<i32>)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, v.copied()))
            .collect();
        assert!(collected.is_empty(), "after reset no slots are dirty");

        // Invalidate slot 0 — becomes dirty again, yields None.
        vec.invalidate(0);
        let collected: Vec<(usize, Option<i32>)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, v.copied()))
            .collect();
        assert_eq!(collected, vec![(0, None)]);
        Ok(())
    }

    #[test]
    fn update_mutates_in_place_and_marks_valid_dirty() -> Result<(), Error> {
        // Start with a valid value, reset to drop the dirty flag, then
        // `update` it. The closure should see the prior value and the
        // slot should come back dirty + valid.
        let mut vec = Vector::new();
        vec.push_committed(vec![1, 2, 3]);
        vec.reset()?;
        assert!(!vec.is_updated());

        let mut prior = None;
        vec.update(0, |v| {
            prior = Some(v.clone());
            v.push(4);
        });

        assert_eq!(prior, Some(vec![1, 2, 3]));
        assert!(vec.is_valid(0));
        assert!(vec.is_updated());
        assert_eq!(vec.get_valid(0), Some(&vec![1, 2, 3, 4]));
        Ok(())
    }

    #[test]
    fn update_on_invalid_slot_makes_it_valid() {
        // A slot that was `push`ed (placeholder, not valid) becomes
        // valid after `update` — the closure sees the placeholder
        // bytes, then we mark it as a real committed value.
        let mut vec = Vector::new();
        vec.push(99); // invalid placeholder
        assert!(!vec.is_valid(0));

        let mut seen_placeholder = None;
        vec.update(0, |v| {
            seen_placeholder = Some(*v);
            *v = 42;
        });

        assert_eq!(seen_placeholder, Some(99));
        assert!(vec.is_valid(0));
        assert_eq!(vec.get_valid(0), Some(&42));
    }

    #[test]
    fn update_redirties_after_reset() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push_committed(10);
        vec.reset()?;
        assert!(!vec.is_updated());

        vec.update(0, |v| *v += 5);
        assert!(vec.is_updated());
        let collected: Vec<(usize, Option<i32>)> = vec
            .iter_updated_valid()
            .map(|(i, v)| (i, v.copied()))
            .collect();
        assert_eq!(collected, vec![(0, Some(15))]);
        Ok(())
    }

    #[test]
    #[should_panic(expected = "Vector::update: index 1 out of bounds")]
    fn update_out_of_bounds_panics() {
        let mut vec: Vector<i32> = Vector::new();
        vec.push_committed(7);
        vec.update(1, |v| *v += 1);
    }

    #[test]
    fn validity_pop_keeps_remaining_bits_aligned() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push(0);
        vec.push_committed(3);
        vec.reset()?;
        assert!(vec.is_valid(0));
        assert!(!vec.is_valid(1));
        assert!(vec.is_valid(2));

        let popped = vec.pop();
        assert_eq!(popped, Some(3));
        assert_eq!(vec.len(), 2);
        assert!(vec.is_valid(0));
        assert!(!vec.is_valid(1));
        Ok(())
    }

    #[test]
    fn validity_clear_resets_everything() {
        let mut vec = Vector::new();
        vec.push_committed(1);
        vec.push_committed(2);
        vec.clear();
        assert_eq!(vec.len(), 0);
        assert!(vec.is_empty());
        assert!(!vec.is_updated());
    }
}
