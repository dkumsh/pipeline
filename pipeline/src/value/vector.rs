use crate::Error;
use crate::Reset;
use std::vec::Vec;

#[inline]
fn word_bit(i: usize) -> (usize, usize) {
    (i / 64, i % 64)
}

/// A `Vec`-like container with per-slot **dirty** *and* **validity**
/// tracking.
///
/// # Two independent bits per slot
///
/// - **Dirty** (`dirty`): per-cycle bit, set when a slot is written
///   via [`Vector::commit`] / [`Vector::invalidate`] /
///   [`Vector::push`] / [`Vector::push_committed`] / [`Vector::update`],
///   cleared by [`Vector::reset`]. Drives [`Vector::iter_updated_valid`].
/// - **Validity** (`valid`): multi-cycle bit, set by
///   [`Vector::commit`] / [`Vector::push_committed`] /
///   [`Vector::update`], cleared by [`Vector::invalidate`]. **Not**
///   touched by [`Vector::reset`] — a slot that was valid at end of
///   cycle stays valid at start of the next cycle. Drives
///   [`Vector::get_valid`] and [`Vector::iter_updated_valid`].
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
    data: Vec<V>,
    /// Per-slot dirty bits, packed 64 per `u64`. Cleared by `reset`.
    dirty: Vec<u64>,
    /// Per-slot validity bits, packed 64 per `u64`. Persists across `reset`.
    valid: Vec<u64>,
    /// O(1) "anything dirty?" and "how many dirty?" — incremented on
    /// every transition `0 → 1` and decremented on `1 → 0`.
    dirty_count: usize,
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
            dirty: Vec::new(),
            valid: Vec::new(),
            dirty_count: 0,
        }
    }

    /// Creates a `Vector` with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        let words = capacity.div_ceil(64);
        Self {
            data: Vec::with_capacity(capacity),
            dirty: Vec::with_capacity(words),
            valid: Vec::with_capacity(words),
            dirty_count: 0,
        }
    }

    /// Returns `true` if any element in the `Vector` is updated.
    #[inline]
    pub fn is_updated(&self) -> bool {
        self.dirty_count != 0
    }

    /// Returns `true` if every element in the `Vector` is updated.
    /// Returns `false` for an empty `Vector`.
    #[inline]
    pub fn all_updated(&self) -> bool {
        !self.data.is_empty() && self.dirty_count == self.data.len()
    }

    /// Clears the `Vector`, removing all values.
    pub fn clear(&mut self) {
        self.data.clear();
        self.dirty.clear();
        self.valid.clear();
        self.dirty_count = 0;
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
        let len = self.data.len();
        if self.dirty_count < len {
            let full_words = len / 64;
            let rem = len % 64;
            for w in 0..full_words {
                self.dirty[w] = u64::MAX;
            }
            if rem != 0 {
                self.dirty[full_words] = (1u64 << rem) - 1;
            }
            self.dirty_count = len;
        }
        self.data.iter_mut()
    }

    /// Internal: mark slot `index` as dirty. Caller is responsible
    /// for ensuring `index < self.data.len()` and the dirty word
    /// for `index` already exists in `self.dirty`.
    #[inline]
    fn mark_dirty_internal(&mut self, index: usize) {
        let (w, b) = word_bit(index);
        let mask = 1u64 << b;
        if (self.dirty[w] & mask) == 0 {
            self.dirty[w] |= mask;
            self.dirty_count += 1;
        }
    }

    /// Internal: set or clear the validity bit for slot `index`.
    #[inline]
    fn set_valid_internal(&mut self, index: usize, value: bool) {
        let (w, b) = word_bit(index);
        let mask = 1u64 << b;
        if value {
            self.valid[w] |= mask;
        } else {
            self.valid[w] &= !mask;
        }
    }

    /// Internal: extend `dirty` and `valid` so they cover slot
    /// `self.data.len() - 1` (called after a push has already
    /// extended `self.data`).
    #[inline]
    fn extend_bitsets_for_last_push(&mut self) {
        let new_idx = self.data.len() - 1;
        if new_idx.is_multiple_of(64) {
            self.dirty.push(0);
            self.valid.push(0);
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
        self.extend_bitsets_for_last_push();
        let idx = self.data.len() - 1;
        // valid stays 0 (invalid placeholder).
        self.mark_dirty_internal(idx);
    }

    /// Like [`Vector::push`], but the new slot is marked **valid**.
    ///
    /// Use this when you genuinely have a meaningful starting value
    /// for the slot; otherwise prefer `push` and explicitly
    /// [`Vector::commit`] later, which makes the moment the slot
    /// becomes valid explicit.
    pub fn push_committed(&mut self, value: V) {
        self.data.push(value);
        self.extend_bitsets_for_last_push();
        let idx = self.data.len() - 1;
        self.set_valid_internal(idx, true);
        self.mark_dirty_internal(idx);
    }

    /// Removes the last element from the `Vector` and returns it, or `None` if empty.
    pub fn pop(&mut self) -> Option<V> {
        if self.data.is_empty() {
            return None;
        }
        let idx = self.data.len() - 1;
        let (w, b) = word_bit(idx);
        let mask = 1u64 << b;
        // If the popped slot was dirty, decrement the count.
        if (self.dirty[w] & mask) != 0 {
            self.dirty_count -= 1;
        }
        // Clear both bits for the popped slot.
        self.dirty[w] &= !mask;
        self.valid[w] &= !mask;
        // If the popped slot owned the last word entirely, drop it.
        if idx.is_multiple_of(64) {
            self.dirty.pop();
            self.valid.pop();
        }
        self.data.pop()
    }

    // -------------------------------------------------------------
    // Validity-aware API
    // -------------------------------------------------------------

    /// Returns `true` if slot `index` is valid (has a committed value
    /// that has not been invalidated). Returns `false` if the index
    /// is out of bounds.
    #[inline]
    pub fn is_valid(&self, index: usize) -> bool {
        if index >= self.data.len() {
            return false;
        }
        let (w, b) = word_bit(index);
        (self.valid[w] & (1u64 << b)) != 0
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
        self.set_valid_internal(index, true);
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
        self.set_valid_internal(index, true);
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
        self.set_valid_internal(index, false);
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
        for w in &mut self.dirty {
            *w = 0;
        }
        self.dirty_count = 0;
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
    /// Iterate dirty slots in **ascending index order**, yielding
    /// `(index, Some(&V))` for slots that are also valid and
    /// `(index, None)` for dirty slots that are invalid.
    pub fn iter_updated_valid(&self) -> IterUpdatedValidItems<'_, V> {
        IterUpdatedValidItems::new(self)
    }

    /// Iterate **indices** of dirty slots in ascending order. Skips
    /// the validity check and data lookup that
    /// [`Vector::iter_updated_valid`] performs — useful when you only
    /// need the slot indices, e.g. to drive a parallel walk over
    /// another `Vector` or external array keyed by the same slot.
    pub fn iter_updated_indices(&self) -> IterUpdatedIndices<'_> {
        IterUpdatedIndices::new(&self.dirty, self.data.len())
    }
}

/// Iterator returned by [`Vector::iter_updated_valid`]. Yields
/// `(usize, Option<&V>)` for every **dirty** slot, where the `Option`
/// reflects the slot's **validity**. Always in ascending index order.
pub struct IterUpdatedValidItems<'a, V> {
    vector: &'a Vector<V>,
    word_idx: usize,
    word: u64,
}

impl<'a, V> IterUpdatedValidItems<'a, V> {
    fn new(v: &'a Vector<V>) -> Self {
        let mut it = Self {
            vector: v,
            word_idx: 0,
            word: 0,
        };
        it.advance_to_nonzero();
        it
    }

    fn advance_to_nonzero(&mut self) {
        while self.word == 0 && self.word_idx < self.vector.dirty.len() {
            let mut w = self.vector.dirty[self.word_idx];
            // Mask padding bits in the last word so we don't yield
            // indices >= data.len().
            if self.word_idx + 1 == self.vector.dirty.len() {
                let rem = self.vector.data.len() % 64;
                if rem != 0 {
                    w &= (1u64 << rem) - 1;
                }
            }
            self.word = w;
            if self.word != 0 {
                break;
            }
            self.word_idx += 1;
        }
    }
}

impl<'a, V> Iterator for IterUpdatedValidItems<'a, V> {
    type Item = (usize, Option<&'a V>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.word == 0 {
            self.advance_to_nonzero();
            if self.word == 0 {
                return None;
            }
        }
        let tz = self.word.trailing_zeros() as usize;
        let idx = self.word_idx * 64 + tz;
        // Pop the bit from the snapshot word.
        self.word &= self.word - 1;
        let opt = if self.vector.is_valid(idx) {
            Some(&self.vector.data[idx])
        } else {
            None
        };
        if self.word == 0 {
            self.word_idx += 1;
        }
        Some((idx, opt))
    }
}

/// Iterator returned by [`Vector::iter_updated_indices`]. Yields the
/// `usize` index of every **dirty** slot in ascending order, with no
/// validity check or data lookup.
pub struct IterUpdatedIndices<'a> {
    dirty: &'a [u64],
    total_len: usize,
    word_idx: usize,
    word: u64,
}

impl<'a> IterUpdatedIndices<'a> {
    fn new(dirty: &'a [u64], total_len: usize) -> Self {
        let mut it = Self {
            dirty,
            total_len,
            word_idx: 0,
            word: 0,
        };
        it.advance_to_nonzero();
        it
    }

    fn advance_to_nonzero(&mut self) {
        while self.word == 0 && self.word_idx < self.dirty.len() {
            let mut w = self.dirty[self.word_idx];
            if self.word_idx + 1 == self.dirty.len() {
                let rem = self.total_len % 64;
                if rem != 0 {
                    w &= (1u64 << rem) - 1;
                }
            }
            self.word = w;
            if self.word != 0 {
                break;
            }
            self.word_idx += 1;
        }
    }
}

impl<'a> Iterator for IterUpdatedIndices<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.word == 0 {
            self.advance_to_nonzero();
            if self.word == 0 {
                return None;
            }
        }
        let tz = self.word.trailing_zeros() as usize;
        let idx = self.word_idx * 64 + tz;
        self.word &= self.word - 1;
        if self.word == 0 {
            self.word_idx += 1;
        }
        Some(idx)
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
        // 10 slots fit in a single u64 word, so the bitsets reserve
        // exactly one word.
        assert!(vec.dirty.capacity() >= 1);
        assert!(vec.valid.capacity() >= 1);
        assert_eq!(vec.dirty.len(), 0);
        assert_eq!(vec.valid.len(), 0);
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

    // -----------------------------------------------------------------
    // iter_updated_indices tests
    // -----------------------------------------------------------------

    #[test]
    fn iter_updated_indices_yields_dirty_in_ascending_order() -> Result<(), Error> {
        let mut vec = Vector::new();
        for _ in 0..5 {
            vec.push_committed(0);
        }
        vec.reset()?;
        assert_eq!(vec.iter_updated_indices().count(), 0);

        // Commit out of order; iter must still yield in ascending order.
        vec.commit(3, 30);
        vec.commit(0, 10);
        vec.commit(4, 40);

        let got: Vec<usize> = vec.iter_updated_indices().collect();
        assert_eq!(got, vec![0, 3, 4]);
        Ok(())
    }

    #[test]
    fn iter_updated_indices_handles_word_boundaries() {
        // Touch slots straddling word boundaries (63 / 64 / 127 / 128).
        let mut vec: Vector<u32> = Vector::new();
        for _ in 0..200 {
            vec.push(0);
        }
        // Reset so only the commits below count.
        vec.reset().unwrap();
        for &i in &[0_usize, 63, 64, 127, 128, 199] {
            vec.commit(i, i as u32);
        }
        let got: Vec<usize> = vec.iter_updated_indices().collect();
        assert_eq!(got, vec![0, 63, 64, 127, 128, 199]);
    }

    #[test]
    fn iter_updated_indices_empty_when_nothing_dirty() -> Result<(), Error> {
        let mut vec: Vector<i32> = Vector::new();
        assert_eq!(vec.iter_updated_indices().count(), 0);

        vec.push_committed(1);
        vec.reset()?;
        assert_eq!(vec.iter_updated_indices().count(), 0);
        Ok(())
    }

    #[test]
    fn iter_updated_indices_ignores_validity() {
        // Dirty + invalid slot must still show up — this iterator
        // is index-only and doesn't gate on validity.
        let mut vec: Vector<i32> = Vector::new();
        vec.push(0); // dirty, invalid
        vec.push_committed(99); // dirty, valid
        let got: Vec<usize> = vec.iter_updated_indices().collect();
        assert_eq!(got, vec![0, 1]);
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
