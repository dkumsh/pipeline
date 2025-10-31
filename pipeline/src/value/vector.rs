use crate::Error;
use crate::value::Reset;
use bitvec::prelude::*;
use std::vec::Vec;

pub struct Vector<V> {
    data: Vec<V>,         // Stores the actual values
    update_flags: BitVec, // Bit vector to track which values are updated
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
            indices: Vec::new(),
            is_updated: false,
        }
    }

    /// Creates a `Vector` with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            update_flags: bitvec![0; capacity],
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
    pub fn get(&self, index: usize) -> Option<&V> {
        self.data.get(index)
    }

    /// Returns a reference to the updated element at the given index, or `None` if not updated or out of bounds.
    pub fn get_updated(&self, index: usize) -> Option<&V> {
        if self.all_updated() || (self.update_flags.get(index).as_deref() == Some(&true)) {
            self.data.get(index)
        } else {
            None
        }
    }

    /// Returns a mutable reference to the element at the given index, marking it as updated.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut V> {
        if index >= self.data.len() {
            return None;
        }

        if self.all_updated() {
            // All elements are already updated
        } else {
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

        self.data.get_mut(index)
    }

    /// Appends an element to the back of the `Vector`, marking it as updated.
    pub fn push(&mut self, value: V) {
        self.data.push(value);
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

    /// Removes the last element from the `Vector` and returns it, or `None` if empty.
    pub fn pop(&mut self) -> Option<V> {
        let value = self.data.pop();
        if value.is_some() {
            let index = self.data.len(); // Index of the removed element

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
}

impl<V> Reset for Vector<V> {
    fn reset(&mut self) -> Result<(), Error> {
        self.update_flags.clear();
        self.update_flags.resize(self.data.len(), false);
        self.indices.clear();
        self.is_updated = false;
        Ok(())
    }
}

pub trait IterUpdated<'a, K: 'a, V: 'a> {
    type Iter: Iterator<Item = (K, &'a V)>;
    fn iter_updated(&'a self) -> Self::Iter;
}

impl<'a, V: 'a> IterUpdated<'a, usize, V> for Vector<V> {
    type Iter = IterUpdatedItems<'a, V>;

    fn iter_updated(&'a self) -> Self::Iter {
        if self.all_updated() {
            IterUpdatedItems {
                vector: self,
                indices_iter: None,
                current_index: 0,
                all_updated: true,
            }
        } else {
            IterUpdatedItems {
                vector: self,
                indices_iter: Some(self.indices.iter()),
                current_index: 0,
                all_updated: false,
            }
        }
    }
}

pub struct IterUpdatedItems<'a, V> {
    vector: &'a Vector<V>,
    indices_iter: Option<std::slice::Iter<'a, usize>>,
    current_index: usize,
    all_updated: bool,
}

impl<'a, V> Iterator for IterUpdatedItems<'a, V> {
    type Item = (usize, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.all_updated {
            if self.current_index < self.vector.data.len() {
                let index = self.current_index;
                self.current_index += 1;
                Some((index, &self.vector.data[index]))
            } else {
                None
            }
        } else if let Some(ref mut indices_iter) = self.indices_iter {
            indices_iter
                .next()
                .map(|&index| (index, &self.vector.data[index]))
        } else {
            None
        }
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
        let vec: Vector<i32> = Vector::with_capacity(10);
        assert_eq!(vec.len(), 0);
        assert_eq!(vec.data.capacity(), 10);
    }

    #[test]
    fn test_get_mut() {
        let mut vec = Vector::new();
        vec.push(10);
        vec.push(20);
        vec.push(30);

        // Modify an element
        if let Some(val) = vec.get_mut(1) {
            *val = 200;
        }

        assert_eq!(vec.get(1), Some(&200));
        assert!(vec.is_updated());
        assert_eq!(vec.indices.len(), 0);
        assert!(vec.all_updated());

        // Check if the element is marked as updated
        assert_eq!(vec.get_updated(1), Some(&200));

        // Modify another element
        if let Some(val) = vec.get_mut(0) {
            *val = 100;
        }

        assert_eq!(vec.get(0), Some(&100));
        assert_eq!(vec.indices.len(), 0);
        assert!(vec.all_updated());
    }

    #[test]
    fn test_iter_updated() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        // Modify an element
        if let Some(val) = vec.get_mut(1) {
            *val = 20;
        }

        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert_eq!(updated.len(), 3);
        assert_eq!(updated, vec![(0, 1), (1, 20), (2, 3)]);

        // Reset and check
        vec.reset()?;
        assert!(!vec.is_updated());
        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert!(updated.is_empty());

        // Modify an element
        if let Some(val) = vec.get_mut(1) {
            *val = 200;
        }
        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert_eq!(updated.len(), 1);
        assert_eq!(updated, vec![(1, 200)]);
        Ok(())
    }

    #[test]
    fn test_all_updated() {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        // Mark all as updated using iter_mut
        for val in vec.iter_mut() {
            *val += 10;
        }

        assert!(vec.all_updated());
        assert_eq!(vec.indices.len(), 0);
        assert_eq!(vec.update_flags.len(), 0);

        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert_eq!(updated.len(), 3);
        assert_eq!(updated, vec![(0, 11), (1, 12), (2, 13)]);
    }

    #[test]
    fn test_reset() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        assert!(vec.is_updated());

        vec.reset()?;
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());
        assert_eq!(vec.indices.len(), 0);
        assert_eq!(vec.update_flags.len(), 3);

        // After reset, get_updated should return None
        assert_eq!(vec.get_updated(0), None);

        // Modify an element
        if let Some(val) = vec.get_mut(1) {
            *val = 20;
        }

        assert!(vec.is_updated());
        assert!(!vec.all_updated());
        assert_eq!(vec.get_updated(1), Some(&20));
        Ok(())
    }

    #[test]
    fn test_clear() {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        vec.clear();
        assert_eq!(vec.len(), 0);
        assert!(vec.is_empty());
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());
    }

    #[test]
    fn test_pop() {
        let mut vec = Vector::new();
        vec.push(10);
        vec.push(20);
        vec.push(30);

        let val = vec.pop();
        assert_eq!(val, Some(30));
        assert_eq!(vec.len(), 2);
        assert!(vec.is_updated());
        assert_eq!(vec.indices.len(), 0);
        assert!(vec.all_updated());

        // The removed index should not affect indices since they are cleared
        // Pop until empty
        vec.pop();
        vec.pop();
        assert!(vec.is_empty());
        assert!(!vec.is_updated());
    }

    #[test]
    fn test_get_out_of_bounds() {
        let mut vec = Vector::new();
        vec.push(1);

        assert_eq!(vec.get(1), None);
        assert_eq!(vec.get_mut(1), None);
        assert_eq!(vec.get_updated(1), None);
    }

    #[test]
    fn test_iter() {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        let collected: Vec<&i32> = vec.iter().collect();
        assert_eq!(collected, vec![&1, &2, &3]);

        for val in vec.iter_mut() {
            *val *= 2;
        }

        let collected: Vec<&i32> = vec.iter().collect();
        assert_eq!(collected, vec![&2, &4, &6]);
    }

    #[test]
    fn test_push_after_all_updated() {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);

        // Mark all as updated
        vec.iter_mut().for_each(|_| {});

        // Now push a new element
        vec.push(3);
        assert!(vec.all_updated());

        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert_eq!(updated, vec![(0, 1), (1, 2), (2, 3)]);
    }

    #[test]
    fn test_multiple_updates() {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        // Update index 0
        if let Some(v) = vec.get_mut(0) {
            *v = 10;
        }

        assert_eq!(vec.indices.len(), 0);
        assert!(vec.all_updated());

        // Update index 1
        if let Some(v) = vec.get_mut(1) {
            *v = 20;
        }
        assert_eq!(vec.indices.len(), 0);

        // Update index 2
        if let Some(v) = vec.get_mut(2) {
            *v = 30
        }
        // All elements are updated
        assert!(vec.all_updated());
        assert_eq!(vec.indices.len(), 0);

        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert_eq!(updated, vec![(0, 10), (1, 20), (2, 30)]);
    }
    #[test]
    fn test_reset_after_all_updated() -> Result<(), Error> {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);

        // Mark all as updated
        vec.iter_mut().for_each(|v| *v += 1);

        assert!(vec.all_updated());

        // Reset
        vec.reset()?;
        assert!(!vec.is_updated());
        assert!(!vec.all_updated());

        // Check that no elements are updated
        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert!(updated.is_empty());

        // Modify one element
        if let Some(v) = vec.get_mut(0) {
            *v = 10;
        }
        assert!(vec.is_updated());
        assert!(!vec.all_updated());

        let updated: Vec<(usize, i32)> = vec.iter_updated().map(|(i, &v)| (i, v)).collect();
        assert_eq!(updated, vec![(0, 10)]);
        Ok(())
    }

    #[test]
    fn test_push_and_get() {
        let mut vec = Vector::new();
        vec.push(1);
        vec.push(2);
        vec.push(3);

        assert_eq!(vec.len(), 3);
        assert_eq!(vec.get(0), Some(&1));
        assert_eq!(vec.get(1), Some(&2));
        assert_eq!(vec.get(2), Some(&3));
        assert_eq!(vec.get(3), None);

        assert!(vec.is_updated());
        assert!(vec.all_updated());

        // Test get_updated
        assert_eq!(vec.get_updated(0), Some(&1));
        assert_eq!(vec.get_updated(1), Some(&2));
        assert_eq!(vec.get_updated(2), Some(&3));
        assert_eq!(vec.get_updated(3), None);
    }
}
