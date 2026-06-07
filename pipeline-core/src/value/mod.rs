//! Single-value cell ([`Value`]) with dirty/validity tracking, plus the
//! [`Vector`] and [`Buckets`] containers.

use crate::{Error, Reset};

pub mod buckets;
pub mod vector;
pub use buckets::Buckets;
pub use vector::Vector;

/// A single dirty/validity-tracked value — the scalar analogue of one
/// [`Vector`] slot. State is **two orthogonal bits**, exactly like a `Vector`
/// slot's separate validity + dirty bitsets:
///
/// - **valid** — does it currently hold a value? (`value.is_some()`)
/// - **dirty** — did it change *this cycle* (written **or** invalidated)?
///
/// The initial state is simply *invalid + clean*; there is no distinct
/// "uninitialised" concept (that's also where a cell lands after `invalidate`
/// then [`Reset`]). A valid→invalid transition sets the dirty bit, so the
/// "became invalid" signal propagates just like a write — exactly as
/// [`Vector::invalidate`] marks a slot dirty. [`Reset`] clears the dirty bit;
/// validity persists.
#[derive(Default)]
pub struct Value<T> {
    value: Option<T>,
    dirty: bool,
}

impl<T> Value<T> {
    /// Create an empty (invalid, clean) cell.
    pub fn new() -> Self {
        Value {
            value: None,
            dirty: false,
        }
    }

    /// Store `value` and mark the cell dirty this cycle.
    pub fn set(&mut self, value: T) {
        self.value = Some(value);
        self.dirty = true;
    }

    /// Borrow the value, or `Err(UninitialisedValue)` if the cell is invalid.
    pub fn get(&self) -> Result<&T, Error> {
        self.value.as_ref().ok_or(Error::UninitialisedValue)
    }

    /// Mutably borrow the value, or `Err(UninitialisedValue)` if invalid.
    pub fn get_mut(&mut self) -> Result<&mut T, Error> {
        self.value.as_mut().ok_or(Error::UninitialisedValue)
    }

    /// Re-mark an existing value as dirty without changing it. No-op if the
    /// cell is invalid.
    pub fn touch(&mut self) {
        if self.value.is_some() {
            self.dirty = true;
        }
    }

    /// Whether the cell changed this cycle — written **or** invalidated. This
    /// is the dirty bit the pipeline engines use, so a valid→invalid transition
    /// counts as a change.
    pub fn is_updated(&self) -> bool {
        self.dirty
    }

    /// Whether the cell currently holds a value.
    pub fn has_value(&self) -> bool {
        self.value.is_some()
    }

    /// Option-shaped accessor mirroring
    /// [`crate::value::vector::Vector::get_valid`]: `Some` iff the cell
    /// currently holds a value, else `None`.
    pub fn get_valid(&self) -> Option<&T> {
        self.value.as_ref()
    }

    /// Mirror of [`Value::has_value`] under the validity-aware naming used by
    /// [`crate::value::vector::Vector::is_valid`].
    pub fn is_valid(&self) -> bool {
        self.value.is_some()
    }

    /// Explicit invalidation: drop the stored value. A valid→invalid transition
    /// is a **change**, so it marks the cell dirty this cycle — mirroring
    /// [`crate::value::vector::Vector::invalidate`]. This lets the "became
    /// invalid" signal propagate to readers that schedule on dirtiness, instead
    /// of looking unchanged. Invalidating an already-empty cell is a no-op (no
    /// spurious dirty). Producers call this when an upstream input is bad and
    /// they want downstream readers to see "no fresh / valid data" instead of
    /// held-last-cycle data.
    pub fn invalidate(&mut self) {
        if self.value.take().is_some() {
            self.dirty = true;
        }
    }
}

impl<T> Reset for Value<T> {
    type Error = Error;
    fn reset(&mut self) -> Result<(), Error> {
        // Clear the per-cycle dirty bit; validity (the held value) persists.
        self.dirty = false;
        Ok(())
    }
}
impl<T: PartialEq> PartialEq<T> for Value<T> {
    fn eq(&self, other: &T) -> bool {
        self.value.as_ref().is_some_and(|v| v == other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initial_state() {
        let value: Value<i32> = Value::new();
        assert!(!value.has_value());
    }

    #[test]
    fn test_set_and_get() {
        let mut value = Value::new();
        value.set(42);
        assert_eq!(value.get(), Ok(&42));
    }

    #[test]
    fn test_touch() {
        let mut value = Value::new();
        value.set(42);
        value.touch();
        assert!(value.is_updated());
    }

    #[test]
    fn test_reset() {
        let mut value = Value::new();
        assert!(!value.has_value());
        value.set(42);
        assert!(value.has_value());
        assert!(value.is_updated());
        assert_eq!(*value.get().unwrap(), 42);

        assert!(value.reset().is_ok());
        assert!(!value.is_updated());
        assert_eq!(*value.get().unwrap(), 42);

        value.touch();
        assert!(value.is_updated());
        assert_eq!(*value.get().unwrap(), 42);
    }

    #[test]
    fn test_uninitialised_get() {
        let value: Value<i32> = Value::new();
        assert_eq!(value.get(), Err(Error::UninitialisedValue));
    }

    #[test]
    fn test_reset_uninitialised() {
        let mut value: Value<i32> = Value::new();
        assert_eq!(value.reset(), Ok(()));
        assert!(!value.has_value());
    }

    #[test]
    fn get_valid_returns_some_when_set_or_updated() {
        let mut value: Value<i32> = Value::new();
        assert_eq!(value.get_valid(), None);
        assert!(!value.is_valid());

        value.set(7);
        assert_eq!(value.get_valid(), Some(&7));
        assert!(value.is_valid());
        assert!(value.is_updated());

        value.reset().unwrap();
        // After reset the value is no longer dirty but still valid.
        assert_eq!(value.get_valid(), Some(&7));
        assert!(value.is_valid());
        assert!(!value.is_updated());
    }

    #[test]
    fn invalidate_drops_value_to_uninitialised() {
        let mut value: Value<i32> = Value::new();
        value.set(42);
        assert!(value.is_valid());

        value.invalidate();
        assert!(!value.is_valid());
        assert_eq!(value.get_valid(), None);
        assert_eq!(value.get(), Err(Error::UninitialisedValue));

        // After invalidate, a fresh set works normally.
        value.set(99);
        assert_eq!(value.get_valid(), Some(&99));
        assert!(value.is_updated());
    }

    /// Mirrors `Vector::validity_invalidate_marks_dirty_and_clears_valid`: a
    /// valid->invalid transition is a change, so it sets the dirty bit and the
    /// signal survives until `reset`.
    #[test]
    fn invalidate_marks_dirty_and_clears_valid() {
        let mut value: Value<i32> = Value::new();
        value.set(42);
        value.reset().unwrap(); // valid but clean (Value state)
        assert!(value.is_valid());
        assert!(!value.is_updated());

        value.invalidate();
        assert!(!value.is_valid()); // no longer holds a value
        assert!(value.is_updated()); // ...but dirty this cycle (Invalidated)
        assert_eq!(value.get_valid(), None);

        // reset settles the invalidation to a clean, empty cell.
        value.reset().unwrap();
        assert!(!value.is_valid());
        assert!(!value.is_updated());
    }

    #[test]
    fn invalidate_on_uninitialised_is_idempotent_and_clean() {
        let mut value: Value<i32> = Value::new();
        value.invalidate();
        value.invalidate();
        assert!(!value.is_valid());
        // Invalidating an already-empty cell is not a change -> not dirty.
        assert!(!value.is_updated());
    }
}
