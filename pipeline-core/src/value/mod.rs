use crate::{Error, Reset};

pub mod buckets;
pub mod vector;
pub use buckets::Buckets;
pub use vector::Vector;

#[derive(Default, Debug, PartialEq)]
pub enum State<T> {
    #[default]
    Uninitialised,
    Value(T),
    Updated(T),
}

#[derive(Default)]
pub struct Value<T> {
    state: State<T>,
}

impl<T> Value<T> {
    pub fn new() -> Self {
        Value {
            state: State::Uninitialised,
        }
    }

    pub fn set(&mut self, value: T) {
        self.state = State::Updated(value);
    }

    pub fn get(&self) -> Result<&T, Error> {
        match &self.state {
            State::Value(v) | State::Updated(v) => Ok(v),
            State::Uninitialised => Err(Error::UninitialisedValue),
        }
    }

    pub fn get_mut(&mut self) -> Result<&mut T, Error> {
        match &mut self.state {
            State::Value(v) | State::Updated(v) => Ok(v),
            State::Uninitialised => Err(Error::UninitialisedValue),
        }
    }

    pub fn touch(&mut self) {
        if let State::Value(_) = self.state
            && let State::Value(v) = std::mem::replace(&mut self.state, State::Uninitialised)
        {
            self.state = State::Updated(v);
        }
    }

    pub fn is_updated(&self) -> bool {
        matches!(self.state, State::Updated(_))
    }

    pub fn has_value(&self) -> bool {
        !matches!(self.state, State::Uninitialised)
    }

    /// Option-shaped alias for [`Value::get`] / [`Value::has_value`] that
    /// mirrors [`crate::value::vector::Vector::get_valid`]. Returns `Some`
    /// iff the value is currently valid (state is `Value(_)` or
    /// `Updated(_)`), `None` if uninitialised or explicitly invalidated.
    pub fn get_valid(&self) -> Option<&T> {
        match &self.state {
            State::Value(v) | State::Updated(v) => Some(v),
            State::Uninitialised => None,
        }
    }

    /// Mirror of [`Value::has_value`] under the validity-aware naming used
    /// by [`crate::value::vector::Vector::is_valid`]. Returns `true` iff the
    /// state is `Value(_)` or `Updated(_)`.
    pub fn is_valid(&self) -> bool {
        self.has_value()
    }

    /// Explicit invalidation: drop the stored value and return the `Value`
    /// to the `Uninitialised` state. Mirrors
    /// [`crate::value::vector::Vector::invalidate`]. Producers should call
    /// this when an upstream input is bad and they want downstream readers
    /// to see "no fresh / valid data" instead of held-last-cycle data.
    pub fn invalidate(&mut self) {
        self.state = State::Uninitialised;
    }
}

impl<T> Reset for Value<T> {
    type Error = Error;
    fn reset(&mut self) -> Result<(), Error> {
        if let State::Updated(_) = self.state
            && let State::Updated(v) = std::mem::replace(&mut self.state, State::Uninitialised)
        {
            self.state = State::Value(v);
        }
        Ok(())
    }
}
impl<T: PartialEq> PartialEq<T> for Value<T> {
    fn eq(&self, other: &T) -> bool {
        match &self.state {
            State::Uninitialised => false,
            State::Value(v) | State::Updated(v) => v.eq(other),
        }
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

    #[test]
    fn invalidate_on_uninitialised_is_idempotent() {
        let mut value: Value<i32> = Value::new();
        value.invalidate();
        value.invalidate();
        assert!(!value.is_valid());
    }
}
