use crate::{Error, Reset};

pub mod vector;
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
}

impl<T> Reset for Value<T> {
    type Error = Error;
    fn clear_updated(&mut self) -> Result<(), Error> {
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

        assert!(value.clear_updated().is_ok());
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
        assert_eq!(value.clear_updated(), Ok(()));
        assert!(!value.has_value());
    }
}
