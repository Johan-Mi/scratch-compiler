use serde::Serialize;
use std::{cell::Cell, fmt, num::NonZeroU32};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Uid(NonZeroU32);

impl fmt::Display for Uid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "id_{}", self.0)
    }
}

impl Serialize for Uid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_str(self)
    }
}

pub struct Generator {
    counter: Cell<NonZeroU32>,
}

impl Generator {
    pub fn new_uid(&self) -> Uid {
        let counter = self.counter.get();
        self.counter
            .set(counter.checked_add(1).expect("ran out of UIDs"));
        Uid(counter)
    }
}

impl Default for Generator {
    fn default() -> Self {
        Self {
            counter: Cell::new(NonZeroU32::MIN),
        }
    }
}
