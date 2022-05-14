use serde::Serialize;
use std::{cell::Cell, fmt, num::NonZeroU32};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Uid(NonZeroU32);

impl fmt::Display for Uid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "id-{}", self.0)
    }
}

impl Serialize for Uid {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

pub(crate) struct UidGenerator {
    counter: Cell<NonZeroU32>,
}

impl UidGenerator {
    pub fn new() -> Self {
        Self {
            counter: Cell::new(NonZeroU32::new(1).unwrap()),
        }
    }

    pub fn new_uid(&self) -> Uid {
        let counter = self.counter.get();
        self.counter
            .set(NonZeroU32::new(counter.get() + 1).expect("ran out of UIDs"));
        Uid(counter)
    }
}
