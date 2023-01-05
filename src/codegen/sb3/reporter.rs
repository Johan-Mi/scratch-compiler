use crate::{codegen::sb3::Mangled, uid::Uid};
use sb3_stuff::Value;
use serde_json::{json, Value as Json};

pub(super) enum Reporter<'a> {
    Literal(Value),
    Variable(Mangled<'a>),
    List(Mangled<'a>),
    Block(Uid),
}

impl Reporter<'_> {
    pub fn with_empty_shadow(&self) -> Json {
        let json = self.inner_json();
        if self.is_shadow() {
            json!([1, json])
        } else {
            json!([3, json, [10, ""]])
        }
    }

    pub fn without_shadow(&self) -> Json {
        let json = self.inner_json();
        if self.is_shadow() {
            json!([1, json])
        } else {
            json!([2, json])
        }
    }

    const fn is_shadow(&self) -> bool {
        matches!(self, Self::Literal(_))
    }

    fn inner_json(&self) -> Json {
        match self {
            Self::Literal(lit) => json!([10, lit.to_cow_str()]),
            Self::Variable(var) => json!([12, var.name, var.id]),
            Self::List(list) => json!([13, list.name, list.id]),
            Self::Block(block_id) => json!(block_id),
        }
    }
}
