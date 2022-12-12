use crate::{codegen::sb3::Mangled, uid::Uid};
use sb3_stuff::Value;
use serde_json::{json, Value as Json};

pub(super) enum Reporter {
    Literal(Value),
    Variable(Mangled),
    List(Mangled),
    Block(Uid),
}

impl Reporter {
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

    fn is_shadow(&self) -> bool {
        matches!(self, Reporter::Literal(_))
    }

    fn inner_json(&self) -> Json {
        match self {
            Reporter::Literal(lit) => json!([10, lit.to_cow_str()]),
            Reporter::Variable(var) => json!([12, var.name, var.id]),
            Reporter::List(list) => json!([13, list.name, list.id]),
            Reporter::Block(block_id) => json!(block_id),
        }
    }
}
