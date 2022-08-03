use crate::uid::Uid;
use sb3_stuff::Value;
use serde_json::{json, Value as Json};

pub struct Reporter {
    json: Json,
    shape: Shape,
}

impl Reporter {
    pub fn from_uid(uid: Uid) -> Self {
        Self {
            json: json!(uid),
            shape: Shape::NonShadow,
        }
    }

    pub fn from_lit(lit: &Value) -> Self {
        Self::shadow(json!([10, lit.to_cow_str()]))
    }

    pub const fn shadow(json: Json) -> Self {
        Self {
            json,
            shape: Shape::Shadow,
        }
    }

    pub const fn non_shadow(json: Json) -> Self {
        Self {
            json,
            shape: Shape::NonShadow,
        }
    }

    pub fn with_empty_shadow(&self) -> Json {
        match self.shape {
            Shape::Shadow => json!([1, self.json]),
            Shape::NonShadow => json!([3, self.json, [10, ""]]),
        }
    }

    pub fn without_shadow(&self) -> Json {
        match self.shape {
            Shape::Shadow => json!([1, self.json]),
            Shape::NonShadow => json!([2, self.json]),
        }
    }
}

enum Shape {
    Shadow,
    NonShadow,
}
