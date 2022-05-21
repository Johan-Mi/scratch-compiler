use crate::uid::Uid;
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

    pub fn shadow(json: Json) -> Self {
        Self {
            json,
            shape: Shape::Shadow,
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
