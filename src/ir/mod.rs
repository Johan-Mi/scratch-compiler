mod expr;
mod proc;
mod sprite;

use crate::{ast::Ast, ir::sprite::Sprite};
use std::collections::HashMap;

#[derive(Debug)]
pub(crate) struct Program {
    stage: Sprite,
    sprites: HashMap<String, Sprite>,
}

impl Program {
    pub fn from_asts(asts: Vec<Ast>) -> Program {
        // TODO: Error handling
        let mut sprites = HashMap::new();

        for ast in asts {
            let (name, sprite) = Sprite::from_ast(ast);
            // TODO: Sprite merging
            sprites.insert(name, sprite);
        }

        let stage = sprites.remove("Stage").unwrap();
        Program { stage, sprites }
    }
}
