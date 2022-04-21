pub(crate) mod expr;
pub(crate) mod proc;
pub(crate) mod sprite;

use crate::{ast::Ast, ir::sprite::Sprite};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
pub(crate) struct Program {
    stage: Sprite,
    sprites: HashMap<String, Sprite>,
}

impl Program {
    pub fn from_asts(asts: Vec<Ast>) -> Program {
        // TODO: Error handling
        let mut sprites = HashMap::<String, Sprite>::new();

        for ast in asts {
            let (name, sprite) = Sprite::from_ast(ast);
            match sprites.entry(name) {
                Entry::Occupied(mut merging_existing_sprite) => {
                    merging_existing_sprite.get_mut().merge(sprite);
                }
                Entry::Vacant(new_sprite) => {
                    new_sprite.insert(sprite);
                }
            }
        }

        let stage = sprites.remove("Stage").unwrap();
        Program { stage, sprites }
    }

    pub fn optimize(&mut self) {
        self.stage.optimize();
        for sprite in self.sprites.values_mut() {
            sprite.optimize();
        }
    }
}
