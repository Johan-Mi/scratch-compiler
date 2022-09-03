pub mod expr;
pub mod proc;
pub mod sprite;
pub mod statement;

use crate::{
    ast::Ast,
    diagnostic::{Error, Result},
    ir::sprite::Sprite,
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug)]
pub struct Program {
    pub stage: Sprite,
    pub sprites: HashMap<String, Sprite>,
}

impl Program {
    pub fn from_asts(asts: Vec<Ast>) -> Result<Self> {
        let mut sprites = HashMap::<String, Sprite>::new();

        for ast in asts {
            let (name, sprite) = Sprite::from_ast(ast)?;
            match sprites.entry(name) {
                Entry::Occupied(mut merging_existing_sprite) => {
                    merging_existing_sprite.get_mut().merge(sprite);
                }
                Entry::Vacant(new_sprite) => {
                    new_sprite.insert(sprite);
                }
            }
        }

        let stage = sprites
            .remove("Stage")
            .ok_or_else(|| Box::new(Error::ProgramMissingStage))?;

        Ok(Self { stage, sprites })
    }

    pub fn optimize(&mut self) {
        self.stage.optimize();
        for sprite in self.sprites.values_mut() {
            sprite.optimize();
        }
    }
}
