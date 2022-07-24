use super::{Mangled, SerCtx};
use crate::{
    asset::Asset,
    ir::{expr::Expr, proc::CustomProcedure, sprite::Sprite},
};
use serde_json::{json, Value as Json};
use std::collections::HashMap;

impl SerCtx {
    pub(super) fn serialize_sprite(
        &mut self,
        name: &str,
        sprite: &Sprite,
    ) -> Json {
        let variables = sprite
            .variables
            .iter()
            .map(|var| {
                (
                    var.into(),
                    Mangled {
                        name: var.clone(),
                        id: self.new_uid(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        let lists = sprite
            .lists
            .iter()
            .map(|lst| {
                (
                    lst.into(),
                    Mangled {
                        name: lst.clone(),
                        id: self.new_uid(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        let var_initializers = variables
            .iter()
            .map(|(name, mangled)| (mangled.id.to_string(), json!([name, 0])))
            .collect::<Json>();
        let list_initializers = lists
            .iter()
            .map(|(name, mangled)| (mangled.id.to_string(), json!([name, []])))
            .collect::<Json>();

        if name != "Stage" {
            // Variables and lists belonging to the stage are considered global,
            // so excluding them here prevents them from being defined twice.
            self.sprite_vars = variables;
            self.sprite_lists = lists;
        }

        let costumes = sprite
            .costumes
            .iter()
            .map(|(name, path)| {
                serde_json::to_value(Asset::new(name, path)).unwrap()
            })
            .collect::<Vec<_>>();

        self.custom_procs = sprite
            .procedures
            .iter()
            .filter_map(|(name, proc)| match &**name {
                "when-flag-clicked" | "when-cloned" | "when-received" => None,
                _ => Some((
                    name.into(),
                    CustomProcedure {
                        params: proc
                            .params
                            .iter()
                            .map(|param| match param {
                                Expr::Sym(sym) => (sym.clone(), self.new_uid()),
                                _ => todo!(
                                    "invalid parameter to custom\
                                    procedure definition:\n{param:#?}"
                                ),
                            })
                            .collect(),
                    },
                )),
            })
            .collect();

        let blocks = self.serialize_procs(&sprite.procedures);

        json!({
            "name": name,
            "isStage": name == "Stage",
            "variables": var_initializers,
            "lists": list_initializers,
            "costumes": costumes,
            "currentCostume": 1,
            "sounds": [],
            "blocks": blocks,
        })
    }
}
