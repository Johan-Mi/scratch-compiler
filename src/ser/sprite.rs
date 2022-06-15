use super::{Mangled, SerCtx};
use crate::{
    ir::{expr::Expr, proc::CustomProcedure, sprite::Sprite},
    ser::asset_json,
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
                    var.clone(),
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
                    lst.clone(),
                    Mangled {
                        name: lst.clone(),
                        id: self.new_uid(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        let costumes = sprite
            .costumes
            .iter()
            .map(|(name, path)| asset_json(name, path))
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
            "variables": variables.values().map(|var| (&var.name, 0)).collect::<Vec<_>>(),
            "lists": lists.values().map(|var| (&var.name, [(); 0])).collect::<Vec<_>>(),
            "costumes": costumes,
            "currentCostume": 1,
            "sounds": [],
            "blocks": blocks,
        })
    }
}
