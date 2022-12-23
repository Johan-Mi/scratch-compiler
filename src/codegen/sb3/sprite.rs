use super::{Mangled, SerCtx};
use crate::{
    asset::Asset,
    diagnostic::{Error, Result},
    ir::{expr::Expr, proc::CustomProcedure, sprite::Sprite},
};
use serde_json::{json, Value as Json};
use std::collections::HashMap;

impl SerCtx {
    pub fn serialize_sprite(
        &mut self,
        name: &str,
        sprite: &Sprite,
    ) -> Result<Json> {
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
            .map(|(name, proc)| match &**name {
                "when-flag-clicked" | "when-cloned" | "when-received" => {
                    Ok(None)
                }
                _ => {
                    assert_eq!(
                        1,
                        proc.len(),
                        "duplicate definition of custom procdeure `{name}`"
                    );
                    let params = proc[0]
                        .params
                        .iter()
                        .map(|(param, span)| match param {
                            Expr::Sym(sym, ..) => {
                                Ok((sym.clone(), self.new_uid()))
                            }
                            _ => Err(Box::new(
                                Error::InvalidParameterForCustomProcDef {
                                    span: *span,
                                },
                            )),
                        })
                        .collect::<Result<_>>()?;
                    Ok(Some((name.into(), CustomProcedure { params })))
                }
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;

        let blocks = self.serialize_procs(&sprite.procedures)?;

        Ok(json!({
            "name": name,
            "isStage": name == "Stage",
            "variables": var_initializers,
            "lists": list_initializers,
            "costumes": costumes,
            "currentCostume": 1,
            "sounds": [],
            "blocks": blocks,
        }))
    }
}
