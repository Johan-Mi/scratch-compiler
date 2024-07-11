use super::{Mangled, SerCtx};
use crate::{
    asset::Asset,
    diagnostic::{Error, Result},
    ir::{expr::Expr, proc::CustomProcedure, sprite::Sprite},
};
use serde_json::{json, Value as Json};
use std::{borrow::Cow, collections::HashMap};

impl<'a> SerCtx<'a> {
    pub fn serialize_sprite(
        &mut self,
        name: &str,
        sprite: &'a Sprite,
    ) -> Result<Json> {
        let variables = sprite
            .variables
            .iter()
            .map(|var| {
                (
                    &**var,
                    Mangled {
                        name: Cow::Borrowed(var),
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
                    &**lst,
                    Mangled {
                        name: Cow::Borrowed(lst),
                        id: self.new_uid(),
                    },
                )
            })
            .collect::<HashMap<_, _>>();

        let mangled_var = |mangled: &Mangled| {
            (mangled.id.to_string(), json!([mangled.name, 0]))
        };
        let mangled_list = |mangled: &Mangled| {
            (mangled.id.to_string(), json!([mangled.name, []]))
        };

        let mut var_initializers =
            variables.values().map(mangled_var).collect::<Json>();
        let mut list_initializers =
            lists.values().map(mangled_list).collect::<Json>();

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
                serde_json::to_value(Asset::new(name.to_owned(), path)).unwrap()
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
                            _ => Err(Error::InvalidParameterForCustomProcDef {
                                span: *span,
                            }),
                        })
                        .collect::<std::result::Result<_, _>>()?;
                    Ok(Some((&**name, CustomProcedure { params })))
                }
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;

        let procs = self.serialize_procs(&sprite.procedures)?;
        var_initializers
            .as_object_mut()
            .unwrap()
            .extend(procs.local_vars.iter().map(mangled_var));
        list_initializers
            .as_object_mut()
            .unwrap()
            .extend(procs.local_lists.iter().map(mangled_list));

        Ok(json!({
            "name": name,
            "isStage": name == "Stage",
            "variables": var_initializers,
            "lists": list_initializers,
            "costumes": costumes,
            "currentCostume": 1,
            "sounds": [],
            "blocks": procs.blocks,
        }))
    }
}
