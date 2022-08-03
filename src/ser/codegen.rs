mod expr;
mod reporter;
mod statement;

use crate::{
    error::{Error, Result},
    ir::{
        expr::Expr,
        {proc::Procedure, statement::Statement},
    },
    ser::{Mangled, SerCtx},
    span::Span,
    uid::Uid,
};
use reporter::Reporter;
use sb3_stuff::Value;
use serde_json::{json, Value as Json};
use std::collections::HashMap;

impl SerCtx {
    pub(super) fn serialize_procs(
        &mut self,
        procs: &HashMap<String, Vec<Procedure>>,
    ) -> Result<HashMap<Uid, Json>> {
        for (name, procs) in procs {
            for proc in procs {
                self.serialize_proc(name, proc)?;
            }
        }
        Ok(self.blocks.take())
    }

    fn serialize_proc(&mut self, name: &str, proc: &Procedure) -> Result<()> {
        self.local_vars = proc
            .variables
            .iter()
            .map(|name| {
                let id = self.new_uid();
                (
                    name.into(),
                    Mangled {
                        name: format!("local {id} {name}"),
                        id,
                    },
                )
            })
            .collect();
        self.local_lists = proc
            .lists
            .iter()
            .map(|name| {
                let id = self.new_uid();
                (
                    name.into(),
                    Mangled {
                        name: format!("local {id} {name}"),
                        id,
                    },
                )
            })
            .collect();

        let this = self.new_uid();
        match name {
            "when-flag-clicked" => {
                assert!(proc.params.is_empty());
                let (body, _) = self.serialize_stmt(&proc.body, this, None)?;
                self.emit_block(
                    this,
                    json!({
                        "opcode": "event_whenflagclicked",
                        "next": body,
                        "parent": null,
                        "topLevel": true,
                        "x": 0,
                        "y": 0,
                    }),
                );
            }
            "when-cloned" => {
                assert!(proc.params.is_empty());
                let (body, _) = self.serialize_stmt(&proc.body, this, None)?;
                self.emit_block(
                    this,
                    json!({
                        "opcode": "control_start_as_clone",
                        "next": body,
                        "parent": null,
                        "topLevel": true,
                        "x": 0,
                        "y": 0,
                    }),
                );
            }
            "when-received" => {
                let broadcast_name = match &proc.params[..] {
                    [Expr::Lit(Value::String(broadcast_name))] => {
                        broadcast_name
                    }
                    _ => todo!(),
                };
                let (body, _) = self.serialize_stmt(&proc.body, this, None)?;
                self.emit_block(
                    this,
                    json!({
                        "opcode": "event_whenbroadcastreceived",
                        "next": body,
                        "parent": null,
                        "fields": {
                            "BROADCAST_OPTION": [broadcast_name, null],
                        },
                        "topLevel": true,
                        "x": 0,
                        "y": 0,
                    }),
                );
            }
            _ => {
                self.proc_args = proc
                    .params
                    .iter()
                    .map(|param| match param {
                        Expr::Sym(sym, ..) => sym.clone(),
                        _ => todo!(
                            "invalid parameter to custom procedure definition:\
                            \n{param:#?}"
                        ),
                    })
                    .collect::<Vec<_>>();

                let prototype_id = self.new_uid();
                let param_ids: Vec<Uid> = self
                    .custom_procs
                    .get(name)
                    .unwrap()
                    .params
                    .iter()
                    .map(|(_, uid)| *uid)
                    .collect();

                let proccode =
                    format!("{name}{}", " %s".repeat(proc.params.len()));
                let argumentids = serde_json::to_string(&param_ids).unwrap();
                let argumentnames =
                    serde_json::to_string(&self.proc_args).unwrap();
                let argumentdefaults =
                    serde_json::to_string(&[""].repeat(proc.params.len()))
                        .unwrap();
                let reporters = proc.params.iter().map(|param| {
                    Result::Ok(
                        self.serialize_expr(param, this)?.without_shadow(),
                    )
                });
                let inputs: Json = param_ids
                    .iter()
                    .zip(reporters)
                    .map(|(id, rep)| Ok((id.to_string(), rep?)))
                    .collect::<Result<_>>()?;
                let (body, _) = self.serialize_stmt(&proc.body, this, None)?;

                self.emit_block(
                    this,
                    json!({
                        "opcode": "procedures_definition",
                        "next": body,
                        "parent": null,
                        "inputs": {
                            "custom_block": [1, prototype_id],
                        },
                        "topLevel": true,
                        "x": 0,
                        "y": 0,
                    }),
                );
                self.emit_block(
                    prototype_id,
                    json!({
                        "opcode": "procedures_prototype",
                        "next": null,
                        "parent": this,
                        "inputs": inputs,
                        "shadow": true,
                        "mutation": {
                            "tagName": "mutation",
                            "children": [],
                            "proccode": proccode,
                            "argumentids": argumentids,
                            "argumentnames": argumentnames,
                            "argumentdefaults": argumentdefaults,
                            "warp": true,
                        },
                    }),
                );
            }
        }
        Ok(())
    }

    fn stmt_input<'s>(
        &'s self,
        stmt: &'s Statement,
    ) -> impl Fn(Uid) -> Result<Json> + 's {
        |this| Ok(json!([2, self.serialize_stmt(stmt, this, None)?.0]))
    }

    fn empty_shadow_input<'s>(
        &'s self,
        expr: &'s Expr,
    ) -> impl Fn(Uid) -> Result<Json> + 's {
        |this| Ok(self.serialize_expr(expr, this)?.with_empty_shadow())
    }

    fn shadowless_input<'s>(
        &'s self,
        expr: &'s Expr,
    ) -> impl Fn(Uid) -> Result<Json> + 's {
        |this| Ok(self.serialize_expr(expr, this)?.without_shadow())
    }

    fn var_input<'s>(
        &'s self,
        var_name: &'s str,
        span: Span,
    ) -> impl Fn(Uid) -> Result<Json> + 's {
        move |_| {
            let var = self.lookup_var(var_name).ok_or_else(|| {
                Box::new(Error::UnknownVar {
                    span,
                    var_name: var_name.into(),
                })
            })?;
            Ok(json!([var.name, var.id]))
        }
    }

    fn emit_non_shadow(
        &self,
        opcode: &str,
        parent: Uid,
        inputs: InputFieldFns,
        fields: InputFieldFns,
    ) -> Result<Reporter> {
        let this = self.new_uid();

        let inputs: Json = inputs
            .iter()
            .copied()
            .map(|(name, fun)| Ok((name, fun(this)?)))
            .collect::<Result<_>>()?;
        let fields: Json = fields
            .iter()
            .copied()
            .map(|(name, fun)| Ok((name, fun(this)?)))
            .collect::<Result<_>>()?;

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "parent": parent,
                "inputs": inputs,
                "fields": fields,
            }),
        );

        Ok(Reporter::from_uid(this))
    }

    fn emit_stacking(
        &self,
        opcode: &str,
        parent: Uid,
        next: Option<Uid>,
        inputs: InputFieldFns,
        fields: InputFieldFns,
    ) -> Result<(Option<Uid>, Option<Uid>)> {
        let this = self.new_uid();

        let inputs: Json = inputs
            .iter()
            .copied()
            .map(|(name, fun)| Ok((name, fun(this)?)))
            .collect::<Result<_>>()?;
        let fields: Json = fields
            .iter()
            .copied()
            .map(|(name, fun)| Ok((name, fun(this)?)))
            .collect::<Result<_>>()?;

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "next": next,
                "parent": parent,
                "inputs": inputs,
                "fields": fields,
            }),
        );

        Ok((Some(this), Some(this)))
    }

    fn emit_block(&self, uid: Uid, block: Json) {
        self.blocks.borrow_mut().insert(uid, block);
    }

    fn lookup_var(&self, var_name: &str) -> Option<&Mangled> {
        self.local_vars
            .get(var_name)
            .or_else(|| self.sprite_vars.get(var_name))
            .or_else(|| self.global_vars.get(var_name))
    }

    fn lookup_list(&self, list_name: &str) -> Option<&Mangled> {
        self.local_lists
            .get(list_name)
            .or_else(|| self.sprite_lists.get(list_name))
            .or_else(|| self.global_lists.get(list_name))
    }
}

type InputFieldFns<'a> = &'a [(&'a str, &'a dyn Fn(Uid) -> Result<Json>)];

enum Param<'a> {
    String(&'a str),
    Number(&'a str),
    Bool(&'a str),
    Var(&'a str),
    List(&'a str),
}

#[derive(Clone, Copy)]
struct Call<'a> {
    name: &'a str,
    opcode: &'a str,
    parent: Uid,
    args: &'a [Expr],
    span: Span,
}
