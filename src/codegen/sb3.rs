mod expr;
mod reporter;
mod sprite;
mod statement;

use crate::{
    asset::Asset,
    diagnostic::{Error, Result},
    ir::{
        expr::Expr,
        proc::CustomProcedure,
        Program,
        {proc::Procedure, statement::Statement},
    },
    span::Span,
    uid::Uid,
};
use reporter::Reporter;
use sb3_stuff::Value;
use serde_json::{json, Value as Json};
use smol_str::SmolStr;
use std::{
    cell::RefCell,
    collections::HashMap,
    fs::{self, File},
    io::{self, Cursor},
    iter,
    path::Path,
};
use zip::{write::FileOptions, ZipWriter};

pub fn write_sb3_file(program: &Program, path: &Path) -> Result<()> {
    // TODO: Error handling
    let mut zip = ZipWriter::new(Cursor::new(Vec::new()));
    zip.start_file("project.json", FileOptions::default())
        .map_err(|err| Error::CouldNotCreateProjectJson { inner: err })?;

    let uid_gen = crate::uid::Generator::default();

    let global_vars = program
        .stage
        .variables
        .iter()
        .map(|var| {
            (
                var.into(),
                Mangled {
                    name: var.clone(),
                    id: uid_gen.new_uid(),
                },
            )
        })
        .collect::<HashMap<_, _>>();
    let global_lists = program
        .stage
        .lists
        .iter()
        .map(|list| {
            (
                list.into(),
                Mangled {
                    name: list.clone(),
                    id: uid_gen.new_uid(),
                },
            )
        })
        .collect::<HashMap<_, _>>();

    let mut ctx = SerCtx {
        uid_gen,
        blocks: RefCell::default(),
        custom_procs: HashMap::new(),
        proc_args: Vec::new(),
        local_vars: HashMap::new(),
        local_lists: HashMap::new(),
        sprite_vars: HashMap::new(),
        sprite_lists: HashMap::new(),
        global_vars,
        global_lists,
    };
    let targets = iter::once(("Stage", &program.stage))
        .chain(program.sprites.iter().map(|(name, spr)| (&**name, spr)))
        .map(|(name, spr)| ctx.serialize_sprite(name, spr))
        .collect::<Result<Vec<_>>>()?;

    serde_json::to_writer(
        &mut zip,
        &json!({
            "meta": {
                "semver": "3.0.0",
            },
            "targets": targets,
        }),
    )
    .unwrap();

    for (name, path) in program
        .sprites
        .values()
        .chain(iter::once(&program.stage))
        .flat_map(|sprite| &sprite.costumes)
    {
        let asset = Asset::new(name, path);
        let mut file = File::open(path).unwrap();
        zip.start_file(asset.md5ext, FileOptions::default())
            .unwrap();
        io::copy(&mut file, &mut zip).unwrap();
    }

    let buf = zip
        .finish()
        .map_err(|err| Error::CouldNotFinishZip { inner: err })?;
    fs::write(path, buf.into_inner())
        .map_err(|err| Error::CouldNotCreateSb3File { inner: err })?;

    Ok(())
}

struct SerCtx {
    uid_gen: crate::uid::Generator,
    blocks: RefCell<HashMap<Uid, Json>>,
    custom_procs: HashMap<SmolStr, CustomProcedure>,
    proc_args: Vec<SmolStr>,
    local_vars: HashMap<SmolStr, Mangled>,
    local_lists: HashMap<SmolStr, Mangled>,
    sprite_vars: HashMap<SmolStr, Mangled>,
    sprite_lists: HashMap<SmolStr, Mangled>,
    global_vars: HashMap<SmolStr, Mangled>,
    global_lists: HashMap<SmolStr, Mangled>,
}

impl SerCtx {
    fn new_uid(&self) -> Uid {
        self.uid_gen.new_uid()
    }

    pub fn serialize_procs(
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
                let [(Expr::Lit(Value::String(broadcast_name)), _)] =
                    &proc.params[..]
                else {
                    todo!();
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
                    .map(|(param, _)| match param {
                        Expr::Sym(sym, ..) => sym.clone(),
                        // This check is already performed when setting
                        // `self.proc_args`
                        _ => unreachable!(),
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
                let reporters = proc.params.iter().map(|(param, _)| {
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
            let var =
                self.lookup_var(var_name).ok_or_else(|| Error::UnknownVar {
                    span,
                    var_name: var_name.into(),
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
        let inputs = resolve_input_field_fns(inputs, this)?;
        let fields = resolve_input_field_fns(fields, this)?;

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "parent": parent,
                "inputs": inputs,
                "fields": fields,
            }),
        );

        Ok(Reporter::Block(this))
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
        let inputs = resolve_input_field_fns(inputs, this)?;
        let fields = resolve_input_field_fns(fields, this)?;

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

    fn create_inputs_and_fields(
        &self,
        params: &[Param],
        args: &[Expr],
        parent: Uid,
    ) -> Result<(Json, Json)> {
        let inputs = params
            .iter()
            .zip(args)
            .map(|(param, arg)| {
                Ok(match param {
                    Param::String(param_name) | Param::Number(param_name) => {
                        Some((
                            *param_name,
                            self.serialize_expr(arg, parent)?
                                .with_empty_shadow(),
                        ))
                    }
                    Param::Bool(param_name) => Some((
                        *param_name,
                        self.serialize_expr(arg, parent)?.without_shadow(),
                    )),
                    _ => None,
                })
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;
        let fields = params
            .iter()
            .zip(args)
            .map(|(param, arg)| {
                Ok(match param {
                    Param::Var(param_name) => {
                        let (var_name, span) = match arg {
                            Expr::Sym(sym, span) => (sym, *span),
                            _ => todo!(),
                        };
                        let var =
                            self.lookup_var(var_name).ok_or_else(|| {
                                Error::UnknownVar {
                                    span,
                                    var_name: var_name.clone(),
                                }
                            })?;
                        Some((*param_name, json!([var.name, var.id])))
                    }
                    Param::List(param_name) => {
                        let (list_name, span) = match arg {
                            Expr::Sym(sym, span) => (sym, *span),
                            _ => todo!(),
                        };
                        let list =
                            self.lookup_list(list_name).ok_or_else(|| {
                                Error::UnknownList {
                                    span,
                                    list_name: list_name.clone(),
                                }
                            })?;
                        Some((*param_name, json!([list.name, list.id])))
                    }
                    _ => None,
                })
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;
        Ok((inputs, fields))
    }
}

type InputFieldFns<'a> = &'a [(&'a str, &'a dyn Fn(Uid) -> Result<Json>)];

fn resolve_input_field_fns(fns: InputFieldFns, parent: Uid) -> Result<Json> {
    fns.iter()
        .copied()
        .map(|(name, fun)| Ok((name, fun(parent)?)))
        .collect()
}

enum Param<'a> {
    String(&'a str),
    Number(&'a str),
    Bool(&'a str),
    Var(&'a str),
    List(&'a str),
}

#[derive(Clone, Copy)]
struct Call<'name, 'a> {
    name: &'name str,
    opcode: &'a str,
    parent: Uid,
    args: &'a [Expr],
    span: Span,
}

#[derive(Clone)]
struct Mangled {
    name: String,
    id: Uid,
}
