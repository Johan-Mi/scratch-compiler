use super::{reporter::Reporter, Mangled, SerCtx};
use crate::{
    ir::{
        expr::Expr,
        proc::{Procedure, Statement},
    },
    uid::Uid,
};
use sb3_stuff::Value;
use serde_json::{json, Value as Json};
use std::collections::HashMap;

impl SerCtx {
    pub(super) fn serialize_procs(
        &mut self,
        procs: &HashMap<String, Procedure>,
    ) -> HashMap<Uid, Json> {
        for (name, proc) in procs {
            self.serialize_proc(name, proc);
        }
        self.blocks.take()
    }

    fn serialize_proc(&mut self, name: &str, proc: &Procedure) {
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
                let (body, _) = self.serialize_stmt(&proc.body, this, None);
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
                let (body, _) = self.serialize_stmt(&proc.body, this, None);
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
                let (body, _) = self.serialize_stmt(&proc.body, this, None);
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
                        Expr::Sym(sym) => sym.clone(),
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
                let reporters: Json = proc
                    .params
                    .iter()
                    .map(|param| {
                        self.serialize_expr(param, this).without_shadow()
                    })
                    .collect();

                let (body, _) = self.serialize_stmt(&proc.body, this, None);

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
                    this,
                    json!({
                        "opcode": "procedures_prototype",
                        "next": null,
                        "parent": this,
                        "inputs": reporters,
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
    }

    fn serialize_stmt(
        &self,
        stmt: &Statement,
        parent: Uid,
        next: Option<Uid>,
    ) -> (Option<Uid>, Option<Uid>) {
        match stmt {
            Statement::ProcCall { proc_name, args } => {
                self.serialize_proc_call(proc_name, args, parent, next)
            }
            Statement::Do(stmts) => match &stmts[..] {
                [] => (None, Some(parent)),
                [single] => self.serialize_stmt(single, parent, next),
                stmts => stmts.iter().rfold(
                    (next, None),
                    |(old_start, old_end), stmt| {
                        let (new_start, new_end) =
                            self.serialize_stmt(stmt, parent, old_start);
                        if let Some(old_start) = &old_start {
                            self.blocks
                                .borrow_mut()
                                .get_mut(old_start)
                                .and_then(|block| {
                                    block.as_object_mut()?.insert(
                                        "parent".to_owned(),
                                        json!(new_end),
                                    )
                                })
                                .expect(
                                    "couldn't replace parent in `do` block",
                                );
                        }
                        (new_start.or(old_start), old_end.or(new_end))
                    },
                ),
            },
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let this = self.new_uid();
                let condition =
                    self.serialize_expr(condition, this).without_shadow();
                let (if_true, _) = self.serialize_stmt(if_true, this, None);
                let (if_false, _) = self.serialize_stmt(if_false, this, None);

                let block_json = if if_false.is_some() {
                    json!({
                        "opcode": "control_if_else",
                        "parent": parent,
                        "inputs": {
                            "CONDITION": condition,
                            "SUBSTACK": [2, if_true],
                            "SUBSTACK2": [2, if_false],
                        },
                    })
                } else {
                    json!({
                        "opcode": "control_if",
                        "parent": parent,
                        "inputs": {
                            "CONDITION": condition,
                            "SUBSTACK": [2, if_true],
                        },
                    })
                };

                self.emit_block(this, block_json);

                (Some(this), Some(this))
            }
            Statement::Repeat { times, body } => self.emit_stacking(
                "control_repeat",
                parent,
                next,
                &[
                    ("TIMES", &self.empty_shadow_input(times)),
                    ("SUBSTACK", &self.stmt_input(body)),
                ],
                &[],
            ),
            Statement::Forever(body) => {
                assert!(next.is_none());
                self.emit_stacking(
                    "control_forever",
                    parent,
                    next,
                    &[("SUBSTACK", &self.stmt_input(body))],
                    &[],
                )
            }
            Statement::Until { condition, body } => self.emit_stacking(
                "control_repeat_until",
                parent,
                next,
                &[
                    ("CONDITION", &self.shadowless_input(condition)),
                    ("SUBSTACK", &self.stmt_input(body)),
                ],
                &[],
            ),
            Statement::While { condition, body } => self.emit_stacking(
                "control_while",
                parent,
                next,
                &[
                    ("CONDITION", &self.shadowless_input(condition)),
                    ("SUBSTACK", &self.stmt_input(body)),
                ],
                &[],
            ),
            Statement::For {
                counter,
                times,
                body,
            } => self.emit_stacking(
                "control_for_each",
                parent,
                next,
                &[
                    ("VARIABLE", &self.var_input(counter)),
                    ("VALUE", &self.shadowless_input(times)),
                    ("SUBSTACK", &self.stmt_input(body)),
                ],
                &[],
            ),
        }
    }

    fn serialize_expr(&self, expr: &Expr, parent: Uid) -> Reporter {
        match expr {
            Expr::Lit(lit) => serialize_lit(lit),
            Expr::Sym(sym) => {
                if self.proc_args.contains(sym) {
                    self.emit_non_shadow(
                        "argument_reporter_string_number",
                        parent,
                        &[],
                        &[("VALUE", &|_| json!([sym, null]))],
                    )
                } else if let Some(var) = self.lookup_var(sym) {
                    Reporter::non_shadow(json!([12, var.name, var.id]))
                } else if let Some(list) = self.lookup_list(sym) {
                    Reporter::non_shadow(json!([13, list.name, list.id]))
                } else {
                    todo!("unknown symbol in expression: `{sym}`")
                }
            }
            Expr::FuncCall(func_name, args) => {
                self.serialize_func_call(func_name, args, parent)
            }
        }
    }

    fn serialize_func_call(
        &self,
        func_name: &str,
        args: &[Expr],
        parent: Uid,
    ) -> Reporter {
        macro_rules! func {
            ($opcode:ident(
                $($param_name:ident: $param_type:ident),*
            )) => {
                self.simple_function(
                    stringify!($opcode),
                    &[$(Param::$param_type(stringify!($param_name))),*],
                    parent,
                    args,
                )
            }
        }
        match func_name {
            "+" => self.associative0(
                "operator_add",
                "NUM1",
                "NUM2",
                &Value::Num(0.0),
                args,
                parent,
            ),
            "-" => match args {
                [negation] => self.emit_non_shadow(
                    "operator_subtract",
                    parent,
                    &[
                        ("NUM1", &|_| {
                            serialize_lit(&Value::Num(0.0)).with_empty_shadow()
                        }),
                        ("NUM2", &self.empty_shadow_input(negation)),
                    ],
                    &[],
                ),
                [lhs, rest @ ..] => self.emit_non_shadow(
                    "operator_subtract",
                    parent,
                    &[
                        ("NUM1", &self.empty_shadow_input(lhs)),
                        ("NUM2", &|parent| {
                            self.serialize_func_call("+", rest, parent)
                                .with_empty_shadow()
                        }),
                    ],
                    &[],
                ),
                [] => todo!(),
            },
            "*" => self.associative0(
                "operator_multiply",
                "NUM1",
                "NUM2",
                &Value::Num(1.0),
                args,
                parent,
            ),
            "/" => match args {
                [] | [_] => todo!(),
                [numerator, denominators @ ..] => self.emit_non_shadow(
                    "operator_divide",
                    parent,
                    &[
                        ("NUM1", &self.empty_shadow_input(numerator)),
                        ("NUM2", &|parent| {
                            self.serialize_func_call("+", denominators, parent)
                                .with_empty_shadow()
                        }),
                    ],
                    &[],
                ),
            },
            "!!" => func!(data_itemoflist(LIST: List, INDEX: Number)),
            "++" => self.associative0(
                "operator_join",
                "STRING1",
                "STRING2",
                &Value::String(Default::default()),
                args,
                parent,
            ),
            "and" => self.associative0(
                "operator_and",
                "OPERAND1",
                "OPERAND2",
                &Value::Bool(true),
                args,
                parent,
            ),
            "or" => self.associative0(
                "operator_or",
                "OPERAND1",
                "OPERAND2",
                &Value::Bool(false),
                args,
                parent,
            ),
            "not" => func!(operator_not(OPERAND: Bool)),
            "=" => func!(operator_equals(OPERAND1: String, OPERAND2: String)),
            "<" => func!(operator_lt(OPERAND1: String, OPERAND2: String)),
            ">" => func!(operator_gt(OPERAND1: String, OPERAND2: String)),
            "length" => func!(data_lengthoflist(LIST: List)),
            "str-length" => func!(operator_length(STRING: String)),
            "char-at" => {
                func!(operator_letter_of(STRING: String, LETTER: Number))
            }
            "mod" => func!(operator_mod(NUM1: Number, NUM2: Number)),
            "abs" => todo!(),
            "floor" => todo!(),
            "ceil" => todo!(),
            "sqrt" => todo!(),
            "ln" => todo!(),
            "log" => todo!(),
            "e^" => todo!(),
            "ten^" => todo!(),
            "sin" => todo!(),
            "cos" => todo!(),
            "tan" => todo!(),
            "asin" => todo!(),
            "acos" => todo!(),
            "atan" => todo!(),
            _ => todo!("unknown function `{func_name}`"),
        }
    }

    fn associative0(
        &self,
        opcode: &str,
        lhs_name: &str,
        rhs_name: &str,
        neutral: &Value,
        args: &[Expr],
        parent: Uid,
    ) -> Reporter {
        match args {
            [] => serialize_lit(neutral),
            [single] => self.serialize_expr(single, parent),
            [lhs, rhs @ ..] => self.emit_non_shadow(
                opcode,
                parent,
                &[
                    (lhs_name, &self.empty_shadow_input(lhs)),
                    (rhs_name, &|parent| {
                        self.associative0(
                            opcode, lhs_name, rhs_name, neutral, rhs, parent,
                        )
                        .with_empty_shadow()
                    }),
                ],
                &[],
            ),
        }
    }

    fn serialize_proc_call(
        &self,
        proc_name: &str,
        args: &[Expr],
        parent: Uid,
        next: Option<Uid>,
    ) -> (Option<Uid>, Option<Uid>) {
        macro_rules! proc {
            ($opcode:ident(
                $($param_name:ident: $param_type:ident),*
            )) => {
                self.simple_proc(
                    stringify!($opcode),
                    &[$(Param::$param_type(stringify!($param_name))),*],
                    parent,
                    next,
                    args,
                )
            }
        }

        match proc_name {
            "erase-all" => proc!(pen_clear()),
            "stamp" => proc!(pen_stamp()),
            "pen-down" => proc!(pen_penDown()),
            "pen-up" => proc!(pen_penUp()),
            "set-xy" => proc!(motion_gotoxy(X: Number, Y: Number)),
            "set-size" => proc!(looks_setsizeto(SIZE: Number)),
            "set-costume" => proc!(looks_switchcostumeto(COSTUME: String)),
            "show" => proc!(looks_show()),
            "hide" => proc!(looks_hide()),
            "say" => proc!(looks_say(MESSAGE: String)),
            "change-x" => proc!(motion_changexby(DX: Number)),
            "change-y" => proc!(motion_changeyby(DY: Number)),
            "set-x" => proc!(motion_setx(X: Number)),
            "set-y" => proc!(motion_sety(Y: Number)),
            "wait" => proc!(control_wait(DURATION: Number)),
            "ask" => proc!(sensing_askandwait(QUESTION: String)),
            "send-broadcast-sync" => todo!(),
            ":=" => proc!(data_setvariableto(VARIABLE: Var, VALUE: String)),
            "+=" => proc!(data_changevariableby(VARIABLE: Var, VALUE: String)),
            "replace" => proc!(data_replaceitemoflist(
                LIST: List,
                INDEX: Number,
                ITEM: String
            )),
            "append" => proc!(data_addtolist(LIST: List, ITEM: String)),
            "delete" => proc!(data_deleteoflist(LIST: List, INDEX: Number)),
            "delete-all" => proc!(data_deletealloflist(LIST: List)),
            "stop-all" => todo!(),
            "stop-this-script" => todo!(),
            "stop-other-scripts" => todo!(),
            "clone-myself" => todo!(),
            _ => self.serialize_custom_proc_call(proc_name, args, parent, next),
        }
    }

    fn serialize_custom_proc_call(
        &self,
        proc_name: &str,
        args: &[Expr],
        parent: Uid,
        next: Option<Uid>,
    ) -> (Option<Uid>, Option<Uid>) {
        let proc = self
            .custom_procs
            .get(proc_name)
            .unwrap_or_else(|| todo!("unknown procedure `{proc_name}`"));

        let this = self.new_uid();

        let args: Vec<Json> = args
            .iter()
            .map(|arg| self.serialize_expr(arg, this).with_empty_shadow())
            .collect();

        let proccode =
            format!("{proc_name}{}", " %s".repeat(proc.params.len()));
        let param_ids: Vec<Uid> =
            proc.params.iter().map(|(_, uid)| *uid).collect();
        let argumentids = serde_json::to_string(&param_ids).unwrap();
        let inputs: Json =
            param_ids.iter().map(Uid::to_string).zip(args).collect();

        self.emit_block(
            this,
            json!({
                "opcode": "procedures_call",
                "next": next,
                "parent": parent,
                "inputs": inputs,
                "fields": [],
                "mutation": {
                      "tagName": "mutation",
                      "children": [],
                      "proccode": proccode,
                      "argumentids": argumentids,
                      "warp": "true",
                }
            }),
        );
        (Some(this), Some(this))
    }

    fn simple_function(
        &self,
        opcode: &str,
        params: &[Param],
        parent: Uid,
        args: &[Expr],
    ) -> Reporter {
        let this = self.new_uid();

        assert_eq!(params.len(), args.len());
        let inputs: Json = params
            .iter()
            .zip(args)
            .map(|(param, arg)| match param {
                Param::String(param_name) | Param::Number(param_name) => (
                    *param_name,
                    self.serialize_expr(arg, this).with_empty_shadow(),
                ),
                Param::Bool(param_name) => (
                    *param_name,
                    self.serialize_expr(arg, this).without_shadow(),
                ),
                Param::Var(param_name) => {
                    let var_name = match arg {
                        Expr::Sym(sym) => sym,
                        _ => todo!(),
                    };
                    let var = self.lookup_var(var_name).unwrap_or_else(|| {
                        todo!("variable `{var_name}` does not exist")
                    });
                    (*param_name, json!([var.name, var.id]))
                }
                Param::List(param_name) => {
                    let list_name = match arg {
                        Expr::Sym(sym) => sym,
                        _ => todo!(),
                    };
                    let list =
                        self.lookup_list(list_name).unwrap_or_else(|| {
                            todo!("list `{list_name}` does not exist")
                        });
                    (*param_name, json!([list.name, list.id]))
                }
            })
            .collect();

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "parent": parent,
                "inputs": inputs,
            }),
        );
        Reporter::from_uid(this)
    }

    fn simple_proc(
        &self,
        opcode: &str,
        params: &[Param],
        parent: Uid,
        next: Option<Uid>,
        args: &[Expr],
    ) -> (Option<Uid>, Option<Uid>) {
        let this = self.new_uid();

        assert_eq!(params.len(), args.len());
        let inputs: Json = params
            .iter()
            .zip(args)
            .map(|(param, arg)| match param {
                Param::String(param_name) | Param::Number(param_name) => (
                    *param_name,
                    self.serialize_expr(arg, this).with_empty_shadow(),
                ),
                Param::Bool(param_name) => (
                    *param_name,
                    self.serialize_expr(arg, this).without_shadow(),
                ),
                Param::Var(param_name) => {
                    let var_name = match arg {
                        Expr::Sym(sym) => sym,
                        _ => todo!(),
                    };
                    let var = self.lookup_var(var_name).unwrap_or_else(|| {
                        todo!("variable `{var_name}` does not exist")
                    });
                    (*param_name, json!([var.name, var.id]))
                }
                Param::List(param_name) => {
                    let list_name = match arg {
                        Expr::Sym(sym) => sym,
                        _ => todo!(),
                    };
                    let list =
                        self.lookup_list(list_name).unwrap_or_else(|| {
                            todo!("list `{list_name}` does not exist")
                        });
                    (*param_name, json!([list.name, list.id]))
                }
            })
            .collect();

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "parent": parent,
                "next": next,
                "inputs": inputs,
            }),
        );
        (Some(this), Some(this))
    }

    fn stmt_input<'s>(
        &'s self,
        stmt: &'s Statement,
    ) -> impl Fn(Uid) -> Json + 's {
        |this| json!([2, self.serialize_stmt(stmt, this, None).0])
    }

    fn empty_shadow_input<'s>(
        &'s self,
        expr: &'s Expr,
    ) -> impl Fn(Uid) -> Json + 's {
        |this| self.serialize_expr(expr, this).with_empty_shadow()
    }

    fn shadowless_input<'s>(
        &'s self,
        expr: &'s Expr,
    ) -> impl Fn(Uid) -> Json + 's {
        |this| self.serialize_expr(expr, this).without_shadow()
    }

    fn var_input<'s>(&'s self, var_name: &'s str) -> impl Fn(Uid) -> Json + 's {
        move |_| {
            let var = self.lookup_var(var_name).unwrap_or_else(|| {
                todo!("variable `{var_name}` does not exist")
            });
            json!([var.name, var.id])
        }
    }

    fn emit_non_shadow(
        &self,
        opcode: &str,
        parent: Uid,
        inputs: &[(&str, &dyn Fn(Uid) -> Json)],
        fields: &[(&str, &dyn Fn(Uid) -> Json)],
    ) -> Reporter {
        let this = self.new_uid();

        let inputs: Json = inputs
            .iter()
            .copied()
            .map(|(name, fun)| (name, fun(this)))
            .collect();
        let fields: Json = fields
            .iter()
            .copied()
            .map(|(name, fun)| (name, fun(this)))
            .collect();

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "parent": parent,
                "inputs": inputs,
                "fields": fields,
            }),
        );

        Reporter::from_uid(this)
    }

    fn emit_stacking(
        &self,
        opcode: &str,
        parent: Uid,
        next: Option<Uid>,
        inputs: &[(&str, &dyn Fn(Uid) -> Json)],
        fields: &[(&str, &dyn Fn(Uid) -> Json)],
    ) -> (Option<Uid>, Option<Uid>) {
        let this = self.new_uid();

        let inputs: Json = inputs
            .iter()
            .copied()
            .map(|(name, fun)| (name, fun(this)))
            .collect();
        let fields: Json = fields
            .iter()
            .copied()
            .map(|(name, fun)| (name, fun(this)))
            .collect();

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

        (Some(this), Some(this))
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

fn serialize_lit(lit: &Value) -> Reporter {
    Reporter::shadow(json!([10, lit.to_cow_str()]))
}

enum Param<'a> {
    String(&'a str),
    Number(&'a str),
    Bool(&'a str),
    Var(&'a str),
    List(&'a str),
}
