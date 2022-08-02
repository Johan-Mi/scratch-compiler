use super::{reporter::Reporter, Mangled, SerCtx};
use crate::{
    error::{Error, Result},
    ir::{
        expr::Expr,
        {proc::Procedure, statement::Statement},
    },
    span::Span,
    uid::Uid,
};
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

    fn serialize_stmt(
        &self,
        stmt: &Statement,
        parent: Uid,
        next: Option<Uid>,
    ) -> Result<(Option<Uid>, Option<Uid>)> {
        Ok(match stmt {
            Statement::ProcCall {
                proc_name,
                proc_span,
                args,
            } => self.serialize_proc_call(
                proc_name, args, parent, next, *proc_span,
            )?,
            Statement::Do(stmts) => match &stmts[..] {
                [] => (None, Some(parent)),
                [single] => self.serialize_stmt(single, parent, next)?,
                stmts => stmts.iter().try_rfold(
                    (next, None),
                    |(old_start, old_end), stmt| {
                        let (new_start, new_end) =
                            self.serialize_stmt(stmt, parent, old_start)?;
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
                        Result::Ok((
                            new_start.or(old_start),
                            old_end.or(new_end),
                        ))
                    },
                )?,
            },
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let this = self.new_uid();
                let condition =
                    self.serialize_expr(condition, this)?.without_shadow();
                let (if_true, _) = self.serialize_stmt(if_true, this, None)?;
                let (if_false, _) =
                    self.serialize_stmt(if_false, this, None)?;

                let block_json = if if_false.is_some() {
                    json!({
                        "opcode": "control_if_else",
                        "parent": parent,
                        "next": next,
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
                        "next": next,
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
            )?,
            Statement::Forever(body) => {
                assert!(next.is_none());
                self.emit_stacking(
                    "control_forever",
                    parent,
                    next,
                    &[("SUBSTACK", &self.stmt_input(body))],
                    &[],
                )?
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
            )?,
            Statement::While { condition, body } => self.emit_stacking(
                "control_while",
                parent,
                next,
                &[
                    ("CONDITION", &self.shadowless_input(condition)),
                    ("SUBSTACK", &self.stmt_input(body)),
                ],
                &[],
            )?,
            Statement::For {
                counter,
                times,
                body,
            } => self.emit_stacking(
                "control_for_each",
                parent,
                next,
                &[
                    ("VALUE", &self.shadowless_input(times)),
                    ("SUBSTACK", &self.stmt_input(body)),
                ],
                &[("VARIABLE", &self.var_input(&counter.0, counter.1))],
            )?,
        })
    }

    fn serialize_expr(&self, expr: &Expr, parent: Uid) -> Result<Reporter> {
        Ok(match expr {
            Expr::Lit(lit) => serialize_lit(lit),
            Expr::Sym(sym, span) => match &**sym {
                "x-pos" => self.simple_symbol("motion_xposition", parent),
                "y-pos" => self.simple_symbol("motion_yposition", parent),
                "timer" => self.simple_symbol("sensing_timer", parent),
                "answer" => self.simple_symbol("sensing_answer", parent),
                _ => {
                    if self.proc_args.contains(sym) {
                        self.emit_non_shadow(
                            "argument_reporter_string_number",
                            parent,
                            &[],
                            &[("VALUE", &|_| Ok(json!([sym, null])))],
                        )?
                    } else if let Some(var) = self.lookup_var(sym) {
                        Reporter::non_shadow(json!([12, var.name, var.id]))
                    } else if let Some(list) = self.lookup_list(sym) {
                        Reporter::non_shadow(json!([13, list.name, list.id]))
                    } else {
                        return Err(Box::new(Error::UnknownVarOrList {
                            span: *span,
                            sym_name: sym.clone(),
                        }));
                    }
                }
            },
            Expr::FuncCall(func_name, span, args) => {
                self.serialize_func_call(func_name, args, parent, *span)?
            }
        })
    }

    fn simple_symbol(&self, opcode: &str, parent: Uid) -> Reporter {
        self.emit_non_shadow(opcode, parent, &[], &[]).unwrap()
    }

    fn serialize_func_call(
        &self,
        func_name: &str,
        args: &[Expr],
        parent: Uid,
        span: Span,
    ) -> Result<Reporter> {
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
                            Ok(serialize_lit(&Value::Num(0.0))
                                .with_empty_shadow())
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
                            Ok(self
                                .serialize_func_call("+", rest, parent, span)?
                                .with_empty_shadow())
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
                            Ok(self
                                .serialize_func_call(
                                    "+",
                                    denominators,
                                    parent,
                                    span,
                                )?
                                .with_empty_shadow())
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
            "abs" => self.mathop("abs", parent, args),
            "floor" => self.mathop("floor", parent, args),
            "ceil" => self.mathop("ceiling", parent, args),
            "sqrt" => self.mathop("sqrt", parent, args),
            "ln" => self.mathop("ln", parent, args),
            "log" => self.mathop("log", parent, args),
            "e^" => self.mathop("e ^", parent, args),
            "ten^" => self.mathop("10 ^", parent, args),
            "sin" => self.mathop("sin", parent, args),
            "cos" => self.mathop("cos", parent, args),
            "tan" => self.mathop("tan", parent, args),
            "asin" => self.mathop("asin", parent, args),
            "acos" => self.mathop("acos", parent, args),
            "atan" => self.mathop("atan", parent, args),
            _ => Err(Box::new(Error::UnknownFunction {
                span,
                func_name: func_name.to_owned(),
            })),
        }
    }

    fn mathop(
        &self,
        op_name: &str,
        parent: Uid,
        args: &[Expr],
    ) -> Result<Reporter> {
        let num = match args {
            [num] => |parent| {
                Ok(self.serialize_expr(num, parent)?.with_empty_shadow())
            },
            _ => todo!(),
        };
        self.emit_non_shadow(
            "operator_mathop",
            parent,
            &[("NUM", &num)],
            &[("OPERATOR", &|_| Ok(json!([op_name, null])))],
        )
    }

    fn associative0(
        &self,
        opcode: &str,
        lhs_name: &str,
        rhs_name: &str,
        neutral: &Value,
        args: &[Expr],
        parent: Uid,
    ) -> Result<Reporter> {
        match args {
            [] => Ok(serialize_lit(neutral)),
            [single] => self.serialize_expr(single, parent),
            [lhs, rhs @ ..] => self.emit_non_shadow(
                opcode,
                parent,
                &[
                    (lhs_name, &self.empty_shadow_input(lhs)),
                    (rhs_name, &|parent| {
                        Ok(self
                            .associative0(
                                opcode, lhs_name, rhs_name, neutral, rhs,
                                parent,
                            )?
                            .with_empty_shadow())
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
        span: Span,
    ) -> Result<(Option<Uid>, Option<Uid>)> {
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
            "send-broadcast-sync" => match args {
                [name] => {
                    let broadcast_input = |parent| {
                        Ok(match name {
                            Expr::Lit(lit) => {
                                json!([1, [11, lit.to_string(), ""]])
                            }
                            _ => self
                                .serialize_expr(name, parent)?
                                .without_shadow(),
                        })
                    };
                    self.emit_stacking(
                        "event_broadcastandwait",
                        parent,
                        next,
                        &[("BROADCAST_INPUT", &broadcast_input)],
                        &[],
                    )
                }
                _ => todo!(),
            },
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
            "stop-all" => match args {
                [] => self.emit_stacking(
                    "control_stop",
                    parent,
                    next,
                    &[],
                    &[("STOP_OPTION", &|_| Ok(json!(["all", null])))],
                ),
                _ => todo!(),
            },
            "stop-this-script" => match args {
                [] => self.emit_stacking(
                    "control_stop",
                    parent,
                    next,
                    &[],
                    &[("STOP_OPTION", &|_| Ok(json!(["this script", null])))],
                ),
                _ => todo!(),
            },
            "stop-other-scripts" => match args {
                [] => self.emit_stacking(
                    "control_stop",
                    parent,
                    next,
                    &[],
                    &[("STOP_OPTION", &|_| {
                        Ok(json!(["other scripts in this sprite", null]))
                    })],
                ),
                _ => todo!(),
            },
            "clone-myself" => todo!(),
            _ => self.serialize_custom_proc_call(
                proc_name, args, parent, next, span,
            ),
        }
    }

    fn serialize_custom_proc_call(
        &self,
        proc_name: &str,
        args: &[Expr],
        parent: Uid,
        next: Option<Uid>,
        span: Span,
    ) -> Result<(Option<Uid>, Option<Uid>)> {
        let proc = self.custom_procs.get(proc_name).ok_or_else(|| {
            Box::new(Error::UnknownProc {
                span,
                proc_name: proc_name.to_owned(),
            })
        })?;

        let this = self.new_uid();

        let args: Vec<Json> = args
            .iter()
            .map(|arg| Ok(self.serialize_expr(arg, this)?.with_empty_shadow()))
            .collect::<Result<_>>()?;

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
                "mutation": {
                      "tagName": "mutation",
                      "children": [],
                      "proccode": proccode,
                      "argumentids": argumentids,
                      "warp": "true",
                }
            }),
        );
        Ok((Some(this), Some(this)))
    }

    fn simple_function(
        &self,
        opcode: &str,
        params: &[Param],
        parent: Uid,
        args: &[Expr],
    ) -> Result<Reporter> {
        let this = self.new_uid();

        assert_eq!(params.len(), args.len());
        let inputs: Json = params
            .iter()
            .zip(args)
            .map(|(param, arg)| {
                Ok(match param {
                    Param::String(param_name) | Param::Number(param_name) => {
                        Some((
                            *param_name,
                            self.serialize_expr(arg, this)?.with_empty_shadow(),
                        ))
                    }
                    Param::Bool(param_name) => Some((
                        *param_name,
                        self.serialize_expr(arg, this)?.without_shadow(),
                    )),
                    _ => None,
                })
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;
        let fields: Json = params
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
                                Box::new(Error::UnknownVar {
                                    span,
                                    var_name: var_name.clone(),
                                })
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
                                Box::new(Error::UnknownList {
                                    span,
                                    list_name: list_name.clone(),
                                })
                            })?;
                        Some((*param_name, json!([list.name, list.id])))
                    }
                    _ => None,
                })
            })
            .filter_map(Result::transpose)
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

    fn simple_proc(
        &self,
        opcode: &str,
        params: &[Param],
        parent: Uid,
        next: Option<Uid>,
        args: &[Expr],
    ) -> Result<(Option<Uid>, Option<Uid>)> {
        let this = self.new_uid();

        assert_eq!(params.len(), args.len());
        let inputs: Json = params
            .iter()
            .zip(args)
            .map(|(param, arg)| {
                Ok(match param {
                    Param::String(param_name) | Param::Number(param_name) => {
                        Some((
                            *param_name,
                            self.serialize_expr(arg, this)?.with_empty_shadow(),
                        ))
                    }
                    Param::Bool(param_name) => Some((
                        *param_name,
                        self.serialize_expr(arg, this)?.without_shadow(),
                    )),
                    _ => None,
                })
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;
        let fields: Json = params
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
                                Box::new(Error::UnknownVar {
                                    span,
                                    var_name: var_name.clone(),
                                })
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
                                Box::new(Error::UnknownList {
                                    span,
                                    list_name: list_name.clone(),
                                })
                            })?;
                        Some((*param_name, json!([list.name, list.id])))
                    }
                    _ => None,
                })
            })
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;

        self.emit_block(
            this,
            json!({
                "opcode": opcode,
                "parent": parent,
                "next": next,
                "inputs": inputs,
                "fields": fields,
            }),
        );
        Ok((Some(this), Some(this)))
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
