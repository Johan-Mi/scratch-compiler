use crate::{
    codegen::sb3::{Call, Param, SerCtx},
    diagnostic::{Error, Result},
    ir::{expr::Expr, statement::Statement},
    span::Span,
    uid::Uid,
};
use serde_json::{json, Value as Json};

impl SerCtx {
    pub(super) fn serialize_stmt(
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
                    Call {
                        args,
                        name: proc_name,
                        opcode: stringify!($opcode),
                        parent,
                        span,
                    },
                    &[$(Param::$param_type(stringify!($param_name))),*],
                    next,
                )
            }
        }

        let wrong_arg_count = |expected| {
            Err(Box::new(Error::BuiltinProcWrongArgCount {
                span,
                proc_name: proc_name.to_owned(),
                expected,
                got: args.len(),
            }))
        };

        match proc_name {
            "erase-all" => proc!(pen_clear()),
            "stamp" => proc!(pen_stamp()),
            "pen-down" => proc!(pen_penDown()),
            "pen-up" => proc!(pen_penUp()),
            "set-pen-size" => proc!(pen_setPenSizeTo(SIZE: Number)),
            // TODO: Color parameters
            "set-pen-color" => proc!(pen_setPenColorToColor(COLOR: Number)),
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
                _ => wrong_arg_count(1),
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
                _ => wrong_arg_count(0),
            },
            "stop-this-script" => match args {
                [] => self.emit_stacking(
                    "control_stop",
                    parent,
                    next,
                    &[],
                    &[("STOP_OPTION", &|_| Ok(json!(["this script", null])))],
                ),
                _ => wrong_arg_count(0),
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
                _ => wrong_arg_count(0),
            },
            "clone-myself" => todo!(),
            "reset-timer" => proc!(sensing_resettimer()),
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
            Error::UnknownProc {
                span,
                proc_name: proc_name.to_owned(),
            }
        })?;

        if args.len() != proc.params.len() {
            return Err(Box::new(Error::CustomProcWrongArgCount {
                span,
                proc_name: proc_name.to_owned(),
                expected: proc.params.len(),
                got: args.len(),
            }));
        }

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

    fn simple_proc(
        &self,
        call: Call,
        params: &[Param],
        next: Option<Uid>,
    ) -> Result<(Option<Uid>, Option<Uid>)> {
        let this = self.new_uid();
        let Call {
            args,
            name: proc_name,
            opcode,
            parent,
            span,
        } = call;

        if params.len() != args.len() {
            return Err(Box::new(Error::BuiltinProcWrongArgCount {
                span,
                proc_name: proc_name.to_owned(),
                expected: params.len(),
                got: args.len(),
            }));
        }
        let (inputs, fields) =
            self.create_inputs_and_fields(params, args, this)?;

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
}
