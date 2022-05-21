use crate::{
    ir::{
        expr::{Expr, Value},
        proc::{Procedure, Statement},
    },
    ser::{reporter::Reporter, ProgramCtx},
    uid::Uid,
};
use serde_json::{json, Value as Json};
use std::{cell::RefCell, collections::HashMap};

pub(super) fn serialize_procs(
    ctx: &ProgramCtx,
    procs: &HashMap<String, Procedure>,
) -> HashMap<Uid, Json> {
    let ctx = ProcCtx {
        inner: ctx,
        blocks: Default::default(),
    };
    for (name, proc) in procs {
        ctx.serialize_proc(name, proc);
    }
    ctx.blocks.into_inner()
}

struct ProcCtx<'a> {
    inner: &'a ProgramCtx,
    blocks: RefCell<HashMap<Uid, Json>>,
}

impl<'a> ProcCtx<'a> {
    fn serialize_proc(&self, name: &str, proc: &Procedure) {
        match name {
            "when-flag-clicked" => {
                assert!(proc.params.is_empty());
                let this = self.new_uid();
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
            "when-cloned" => todo!(),
            "when-received" => todo!(),
            _ => todo!(),
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
                _ => {
                    todo!() // How can we do this without ugly hacks?
                }
            },
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => todo!(),
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
            } => todo!(),
        }
    }

    fn serialize_expr(&self, expr: &Expr, parent: Uid) -> Reporter {
        match expr {
            Expr::Lit(lit) => serialize_lit(lit),
            Expr::Sym(_) => todo!(),
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
        match func_name {
            "+" => self.associative0(
                "operator_add",
                "NUM1",
                "NUM2",
                &Value::Num(0.0),
                args,
                parent,
            ),
            "-" => todo!(),
            "*" => self.associative0(
                "operator_multiply",
                "NUM1",
                "NUM2",
                &Value::Num(1.0),
                args,
                parent,
            ),
            "/" => todo!(),
            "!!" => todo!(),
            "++" => self.associative0(
                "operator_join",
                "STRING1",
                "STRING2",
                &Value::String(String::new()),
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
            "not" => todo!(),
            "=" => todo!(),
            "<" => todo!(),
            ">" => todo!(),
            "length" => todo!(),
            "str-length" => todo!(),
            "char-at" => todo!(),
            "mod" => todo!(),
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
            [lhs, rhs] => {
                let this = self.new_uid();
                let lhs = self.serialize_expr(lhs, this).with_empty_shadow();
                let rhs = self.serialize_expr(rhs, this).with_empty_shadow();
                self.emit_block(
                    this,
                    json!({
                        "opcode": opcode,
                        "parent": parent,
                        "inputs": {
                            lhs_name: lhs,
                            rhs_name: rhs,
                        },
                    }),
                );
                Reporter::from_uid(this)
            }
            _ => todo!("variadic argument counts"),
        }
    }

    fn serialize_proc_call(
        &self,
        proc_name: &str,
        args: &[Expr],
        parent: Uid,
        next: Option<Uid>,
    ) -> (Option<Uid>, Option<Uid>) {
        match proc_name {
            _ => todo!("unknown procedure `{proc_name}`"),
        }
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

    fn emit_stacking(
        &self,
        opcode: &str,
        parent: Uid,
        next: Option<Uid>,
        inputs: &[(&str, &dyn Fn(Uid) -> Json)],
        fields: &[(&str, &dyn Fn(Uid) -> Json)],
    ) -> (Option<Uid>, Option<Uid>) {
        let this = self.new_uid();

        let inputs = Json::Object(
            inputs
                .iter()
                .copied()
                .map(|(name, fun)| (name.to_owned(), fun(this)))
                .collect(),
        );
        let fields = Json::Object(
            fields
                .iter()
                .copied()
                .map(|(name, fun)| (name.to_owned(), fun(this)))
                .collect(),
        );

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

    fn new_uid(&self) -> Uid {
        self.inner.new_uid()
    }
}

fn serialize_lit(lit: &Value) -> Reporter {
    Reporter::shadow(json!([10, lit.to_cow_str()]))
}
