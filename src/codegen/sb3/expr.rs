use crate::{
    codegen::sb3::{Call, Expr, Param, Reporter, SerCtx},
    diagnostic::{Error, Result},
    uid::Uid,
};
use codemap::Span;
use sb3_stuff::Value;
use serde_json::json;

impl SerCtx<'_> {
    pub(super) fn serialize_expr(
        &self,
        expr: &Expr,
        parent: Uid,
    ) -> Result<Reporter> {
        Ok(match expr {
            Expr::Imm(imm) => Reporter::Literal(imm.clone()),
            Expr::Sym(sym, span) => match &**sym {
                "x-pos" => self.simple_symbol("motion_xposition", parent),
                "y-pos" => self.simple_symbol("motion_yposition", parent),
                "timer" => self.simple_symbol("sensing_timer", parent),
                "answer" => self.simple_symbol("sensing_answer", parent),
                _ => {
                    if self.proc_args.contains(&&**sym) {
                        self.emit_non_shadow(
                            "argument_reporter_string_number",
                            parent,
                            &[],
                            &[("VALUE", &|_| Ok(json!([**sym, null])))],
                        )?
                    } else if let Some(var) = self.lookup_var(sym).cloned() {
                        Reporter::Variable(var)
                    } else if let Some(list) = self.lookup_list(sym).cloned() {
                        Reporter::List(list)
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
            Expr::AddSub(positives, negatives) => {
                self.serialize_add_sub(positives, negatives, parent)?
            }
            Expr::MulDiv(numerators, denominators) => {
                self.serialize_mul_div(numerators, denominators, parent)?
            }
        })
    }

    fn serialize_mul_div(
        &self,
        numerators: &[Expr],
        denominators: &[Expr],
        parent: Uid,
    ) -> Result<Reporter> {
        let multiplication = |terms, parent| {
            self.associative1(
                "operator_multiply",
                "NUM1",
                "NUM2",
                terms,
                parent,
            )
        };
        Ok(match (numerators.is_empty(), denominators.is_empty()) {
            (true, true) => Reporter::Literal(Value::Num(1.0)),
            (true, false) => self.emit_non_shadow(
                "operator_divide",
                parent,
                &[
                    ("NUM1", &|_| Ok(json!([1, [10, "1"]]))),
                    ("NUM2", &|parent| {
                        Ok(multiplication(denominators, parent)?
                            .with_empty_shadow())
                    }),
                ],
                &[],
            )?,
            (false, true) => multiplication(numerators, parent)?,
            (false, false) => self.emit_non_shadow(
                "operator_divide",
                parent,
                &[
                    ("NUM1", &|parent| {
                        Ok(multiplication(numerators, parent)?
                            .with_empty_shadow())
                    }),
                    ("NUM2", &|parent| {
                        Ok(multiplication(denominators, parent)?
                            .with_empty_shadow())
                    }),
                ],
                &[],
            )?,
        })
    }

    fn serialize_add_sub(
        &self,
        positives: &[Expr],
        negatives: &[Expr],
        parent: Uid,
    ) -> Result<Reporter> {
        let addition = |terms, parent| {
            self.associative1("operator_add", "NUM1", "NUM2", terms, parent)
        };
        Ok(match (positives.is_empty(), negatives.is_empty()) {
            (true, true) => Reporter::Literal(Value::Num(0.0)),
            (true, false) => self.emit_non_shadow(
                "operator_subtract",
                parent,
                &[
                    ("NUM1", &|_| Ok(json!([1, [10, ""]]))),
                    ("NUM2", &|parent| {
                        Ok(addition(negatives, parent)?.with_empty_shadow())
                    }),
                ],
                &[],
            )?,
            (false, true) => addition(positives, parent)?,
            (false, false) => self.emit_non_shadow(
                "operator_subtract",
                parent,
                &[
                    ("NUM1", &|parent| {
                        Ok(addition(positives, parent)?.with_empty_shadow())
                    }),
                    ("NUM2", &|parent| {
                        Ok(addition(negatives, parent)?.with_empty_shadow())
                    }),
                ],
                &[],
            )?,
        })
    }

    fn simple_symbol(&self, opcode: &str, parent: Uid) -> Reporter {
        self.emit_non_shadow(opcode, parent, &[], &[]).unwrap()
    }

    fn serialize_func_call(
        &self,
        func_name: &'static str,
        args: &[Expr],
        parent: Uid,
        span: Span,
    ) -> Result<Reporter> {
        macro_rules! func {
            ($opcode:ident(
                $($param_name:ident: $param_type:ident),*
            )) => {
                self.simple_function(
                    Call {
                        name: func_name,
                        opcode: stringify!($opcode),
                        parent,
                        args,
                        span,
                    },
                    &[$(Param::$param_type(stringify!($param_name))),*],
                )
            }
        }
        match func_name {
            "!!" => func!(data_itemoflist(LIST: List, INDEX: Number)),
            "++" => self.associative1(
                "operator_join",
                "STRING1",
                "STRING2",
                args,
                parent,
            ),
            "and" => self.associative1(
                "operator_and",
                "OPERAND1",
                "OPERAND2",
                args,
                parent,
            ),
            "or" => self.associative1(
                "operator_or",
                "OPERAND1",
                "OPERAND2",
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
            "abs" => self.mathop("abs", parent, args, span),
            "floor" => self.mathop("floor", parent, args, span),
            "ceil" => self.mathop("ceiling", parent, args, span),
            "sqrt" => self.mathop("sqrt", parent, args, span),
            "ln" => self.mathop("ln", parent, args, span),
            "log" => self.mathop("log", parent, args, span),
            "e^" => self.mathop("e ^", parent, args, span),
            "ten^" => self.mathop("10 ^", parent, args, span),
            "sin" => self.mathop("sin", parent, args, span),
            "cos" => self.mathop("cos", parent, args, span),
            "tan" => self.mathop("tan", parent, args, span),
            "asin" => self.mathop("asin", parent, args, span),
            "acos" => self.mathop("acos", parent, args, span),
            "atan" => self.mathop("atan", parent, args, span),
            "pressing-key" => func!(sensing_keypressed(KEY_OPTION: String)),
            "to-num" => match args {
                [arg] => self.emit_non_shadow(
                    "operator_add",
                    parent,
                    &[
                        ("NUM1", &|_| Ok(json!([1, [10, ""]]))),
                        ("NUM2", &self.shadowless_input(arg)),
                    ],
                    &[],
                ),
                _ => Err(Box::new(Error::FunctionWrongArgCount {
                    span,
                    func_name,
                    expected: 1,
                    got: args.len(),
                })),
            },
            "random" => match args {
                [low, high] => self.emit_non_shadow(
                    "operator_random",
                    parent,
                    &[
                        ("FROM", &self.shadowless_input(low)),
                        ("TO", &self.shadowless_input(high)),
                    ],
                    &[],
                ),
                _ => Err(Box::new(Error::FunctionWrongArgCount {
                    span,
                    func_name,
                    expected: 2,
                    got: args.len(),
                })),
            },
            _ => Err(Box::new(Error::UnknownFunction {
                span,
                func_name: func_name.to_owned(),
            })),
        }
    }

    fn mathop(
        &self,
        op_name: &'static str,
        parent: Uid,
        args: &[Expr],
        span: Span,
    ) -> Result<Reporter> {
        let num = |parent| match args {
            [num] => Ok(self.serialize_expr(num, parent)?.with_empty_shadow()),
            _ => Err(Box::new(Error::FunctionWrongArgCount {
                span,
                func_name: op_name,
                expected: 1,
                got: args.len(),
            })),
        };
        self.emit_non_shadow(
            "operator_mathop",
            parent,
            &[("NUM", &num)],
            &[("OPERATOR", &|_| Ok(json!([op_name, null])))],
        )
    }

    fn associative1(
        &self,
        opcode: &str,
        lhs_name: &str,
        rhs_name: &str,
        args: &[Expr],
        parent: Uid,
    ) -> Result<Reporter> {
        match args {
            [] => unreachable!(),
            [single] => self.serialize_expr(single, parent),
            [lhs, rhs @ ..] => self.emit_non_shadow(
                opcode,
                parent,
                &[
                    (lhs_name, &self.empty_shadow_input(lhs)),
                    (rhs_name, &|parent| {
                        Ok(self
                            .associative1(
                                opcode, lhs_name, rhs_name, rhs, parent,
                            )?
                            .with_empty_shadow())
                    }),
                ],
                &[],
            ),
        }
    }

    fn simple_function(
        &self,
        call: Call<'static, '_>,
        params: &[Param],
    ) -> Result<Reporter> {
        let this = self.new_uid();
        let Call {
            name: func_name,
            opcode,
            parent,
            args,
            span,
        } = call;

        if params.len() != args.len() {
            return Err(Box::new(Error::FunctionWrongArgCount {
                span,
                func_name,
                expected: params.len(),
                got: args.len(),
            }));
        }
        assert_eq!(params.len(), args.len());
        let (inputs, fields) =
            self.create_inputs_and_fields(params, args, this)?;

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
}
