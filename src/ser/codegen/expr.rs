use crate::{
    error::{Error, Result},
    ser::{
        codegen::{Call, Expr, Param, Reporter},
        SerCtx,
    },
    span::Span,
    uid::Uid,
};
use sb3_stuff::Value;
use serde_json::json;

impl SerCtx {
    pub(super) fn serialize_expr(
        &self,
        expr: &Expr,
        parent: Uid,
    ) -> Result<Reporter> {
        Ok(match expr {
            Expr::Lit(lit) => Reporter::Literal(lit.clone()),
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
                            Ok(Reporter::Literal(Value::Num(0.0))
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
                                    "*",
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
                &Value::String(smol_str::SmolStr::default()),
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
            [] => Ok(Reporter::Literal(neutral.clone())),
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
