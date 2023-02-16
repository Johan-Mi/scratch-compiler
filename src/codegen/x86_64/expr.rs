use super::{
    typ::{expr_type, MixedSizeValue, Typ},
    Program,
};
use crate::{
    diagnostic::{Error, Result},
    ir::expr::Expr,
    span::Span,
};
use cranelift::prelude::{types::*, *};
use sb3_stuff::Value as Immediate;
use std::{borrow::Cow, cmp::Ordering};

impl<'a> Program<'a> {
    pub(super) fn generate_expr(
        &mut self,
        expr: &'a Expr,
        fb: &mut FunctionBuilder,
    ) -> Result<MixedSizeValue> {
        match expr {
            Expr::Imm(imm) => Ok(self.generate_imm(imm, fb)),
            Expr::Sym(sym, sym_span) => {
                self.generate_symbol(sym, *sym_span, fb)
            }
            Expr::FuncCall(func_name, span, args) => {
                self.generate_func_call(func_name, args, *span, fb)
            }
            Expr::AddSub(positives, negatives) => self
                .generate_add_sub(positives, negatives, fb)
                .map(From::from),
            Expr::MulDiv(numerators, denominators) => self
                .generate_mul_div(numerators, denominators, fb)
                .map(From::from),
        }
    }

    fn generate_add_sub(
        &mut self,
        positives: &'a [Expr],
        negatives: &'a [Expr],
        fb: &mut FunctionBuilder,
    ) -> Result<Value> {
        let positive_sum = if let [initial, rest @ ..] = positives {
            let initial = self.generate_double_expr(initial, fb)?;
            rest.iter().try_fold(initial, |accum, term| {
                let term = self.generate_double_expr(term, fb)?;
                Result::Ok(fb.ins().fadd(accum, term))
            })?
        } else {
            fb.ins().f64const(0.0)
        };
        negatives.iter().try_fold(positive_sum, |accum, term| {
            let term = self.generate_double_expr(term, fb)?;
            Result::Ok(fb.ins().fsub(accum, term))
        })
    }

    fn generate_mul_div(
        &mut self,
        numerators: &'a [Expr],
        denominators: &'a [Expr],
        fb: &mut FunctionBuilder,
    ) -> Result<Value> {
        let numerator_product = if let [initial, rest @ ..] = numerators {
            let initial = self.generate_double_expr(initial, fb)?;
            rest.iter().try_fold(initial, |accum, factor| {
                let factor = self.generate_double_expr(factor, fb)?;
                Result::Ok(fb.ins().fmul(accum, factor))
            })?
        } else {
            fb.ins().f64const(1.0)
        };
        denominators
            .iter()
            .try_fold(numerator_product, |accum, factor| {
                let factor = self.generate_double_expr(factor, fb)?;
                Result::Ok(fb.ins().fdiv(accum, factor))
            })
    }

    fn generate_symbol(
        &mut self,
        sym: &str,
        span: Span,
        fb: &mut FunctionBuilder,
    ) -> Result<MixedSizeValue> {
        if sym == "answer" {
            let answer = self.answer(fb);
            let mem_flags = MemFlags::trusted();
            let low = fb.ins().load(I64, mem_flags, answer, 0);
            let high = fb.ins().load(I64, mem_flags, answer, 8);
            let cloned = self.call_extern("clone_cow", &[low, high], fb);
            Ok(pair(fb.inst_results(cloned)).into())
        } else if let Some(param) = self.proc_params.get(sym) {
            let cloned = self.call_extern("clone_any", &[param.0, param.1], fb);
            Ok(pair(fb.inst_results(cloned)).into())
        } else if let Some(var) = self.lookup_var(sym, fb) {
            let mem_flags = MemFlags::trusted();
            let low = fb.ins().load(I64, mem_flags, var, 0);
            let high = fb.ins().load(I64, mem_flags, var, 8);
            let cloned = self.call_extern("clone_any", &[low, high], fb);
            Ok(pair(fb.inst_results(cloned)).into())
        } else {
            Err(Box::new(Error::UnknownVarOrList {
                span,
                sym_name: sym.into(),
            }))
        }
    }

    fn generate_func_call(
        &mut self,
        func_name: &'static str,
        args: &'a [Expr],
        span: Span,
        fb: &mut FunctionBuilder,
    ) -> Result<MixedSizeValue> {
        let wrong_arg_count = |expected| {
            Err(Box::new(Error::FunctionWrongArgCount {
                span,
                func_name,
                expected,
                got: args.len(),
            }))
        };

        let mut mathop = |op| match args {
            [operand] => {
                let n = self.generate_double_expr(operand, fb)?;
                let res = self.call_extern(op, &[n], fb);
                Ok(fb.inst_results(res)[0].into())
            }
            _ => wrong_arg_count(1),
        };

        match func_name {
            "!!" => match args {
                [Expr::Sym(list_name, list_span), index] => {
                    let list = self.lookup_list(list_name, *list_span, fb)?;
                    let index = self.generate_any_expr(index, fb)?;
                    let got = self.call_extern(
                        "list_get",
                        &[index.0, index.1, list],
                        fb,
                    );
                    Ok(pair(fb.inst_results(got)).into())
                }
                _ => wrong_arg_count(2),
            },
            "++" => {
                let args = args
                    .iter()
                    .map(|arg| self.generate_cow_expr(arg, fb))
                    .collect::<Result<Vec<_>>>()?;
                let total_len = args
                    .iter()
                    .map(|(_, len)| *len)
                    .reduce(|a, b| fb.ins().iadd(a, b))
                    .unwrap();
                let buf = self.call_extern("malloc", &[total_len], fb);
                let buf = fb.inst_results(buf)[0];

                let dest = self.new_variable();
                fb.declare_var(dest, I64);
                fb.def_var(dest, buf);
                for (i, (ptr, len)) in args.iter().enumerate() {
                    let dest_value = fb.use_var(dest);
                    fb.call_memcpy(
                        self.target_frontend_config,
                        dest_value,
                        *ptr,
                        *len,
                    );
                    if args.len() - i != 1 {
                        let next_dest = fb.ins().iadd(dest_value, *len);
                        fb.def_var(dest, next_dest);
                    }
                    self.call_extern("drop_cow", &[*ptr], fb);
                }

                Ok((buf, total_len).into())
            }
            "and" | "or" => match args {
                [] => unreachable!(),
                [rest @ .., last] => {
                    let last_block = fb.create_block();
                    let res = fb.append_block_param(last_block, I8);
                    for term in rest {
                        let term = self.generate_bool_expr(term, fb)?;
                        let next_block = fb.create_block();
                        if func_name == "and" {
                            fb.ins().brz(term, last_block, &[term]);
                        } else {
                            fb.ins().brnz(term, last_block, &[term]);
                        }
                        fb.ins().jump(next_block, &[]);
                        fb.switch_to_block(next_block);
                        fb.seal_block(next_block);
                    }
                    let last_term = self.generate_bool_expr(last, fb)?;
                    fb.ins().jump(last_block, &[last_term]);
                    fb.switch_to_block(last_block);
                    fb.seal_block(last_block);
                    Ok(res.into())
                }
            },
            "not" => match args {
                [operand] => {
                    let operand = self.generate_bool_expr(operand, fb)?;
                    Ok(fb.ins().bxor_imm(operand, 1).into())
                }
                _ => wrong_arg_count(1),
            },
            "<" | "=" | ">" => match args {
                [lhs, rhs] => {
                    let ordering = match func_name {
                        "<" => Ordering::Less,
                        "=" => Ordering::Equal,
                        ">" => Ordering::Greater,
                        _ => unreachable!(),
                    };
                    Ok(self.generate_comparison(ordering, lhs, rhs, fb)?.into())
                }
                _ => wrong_arg_count(2),
            },
            "length" => match args {
                [Expr::Sym(list_name, list_span)] => {
                    let list = self.lookup_list(list_name, *list_span, fb)?;
                    let mem_flags = MemFlags::trusted();
                    let len_as_usize = fb.ins().load(I64, mem_flags, list, 8);
                    Ok(fb.ins().fcvt_from_uint(F64, len_as_usize).into())
                }
                _ => wrong_arg_count(1),
            },
            "str-length" => match args {
                [s] => {
                    let s = self.generate_cow_expr(s, fb)?;
                    let len = self.call_extern("str_length", &[s.0, s.1], fb);
                    let len = fb.inst_results(len)[0];
                    let len = fb.ins().fcvt_from_uint(F64, len);
                    self.call_extern("drop_cow", &[s.0], fb);
                    Ok(len.into())
                }
                _ => wrong_arg_count(1),
            },
            "char-at" => match args {
                [s, index] => {
                    let s = self.generate_cow_expr(s, fb)?;
                    let index = self.generate_double_expr(index, fb)?;
                    let index = fb.ins().fcvt_to_uint_sat(I64, index);
                    let res =
                        self.call_extern("char_at", &[s.0, s.1, index], fb);
                    self.call_extern("drop_cow", &[s.0], fb);
                    Ok(pair(fb.inst_results(res)).into())
                }
                _ => wrong_arg_count(2),
            },
            "mod" => match args {
                [a, n] => {
                    let a = self.generate_double_expr(a, fb)?;
                    let n = self.generate_double_expr(n, fb)?;
                    let res = self.call_extern("fmod", &[a, n], fb);
                    Ok(fb.inst_results(res)[0].into())
                }
                _ => wrong_arg_count(2),
            },
            "abs" => match args {
                [operand] => {
                    let n = self.generate_double_expr(operand, fb)?;
                    Ok(fb.ins().fabs(n).into())
                }
                _ => wrong_arg_count(1),
            },
            "floor" => match args {
                [operand] => {
                    let n = self.generate_double_expr(operand, fb)?;
                    Ok(fb.ins().floor(n).into())
                }
                _ => wrong_arg_count(1),
            },
            "ceil" => match args {
                [operand] => {
                    let n = self.generate_double_expr(operand, fb)?;
                    Ok(fb.ins().ceil(n).into())
                }
                _ => wrong_arg_count(1),
            },
            "sqrt" => match args {
                [operand] => {
                    let n = self.generate_double_expr(operand, fb)?;
                    Ok(fb.ins().sqrt(n).into())
                }
                _ => wrong_arg_count(1),
            },
            "ln" => mathop("log"),
            "log" => mathop("log10"),
            "e^" => mathop("exp"),
            "ten^" => mathop("exp10"),
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" => {
                mathop(func_name)
            }
            "to-num" => match args {
                [operand] => {
                    self.generate_double_expr(operand, fb).map(From::from)
                }
                _ => wrong_arg_count(1),
            },
            "random" => match args {
                [low, high] => {
                    self.uses_drand48 = true;
                    let low = self.generate_double_expr(low, fb)?;
                    let high = self.generate_double_expr(high, fb)?;
                    let res =
                        self.call_extern("random_between", &[low, high], fb);
                    Ok(fb.inst_results(res)[0].into())
                }
                _ => wrong_arg_count(2),
            },
            _ => Err(Box::new(Error::UnknownFunction {
                span,
                func_name: func_name.to_owned(),
            })),
        }
    }

    pub(super) fn generate_bool_expr(
        &mut self,
        expr: &'a Expr,
        fb: &mut FunctionBuilder,
    ) -> Result<Value> {
        let res = self.generate_expr(expr, fb)?;
        match expr_type(expr) {
            Typ::Double => todo!(),
            Typ::Bool => Ok(res.single()),
            Typ::StaticStr(_) => todo!(),
            Typ::OwnedString => todo!(),
            Typ::Any => {
                let inst = self.call_extern("any_to_bool", res.as_slice(), fb);
                Ok(fb.inst_results(inst)[0])
            }
        }
    }

    pub(super) fn generate_double_expr(
        &mut self,
        expr: &'a Expr,
        fb: &mut FunctionBuilder,
    ) -> Result<Value> {
        let res = self.generate_expr(expr, fb)?;
        match expr_type(expr) {
            Typ::Double => Ok(res.single()),
            Typ::Bool => todo!(),
            Typ::StaticStr(_) => todo!(),
            Typ::OwnedString => todo!(),
            Typ::Any => {
                let inst =
                    self.call_extern("any_to_double", res.as_slice(), fb);
                Ok(fb.inst_results(inst)[0])
            }
        }
    }

    pub(super) fn generate_cow_expr(
        &mut self,
        expr: &'a Expr,
        fb: &mut FunctionBuilder,
    ) -> Result<(Value, Value)> {
        let res = self.generate_expr(expr, fb)?;
        match expr_type(expr) {
            Typ::Double => {
                let inst =
                    self.call_extern("double_to_cow", &[res.single()], fb);
                Ok(pair(fb.inst_results(inst)))
            }
            Typ::Bool => {
                let inst = self.call_extern("bool_to_str", &[res.single()], fb);
                Ok(pair(fb.inst_results(inst)))
            }
            Typ::StaticStr(_) | Typ::OwnedString => Ok(res.pair()),
            Typ::Any => {
                let inst = self.call_extern("any_to_cow", res.as_slice(), fb);
                Ok(pair(fb.inst_results(inst)))
            }
        }
    }

    pub(super) fn generate_any_expr(
        &mut self,
        expr: &'a Expr,
        fb: &mut FunctionBuilder,
    ) -> Result<(Value, Value)> {
        let res = self.generate_expr(expr, fb)?;
        match expr_type(expr) {
            Typ::Double => {
                let bits = fb.ins().bitcast(I64, MemFlags::new(), res.single());
                Ok((fb.ins().iconst(I64, 2), bits))
            }
            Typ::Bool => {
                let extended = fb.ins().uextend(I64, res.single());
                Ok((extended, fb.ins().iconst(I64, 0)))
            }
            Typ::StaticStr(_) | Typ::OwnedString | Typ::Any => Ok(res.pair()),
        }
    }

    fn generate_imm(
        &mut self,
        imm: &'a Immediate,
        fb: &mut FunctionBuilder,
    ) -> MixedSizeValue {
        match imm {
            Immediate::Num(n) => fb.ins().f64const(*n).into(),
            Immediate::String(s) => {
                self.allocate_static_str(Cow::Borrowed(s), fb).into()
            }
            Immediate::Bool(b) => fb.ins().iconst(I8, i64::from(*b)).into(),
        }
    }

    fn generate_comparison(
        &mut self,
        mut ordering: Ordering,
        mut lhs: &'a Expr,
        mut rhs: &'a Expr,
        fb: &mut FunctionBuilder,
    ) -> Result<Value> {
        if ordering.is_gt() {
            ordering = Ordering::Less;
            std::mem::swap(&mut lhs, &mut rhs);
        }
        let eq = ordering.is_eq();

        let lhs_type = expr_type(lhs);
        let rhs_type = expr_type(rhs);

        Ok(match (&lhs_type, &rhs_type, eq) {
            (Typ::Double, Typ::Bool, true) | (Typ::Bool, Typ::Double, true) => {
                fb.ins().iconst(I8, 0)
            }
            (Typ::Double, Typ::Bool, false) => {
                let lhs = self.generate_expr(lhs, fb)?.single();
                let rhs = self.generate_expr(rhs, fb)?.single();
                let inf = fb.ins().f64const(f64::INFINITY);
                let is_inf = fb.ins().fcmp(FloatCC::Equal, lhs, inf);
                fb.ins().bor_not(rhs, is_inf)
            }
            (Typ::Bool, Typ::Double, false) => {
                let lhs = self.generate_expr(lhs, fb)?.single();
                let rhs = self.generate_expr(rhs, fb)?.single();
                let inf = fb.ins().f64const(f64::INFINITY);
                let is_inf = fb.ins().fcmp(FloatCC::Equal, rhs, inf);
                fb.ins().bor_not(is_inf, lhs)
            }
            (Typ::Double, Typ::Any, _) | (Typ::Any, Typ::Double, _) => {
                let lhs_is_double = matches!(lhs_type, Typ::Double);
                let (the_double, the_any) = if lhs_is_double {
                    (self.generate_expr(lhs, fb)?, self.generate_expr(rhs, fb)?)
                } else {
                    (self.generate_expr(rhs, fb)?, self.generate_expr(lhs, fb)?)
                };
                let the_any = the_any.pair();
                let inst = self.call_extern(
                    if eq {
                        "any_eq_double"
                    } else if lhs_is_double {
                        "double_lt_any"
                    } else {
                        "any_lt_double"
                    },
                    &[the_any.0, the_any.1, the_double.single()],
                    fb,
                );
                fb.inst_results(inst)[0]
            }
            (Typ::Bool, Typ::Bool, _) | (Typ::Double, Typ::Double, _) => {
                let lhs = self.generate_expr(lhs, fb)?.single();
                let rhs = self.generate_expr(rhs, fb)?.single();
                match (matches!(lhs_type, Typ::Bool), eq) {
                    (true, true) => fb.ins().icmp(IntCC::Equal, lhs, rhs),
                    (true, false) => fb.ins().band_not(rhs, lhs),
                    (false, true) => fb.ins().fcmp(FloatCC::Equal, lhs, rhs),
                    (false, false) => {
                        fb.ins().fcmp(FloatCC::LessThan, lhs, rhs)
                    }
                }
            }
            (Typ::Bool, Typ::StaticStr(_), false) => {
                todo!()
            }
            (Typ::StaticStr(s), Typ::Bool, true)
            | (Typ::Bool, Typ::StaticStr(s), true) => {
                let the_bool = if matches!(lhs_type, Typ::Bool) {
                    lhs
                } else {
                    rhs
                };
                if s.eq_ignore_ascii_case("true") {
                    self.generate_expr(the_bool, fb)?.single()
                } else if s.eq_ignore_ascii_case("false") {
                    let the_bool = self.generate_expr(the_bool, fb)?.single();
                    fb.ins().bxor_imm(the_bool, 1)
                } else {
                    fb.ins().iconst(I8, 0)
                }
            }
            (Typ::StaticStr(_), Typ::Bool, false) => todo!(),
            (Typ::StaticStr(lhs), Typ::StaticStr(rhs), _) => fb.ins().iconst(
                I8,
                i64::from(
                    Immediate::String(lhs.into())
                        .compare(&Immediate::String(rhs.into()))
                        == ordering,
                ),
            ),
            (Typ::Double, Typ::StaticStr(_), _) => todo!(),
            (Typ::Double, Typ::OwnedString, _) => todo!(),
            (Typ::Bool, Typ::OwnedString, _) => todo!(),
            (Typ::StaticStr(_), Typ::Double, _) => todo!(),
            (Typ::OwnedString, Typ::Double, _) => todo!(),
            (Typ::OwnedString, Typ::Bool, _) => todo!(),
            (Typ::OwnedString, Typ::OwnedString, _) => todo!(),
            (Typ::OwnedString, Typ::Any, _) => todo!(),
            (Typ::Any, Typ::OwnedString, _) => todo!(),
            (Typ::StaticStr(_), Typ::OwnedString, _)
            | (Typ::OwnedString, Typ::StaticStr(_), _) => {
                let lhs = self.generate_expr(lhs, fb)?.pair();
                let rhs = self.generate_expr(rhs, fb)?.pair();
                let inst = self.call_extern(
                    if eq { "str_eq_str" } else { "str_lt_str" },
                    &[lhs.0, lhs.1, rhs.0, rhs.1],
                    fb,
                );
                let to_free = if matches!(lhs_type, Typ::OwnedString) {
                    lhs
                } else {
                    rhs
                };
                self.call_extern("free", &[to_free.0], fb);
                fb.inst_results(inst)[0]
            }
            (Typ::StaticStr(_), Typ::Any, _)
            | (Typ::Any, Typ::StaticStr(_), _) => {
                let lhs_is_str = matches!(lhs_type, Typ::StaticStr(_));
                let lhs = self.generate_expr(lhs, fb)?.pair();
                let rhs = self.generate_expr(rhs, fb)?.pair();
                let inst = self.call_extern(
                    if eq {
                        "any_eq_str"
                    } else if lhs_is_str {
                        "str_lt_any"
                    } else {
                        "any_lt_str"
                    },
                    &if eq && lhs_is_str {
                        [rhs.0, rhs.1, lhs.0, lhs.1]
                    } else {
                        [lhs.0, lhs.1, rhs.0, rhs.1]
                    },
                    fb,
                );
                fb.inst_results(inst)[0]
            }
            (Typ::Bool, Typ::Any, _) | (Typ::Any, Typ::Bool, _) => {
                let lhs_is_bool = matches!(lhs_type, Typ::Bool);
                let (the_bool, the_any) =
                    if lhs_is_bool { (lhs, rhs) } else { (rhs, lhs) };
                let the_bool = self.generate_expr(the_bool, fb)?.single();
                let the_any = self.generate_expr(the_any, fb)?.pair();
                let inst = self.call_extern(
                    if eq {
                        "any_eq_bool"
                    } else if lhs_is_bool {
                        "bool_lt_any"
                    } else {
                        "any_lt_bool"
                    },
                    &[the_any.0, the_any.1, the_bool],
                    fb,
                );
                fb.inst_results(inst)[0]
            }
            (Typ::Any, Typ::Any, _) => {
                let lhs = self.generate_expr(lhs, fb)?.pair();
                let rhs = self.generate_expr(rhs, fb)?.pair();
                let inst = self.call_extern(
                    if eq { "any_eq_any" } else { "any_lt_any" },
                    &[lhs.0, lhs.1, rhs.0, rhs.1],
                    fb,
                );
                fb.inst_results(inst)[0]
            }
        })
    }
}

fn pair(values: &[Value]) -> (Value, Value) {
    match values {
        [v0, v1] => (*v0, *v1),
        _ => unimplemented!(),
    }
}
