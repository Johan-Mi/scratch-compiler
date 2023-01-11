use super::{
    typ::{StrKnowledge, Typ},
    AsmProgram, LocalLabel,
};
use crate::{
    diagnostic::{Error, Result},
    ir::expr::Expr,
    span::Span,
};
use sb3_stuff::Value;
use std::{borrow::Cow, cmp::Ordering, fmt::Write as _};

impl<'a> AsmProgram<'a> {
    pub(super) fn generate_expr(&mut self, expr: &'a Expr) -> Result<Typ> {
        match expr {
            Expr::Lit(lit) => Ok(self.generate_lit(lit)),
            Expr::Sym(sym, sym_span) => self.generate_symbol(sym, *sym_span),
            Expr::FuncCall(func_name, span, args) => {
                self.generate_func_call(func_name, args, *span)
            }
            Expr::AddSub(positives, negatives) => {
                self.generate_add_sub(positives, negatives)
            }
            Expr::MulDiv(numerators, denominators) => {
                self.generate_mul_div(numerators, denominators)
            }
        }
    }

    fn generate_add_sub(
        &mut self,
        positives: &'a [Expr],
        negatives: &'a [Expr],
    ) -> Result<Typ> {
        match (positives, negatives) {
            ([], []) => self.emit("    xorpd xmm0, xmm0"),
            ([initial, positives @ ..], negatives) => {
                self.generate_double_expr(initial)?;
                self.emit(
                    "    sub rsp, 8
    movsd [rsp], xmm0",
                );
                self.stack_aligned ^= true;
                for term in positives {
                    self.generate_double_expr(term)?;
                    self.emit(
                        "    addsd xmm0, [rsp]
    movsd [rsp], xmm0",
                    );
                }
                for term in negatives {
                    self.generate_double_expr(term)?;
                    self.emit(
                        "    movsd xmm1, [rsp]
    subsd xmm1, xmm0
    movsd [rsp], xmm1",
                    );
                }
                self.emit(
                    "    movsd xmm0, [rsp]
    add rsp, 8",
                );
                self.stack_aligned ^= true;
            }
            ([], [initial, negatives @ ..]) => {
                self.generate_double_expr(initial)?;
                self.emit(
                    "    sub rsp, 8
    movsd [rsp], xmm0",
                );
                self.stack_aligned ^= true;
                for term in negatives {
                    self.generate_double_expr(term)?;
                    self.emit(
                        "    addsd xmm0, [rsp]
    movsd [rsp], xmm0",
                    );
                }
                self.emit(
                    "    mov rax, (1 << 63)
    xor [rsp], rax
    movsd xmm0, [rsp]
    add rsp, 8",
                );
                self.stack_aligned ^= true;
            }
        }
        Ok(Typ::Double)
    }

    fn generate_mul_div(
        &mut self,
        numerators: &'a [Expr],
        denominators: &'a [Expr],
    ) -> Result<Typ> {
        match (numerators, denominators) {
            ([], []) => {
                self.generate_lit(&Value::Num(1.0));
            }
            ([initial, numerators @ ..], denominators) => {
                self.generate_double_expr(initial)?;
                self.emit(
                    "    sub rsp, 8
    movsd [rsp], xmm0",
                );
                self.stack_aligned ^= true;
                for term in numerators {
                    self.generate_double_expr(term)?;
                    self.emit(
                        "    mulsd xmm0, [rsp]
    movsd [rsp], xmm0",
                    );
                }
                for term in denominators {
                    self.generate_double_expr(term)?;
                    self.emit(
                        "    movsd xmm1, [rsp]
    divsd xmm1, xmm0
    movsd [rsp], xmm1",
                    );
                }
                self.emit(
                    "    movsd xmm0, [rsp]
    add rsp, 8",
                );
                self.stack_aligned ^= true;
            }
            ([], [initial, denominators @ ..]) => {
                self.generate_double_expr(initial)?;
                self.emit(
                    "    sub rsp, 8
    movsd [rsp], xmm0",
                );
                self.stack_aligned ^= true;
                for term in denominators {
                    self.generate_double_expr(term)?;
                    self.emit(
                        "    mulsd xmm0, [rsp]
    movsd [rsp], xmm0",
                    );
                }
                self.emit(
                    "    mov rax, __?float64?__(1.0)
    movq xmm0, rax
    divsd xmm0, [rsp]
    add rsp, 8",
                );
                self.stack_aligned ^= true;
            }
        }
        Ok(Typ::Double)
    }

    fn generate_symbol(&mut self, sym: &str, span: Span) -> Result<Typ> {
        if let Some(param_index) =
            self.proc_params.iter().position(|&param| param == sym)
        {
            writeln!(
                self,
                "    mov rdi, [rbp+{}]
    mov rsi, [rbp+{}]",
                (self.proc_params.len() - param_index) * 16,
                (self.proc_params.len() - param_index) * 16 + 8,
            )
            .unwrap();
            self.aligning_call("clone_any");
            Ok(Typ::Any)
        } else if let Some(var_id) = self.lookup_var(sym) {
            writeln!(
                self,
                "    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]"
            )
            .unwrap();
            self.aligning_call("clone_any");
            Ok(Typ::Any)
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
    ) -> Result<Typ> {
        let wrong_arg_count = |expected| {
            Err(Box::new(Error::FunctionWrongArgCount {
                span,
                func_name,
                expected,
                got: args.len(),
            }))
        };

        let mut mathop = |code| match args {
            [operand] => {
                self.generate_double_expr(operand)?;
                self.emit(code);
                Ok(Typ::Double)
            }
            _ => wrong_arg_count(1),
        };

        let libc_mathop = |this: &mut Self, func_name| match args {
            [operand] => {
                this.generate_double_expr(operand)?;
                this.aligning_call(format_args!("{func_name} wrt ..plt"));
                Ok(Typ::Double)
            }
            _ => wrong_arg_count(1),
        };

        match func_name {
            "!!" => match args {
                [Expr::Sym(list_name, list_span), index] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    self.generate_any_expr(index)?;
                    writeln!(
                        self,
                        "    mov rdi, rax
    mov rsi, rdx
    lea rdx, [{list_id}]"
                    )
                    .unwrap();
                    self.aligning_call("list_get");
                    Ok(Typ::Any)
                }
                _ => wrong_arg_count(2),
            },
            "++" => match args {
                [] => {
                    self.emit(
                        "    lea rax, [str_empty]
    xor edx, edx",
                    );
                    Ok(Typ::StaticStr(StrKnowledge::Exact("")))
                }
                [single] => self.generate_expr(single),
                [rest @ .., last] => {
                    self.generate_cow_expr(last)?;
                    let stack_was_aligned = self.stack_aligned;
                    if !stack_was_aligned {
                        self.emit("    sub rsp, 8");
                    }
                    self.stack_aligned = true;
                    for arg in rest.iter().rev() {
                        self.emit(
                            "    push rdx
    sub rsp, 8
    push rdx
    push rax",
                        );
                        self.generate_cow_expr(arg)?;
                        self.emit(
                            "    add [rsp+24], rdx
    push rdx
    push rax
    mov rdi, [rsp+40]
    call malloc wrt ..plt
    mov [rsp+32], rax
    mov rdi, rax
    mov rsi, [rsp]
    mov rdx, [rsp+8]
    call memcpy wrt ..plt
    mov rdi, rax
    add rdi, [rsp+8]
    mov rsi, [rsp+16]
    mov rdx, [rsp+24]
    call memcpy wrt ..plt
    call drop_pop_cow
    call drop_pop_cow
    pop rax
    pop rdx",
                        );
                    }
                    if !stack_was_aligned {
                        self.emit("    add rsp, 8");
                    }
                    self.stack_aligned = stack_was_aligned;
                    Ok(Typ::OwnedString)
                }
            },
            "and" | "or" => match args {
                [] => Ok(self.generate_lit(if func_name == "and" {
                    &Value::Bool(true)
                } else {
                    &Value::Bool(false)
                })),
                [single] => self.generate_expr(single),
                [rest @ .., last] => {
                    let short_circuit = LocalLabel(self.new_uid());
                    let short_circuit_condition =
                        if func_name == "and" { "jz" } else { "jnz" };
                    for arg in rest {
                        self.generate_bool_expr(arg)?;
                        writeln!(
                            self,
                            "    test al, al
    {short_circuit_condition} {short_circuit}",
                        )
                        .unwrap();
                    }
                    self.generate_bool_expr(last)?;
                    self.emit(short_circuit);
                    Ok(Typ::Bool)
                }
            },
            "not" => match args {
                [operand] => {
                    self.generate_bool_expr(operand)?;
                    self.emit("    xor al, 1");
                    Ok(Typ::Bool)
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
                    self.generate_comparison(ordering, lhs, rhs)
                }
                _ => wrong_arg_count(2),
            },
            "length" => match args {
                [Expr::Sym(list_name, list_span)] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    writeln!(
                        self,
                        "    mov rdi, [{list_id}+8]
    call usize_to_double"
                    )
                    .unwrap();
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(1),
            },
            "str-length" => match args {
                [s] => {
                    self.generate_cow_expr(s)?;
                    let stack_was_aligned = self.stack_aligned;
                    self.emit(if stack_was_aligned {
                        "    sub rsp, 8"
                    } else {
                        "    sub rsp, 16"
                    });
                    self.stack_aligned = true;
                    self.emit(
                        "    push rdx
    push rax
    call str_length
    mov rdi, rax
    call usize_to_double
    movsd [rsp+16], xmm0
    call drop_pop_cow
    movsd xmm0, [rsp]",
                    );
                    self.emit(if stack_was_aligned {
                        "    add rsp, 8"
                    } else {
                        "    add rsp, 16"
                    });
                    self.stack_aligned = stack_was_aligned;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(1),
            },
            "char-at" => match args {
                [s, index] => {
                    self.generate_cow_expr(s)?;
                    self.emit(
                        "    sub rsp, 16
    push rdx
    push rax",
                    );
                    self.generate_double_expr(index)?;
                    let stack_was_aligned = self.stack_aligned;
                    if !stack_was_aligned {
                        self.emit("    sub rsp, 8");
                    }
                    self.stack_aligned = true;
                    self.emit(
                        "    call double_to_usize
    mov rdx, rax
    mov rdi, [rsp]
    mov rsi, [rsp+8]
    call char_at
    mov [rsp+16], rax
    mov [rsp+24], rdx
    call drop_pop_cow
    pop rax
    pop rdx",
                    );
                    if !stack_was_aligned {
                        self.emit("    add rsp, 8");
                    }
                    self.stack_aligned = stack_was_aligned;
                    Ok(Typ::OwnedString)
                }
                _ => wrong_arg_count(2),
            },
            "mod" => match args {
                [lhs, rhs] => {
                    self.generate_double_expr(rhs)?;
                    let stack_was_aligned = self.stack_aligned;
                    self.emit(if stack_was_aligned {
                        "    sub rsp, 8"
                    } else {
                        "    sub rsp, 16"
                    });
                    self.stack_aligned = true;
                    self.emit("    movsd [rsp], xmm0");
                    self.generate_double_expr(lhs)?;
                    self.emit(
                        "    movsd xmm1, [rsp]
    call fmod wrt ..plt",
                    );
                    self.emit(if stack_was_aligned {
                        "    add rsp, 8"
                    } else {
                        "    add rsp, 16"
                    });
                    self.stack_aligned = stack_was_aligned;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(2),
            },
            "abs" => mathop(
                "    mov rax, (1 << 63) - 1
    movq xmm1, rax
    andpd xmm0, xmm1",
            ),
            "floor" => mathop("    roundsd xmm0, xmm0, 1"),
            "ceil" => mathop("    roundsd xmm0, xmm0, 2"),
            "sqrt" => mathop("    sqrtsd xmm0, xmm0"),
            "ln" => libc_mathop(self, "log"),
            "log" => libc_mathop(self, "log10"),
            "e^" => libc_mathop(self, "exp"),
            "ten^" => libc_mathop(self, "exp10"),
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" => {
                libc_mathop(self, func_name)
            }
            "pressing-key" => todo!(),
            "to-num" => match args {
                [operand] => {
                    self.generate_double_expr(operand)?;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(1),
            },
            "random" => match args {
                [low, high] => {
                    self.uses_drand48 = true;
                    self.generate_double_expr(high)?;
                    self.emit(
                        "    sub rsp, 8
    movsd [rsp], xmm0",
                    );
                    self.stack_aligned ^= true;
                    self.generate_double_expr(low)?;
                    self.emit("    movsd xmm1, [rsp]");
                    self.emit(if self.stack_aligned {
                        "    call random_between
    add rsp, 8"
                    } else {
                        "    add rsp, 8
    call random_between"
                    });
                    self.stack_aligned ^= true;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(2),
            },
            _ => Err(Box::new(Error::UnknownFunction {
                span,
                func_name: func_name.to_owned(),
            })),
        }
    }

    pub(super) fn generate_bool_expr(&mut self, expr: &'a Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.aligning_call("double_to_bool"),
            Typ::Bool => {}
            Typ::StaticStr(_) => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("static_str_to_bool");
            }
            Typ::OwnedString => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("owned_string_to_bool");
            }
            Typ::Any => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("any_to_bool")
            }
        }
        Ok(())
    }

    pub(super) fn generate_double_expr(
        &mut self,
        expr: &'a Expr,
    ) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => {}
            Typ::Bool => {
                self.emit("mov edi, eax");
                self.aligning_call("bool_to_double")
            }
            Typ::StaticStr(_) => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("str_to_double");
            }
            Typ::OwnedString => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("owned_string_to_double");
            }
            Typ::Any => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("any_to_double");
            }
        }
        Ok(())
    }

    pub(super) fn generate_cow_expr(&mut self, expr: &'a Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.aligning_call("double_to_cow"),
            Typ::Bool => self.aligning_call("bool_to_static_str"),
            Typ::StaticStr(_) | Typ::OwnedString => {}
            Typ::Any => {
                self.emit(
                    "    mov rdi, rax
    mov rsi, rdx",
                );
                self.aligning_call("any_to_cow");
            }
        }
        Ok(())
    }

    pub(super) fn generate_any_expr(&mut self, expr: &'a Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.emit(
                "    movq rdx, xmm0
    mov rax, 2",
            ),
            Typ::Bool | Typ::StaticStr(_) | Typ::OwnedString | Typ::Any => {}
        }
        Ok(())
    }

    fn generate_lit(&mut self, lit: &'a Value) -> Typ<'a> {
        match lit {
            Value::Num(num) => {
                let bits = num.to_bits();
                writeln!(
                    self,
                    "    mov rax, {bits}
    movq xmm0, rax"
                )
                .unwrap();
                Typ::Double
            }
            Value::String(s) => {
                let string_id = self.allocate_static_str(Cow::Borrowed(s));
                writeln!(
                    self,
                    "    lea rax, [{string_id}]
    mov rdx, {}",
                    s.len(),
                )
                .unwrap();
                Typ::StaticStr(StrKnowledge::Exact(s))
            }
            Value::Bool(false) => {
                self.emit("    xor eax, eax");
                Typ::Bool
            }
            Value::Bool(true) => {
                self.emit("    mov eax, 1");
                Typ::Bool
            }
        }
    }

    fn generate_comparison(
        &mut self,
        mut ordering: Ordering,
        mut lhs: &'a Expr,
        mut rhs: &'a Expr,
    ) -> Result<Typ> {
        if ordering.is_gt() {
            ordering = Ordering::Less;
            std::mem::swap(&mut lhs, &mut rhs);
        }

        match self.generate_expr(lhs)? {
            Typ::Double => {
                self.emit(
                    "    sub rsp, 8
    movsd [rsp], xmm0",
                );
                self.stack_aligned ^= true;
                match self.generate_expr(rhs)? {
                    Typ::Double => {
                        let condition =
                            if ordering.is_lt() { 'a' } else { 'e' };
                        writeln!(
                            self,
                            "    xor eax, eax
    ucomisd xmm0, [rsp]
    set{condition} al",
                        )
                        .unwrap();
                    }
                    Typ::Bool => self.emit(if ordering.is_eq() {
                        "    xor eax, eax"
                    } else {
                        "    mov edx, 1
    cmp qword [rsp], __?float64?__(__?Infinity?__)
    cmovne eax, edx"
                    }),
                    Typ::StaticStr(_) => todo!(),
                    Typ::OwnedString => todo!(),
                    Typ::Any => {
                        self.emit(
                            "    movsd xmm0, [rsp]
    mov rdi, rax
    mov rsi, rdx",
                        );
                        self.aligning_call(if ordering.is_eq() {
                            "any_eq_double"
                        } else {
                            "double_lt_any"
                        });
                    }
                }
                self.emit("    add rsp, 8");
                self.stack_aligned ^= true;
            }
            Typ::Bool => {
                self.emit("    push rax");
                self.stack_aligned ^= true;
                match self.generate_expr(rhs)? {
                    Typ::Double => todo!(),
                    Typ::Bool => self.emit(if ordering.is_eq() {
                        "    cmp al, [rsp]
    sete al"
                    } else {
                        "    cmp al, [rsp]
    seta al"
                    }),
                    Typ::StaticStr(_) => todo!(),
                    Typ::OwnedString => todo!(),
                    Typ::Any => {
                        self.emit(
                            "    mov rdi, rax
    mov rsi, rdx
    mov rdx, [rsp]",
                        );
                        self.aligning_call(if ordering.is_eq() {
                            "any_eq_bool"
                        } else {
                            "any_lt_bool"
                        });
                    }
                }
                self.stack_aligned ^= true;
                self.emit("    add rsp, 8");
            }
            Typ::StaticStr(lhs_knowledge) => {
                let lhs_is_true =
                    matches!(lhs_knowledge, StrKnowledge::Exact("true"));
                self.emit(
                    "    push rdx
    push rax",
                );
                match self.generate_expr(rhs)? {
                    Typ::Double => todo!(),
                    Typ::Bool => todo!(),
                    Typ::StaticStr(_) => todo!(),
                    Typ::OwnedString => todo!(),
                    Typ::Any => {
                        if ordering.is_eq() && lhs_is_true {
                            self.emit(
                                "    mov rdi, rax
    mov rsi, rdx
    add rsp, 16",
                            );
                            self.aligning_call("any_eq_true");
                        } else {
                            self.emit(
                                "    mov rcx, rdx
    mov rdx, rax
    pop rdi
    pop rsi",
                            );
                            self.aligning_call(if ordering.is_eq() {
                                "any_eq_str"
                            } else {
                                "any_lt_str"
                            });
                        }
                    }
                }
            }
            Typ::OwnedString => todo!(),
            Typ::Any => {
                self.emit(
                    "    push rdx
    push rax",
                );
                match self.generate_expr(rhs)? {
                    Typ::Double => {
                        self.emit(
                            "pop rdi
    pop rsi",
                        );
                        self.aligning_call(if ordering.is_eq() {
                            "any_eq_double"
                        } else {
                            "any_lt_double"
                        });
                    }
                    Typ::Bool => todo!(),
                    Typ::StaticStr(_) => todo!(),
                    Typ::OwnedString => todo!(),
                    Typ::Any => {
                        self.emit(
                            "    mov rcx, rdx
    mov rdx, rax
    pop rdi
    pop rsi",
                        );
                        self.aligning_call(if ordering.is_eq() {
                            "any_eq_any"
                        } else {
                            "any_lt_any"
                        });
                    }
                }
            }
        }

        Ok(Typ::Bool)
    }
}
