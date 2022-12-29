use super::{typ::Typ, AsmProgram, LocalLabel};
use crate::{
    diagnostic::{Error, Result},
    ir::expr::Expr,
    span::Span,
};
use sb3_stuff::Value;
use std::fmt::Write as _;

impl AsmProgram {
    pub(super) fn generate_expr(&mut self, expr: &Expr) -> Result<Typ> {
        match expr {
            Expr::Lit(lit) => Ok(self.generate_lit(lit)),
            Expr::Sym(sym, sym_span) => self.generate_symbol(sym, *sym_span),
            Expr::FuncCall(func_name, span, args) => {
                self.generate_func_call(func_name, args, *span)
            }
            Expr::AddSub(positives, negatives) => {
                match (&positives[..], &negatives[..]) {
                    ([], []) => {
                        self.generate_lit(&Value::Num(0.0));
                    }
                    ([initial, positives @ ..], negatives) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movsd [rsp], xmm0
",
                        );
                        self.stack_aligned ^= true;
                        for term in positives {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    addsd xmm0, [rsp]
    movsd [rsp], xmm0
",
                            );
                        }
                        for term in negatives {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    movsd xmm1, [rsp]
    subsd xmm1, xmm0
    movsd [rsp], xmm1
",
                            );
                        }
                        self.text.push_str(
                            "    movsd xmm0, [rsp]
    add rsp, 8
",
                        );
                        self.stack_aligned ^= true;
                    }
                    ([], [initial, negatives @ ..]) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movsd [rsp], xmm0
",
                        );
                        self.stack_aligned ^= true;
                        for term in negatives {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    addsd xmm0, [rsp]
    movsd [rsp], xmm0
",
                            );
                        }
                        self.text.push_str(
                            "    mov rax, (1 << 63)
    xor [rsp], rax
    movsd xmm0, [rsp]
    add rsp, 8
",
                        );
                        self.stack_aligned ^= true;
                    }
                }
                Ok(Typ::Double)
            }
            Expr::MulDiv(numerators, denominators) => {
                match (&numerators[..], &denominators[..]) {
                    ([], []) => {
                        self.generate_lit(&Value::Num(1.0));
                    }
                    ([initial, numerators @ ..], denominators) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movsd [rsp], xmm0
",
                        );
                        self.stack_aligned ^= true;
                        for term in numerators {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    mulsd xmm0, [rsp]
    movsd [rsp], xmm0
",
                            );
                        }
                        for term in denominators {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    movsd xmm1, [rsp]
    divsd xmm1, xmm0
    movsd [rsp], xmm1
",
                            );
                        }
                        self.text.push_str(
                            "    movsd xmm0, [rsp]
    add rsp, 8
",
                        );
                        self.stack_aligned ^= true;
                    }
                    ([], [initial, denominators @ ..]) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movsd [rsp], xmm0
",
                        );
                        self.stack_aligned ^= true;
                        for term in denominators {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    mulsd xmm0, [rsp]
    movsd [rsp], xmm0
",
                            );
                        }
                        self.text.push_str(
                            "    mov rax, __?float64?__(1.0)
    movq xmm0, rax
    divsd xmm0, [rsp]
    add rsp, 8
",
                        );
                        self.stack_aligned ^= true;
                    }
                }
                Ok(Typ::Double)
            }
        }
    }

    fn generate_symbol(&mut self, sym: &str, span: Span) -> Result<Typ> {
        if let Some(var_id) = self.lookup_var(sym) {
            writeln!(
                self.text,
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
        args: &[Expr],
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
                self.text.push_str(code);
                Ok(Typ::Double)
            }
            _ => wrong_arg_count(1),
        };

        let libc_mathop = |this: &mut Self, func_name| match args {
            [operand] => {
                this.generate_double_expr(operand)?;
                writeln!(this.text, "    call {func_name} wrt ..plt").unwrap();
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
                        self.text,
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
                    self.text.push_str(
                        "    lea rax, [str_empty]
    xor edx, edx
",
                    );
                    Ok(Typ::StaticStr)
                }
                [single] => self.generate_expr(single),
                [rest @ .., last] => {
                    self.generate_cow_expr(last)?;
                    for arg in rest.iter().rev() {
                        let stack_was_aligned = self.stack_aligned;
                        if !stack_was_aligned {
                            self.text.push_str("    sub rsp, 8\n");
                        }
                        self.stack_aligned = true;
                        self.text.push_str(
                            "    push rdx
    sub rsp, 8
    push rdx
    push rax
",
                        );
                        self.generate_cow_expr(arg)?;
                        self.text.push_str(
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
    pop rdx
",
                        );
                        if !stack_was_aligned {
                            self.text.push_str("    add rsp, 8\n");
                        }
                        self.stack_aligned = stack_was_aligned;
                    }
                    Ok(Typ::OwnedString)
                }
            },
            "and" | "or" => match args {
                [] => Ok(self.generate_lit(&Value::Bool(func_name == "and"))),
                [single] => self.generate_expr(single),
                [rest @ .., last] => {
                    let short_circuit = LocalLabel(self.new_uid());
                    let short_circuit_condition =
                        if func_name == "and" { "jz" } else { "jnz" };
                    for arg in rest {
                        self.generate_bool_expr(arg)?;
                        writeln!(
                            self.text,
                            "    test rax, rax
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
                    self.text.push_str("    xor rax, 1\n");
                    Ok(Typ::Bool)
                }
                _ => wrong_arg_count(1),
            },
            "=" => match args {
                [lhs, rhs] => {
                    match self.generate_expr(lhs)? {
                        Typ::Double => {
                            self.text.push_str(
                                "    sub rsp, 8
    movsd [rsp], xmm0
",
                            );
                            self.stack_aligned ^= true;
                            match self.generate_expr(rhs)? {
                                Typ::Double => self.text.push_str(
                                    "    movsd xmm1, [rsp]
    xor eax, eax
    ucomisd xmm0, xmm1
    sete al
",
                                ),
                                Typ::Bool => {
                                    self.text.push_str("    xor eax, eax\n");
                                }
                                Typ::StaticStr => todo!(),
                                Typ::OwnedString => todo!(),
                                Typ::Any => todo!(),
                            }
                            self.text.push_str("    add rsp, 8\n");
                            self.stack_aligned ^= true;
                        }
                        Typ::Bool => todo!(),
                        Typ::StaticStr => todo!(),
                        Typ::OwnedString => todo!(),
                        Typ::Any => todo!(),
                    };
                    Ok(Typ::Bool)
                }
                _ => wrong_arg_count(2),
            },
            "<" | ">" => match args {
                [lhs, rhs] => {
                    match self.generate_expr(lhs)? {
                        Typ::Double => {
                            self.text.push_str(
                                "    sub rsp, 8
    movsd [rsp], xmm0
",
                            );
                            self.stack_aligned ^= true;
                            match self.generate_expr(rhs)? {
                                Typ::Double => {
                                    let condition = if func_name == "<" {
                                        'b'
                                    } else {
                                        'a'
                                    };
                                    writeln!(
                                        self.text,
                                        "    movsd xmm1, [rsp]
    xor eax, eax
    ucomisd xmm1, xmm0
    set{condition} al",
                                    )
                                    .unwrap();
                                }
                                Typ::Bool => todo!(),
                                Typ::StaticStr => todo!(),
                                Typ::OwnedString => todo!(),
                                Typ::Any => todo!(),
                            }
                            self.text.push_str("    add rsp, 8\n");
                            self.stack_aligned ^= true;
                        }
                        Typ::Bool => todo!(),
                        Typ::StaticStr => todo!(),
                        Typ::OwnedString => todo!(),
                        Typ::Any => todo!(),
                    };
                    Ok(Typ::Bool)
                }
                _ => wrong_arg_count(2),
            },
            "length" => match args {
                [Expr::Sym(list_name, list_span)] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    writeln!(self.text, "    mov rdi, [{list_id}+8]").unwrap();
                    self.aligning_call("usize_to_double");
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(1),
            },
            "str-length" => match args {
                [s] => {
                    self.generate_cow_expr(s)?;
                    let stack_was_aligned = self.stack_aligned;
                    self.text.push_str(if stack_was_aligned {
                        "    sub rsp, 8\n"
                    } else {
                        "    sub rsp, 16\n"
                    });
                    self.stack_aligned = true;
                    self.text.push_str(
                        "    push rdx
    push rax
    call str_length
    mov rdi, rax
    call usize_to_double
    movsd [rsp+16], xmm0
    call drop_pop_cow
    movsd xmm0, [rsp]
",
                    );
                    self.text.push_str(if stack_was_aligned {
                        "    add rsp, 8\n"
                    } else {
                        "    add rsp, 16\n"
                    });
                    self.stack_aligned = stack_was_aligned;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(1),
            },
            "char-at" => match args {
                [s, index] => {
                    self.generate_cow_expr(s)?;
                    self.text.push_str(
                        "    sub rsp, 16
    push rdx
    push rax
",
                    );
                    self.generate_double_expr(index)?;
                    let stack_was_aligned = self.stack_aligned;
                    if !stack_was_aligned {
                        self.text.push_str("    sub rsp, 8\n");
                    }
                    self.stack_aligned = true;
                    self.text.push_str(
                        "    call double_to_usize
    mov rdx, rax
    mov rdi, [rsp]
    mov rsi, [rsp+8]
    call char_at
    mov [rsp+16], rax
    mov [rsp+24], rdx
    call drop_pop_cow
    pop rax
    pop rdx
",
                    );
                    if !stack_was_aligned {
                        self.text.push_str("    add rsp, 8\n");
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
                    self.text.push_str(if stack_was_aligned {
                        "    sub rsp, 8\n"
                    } else {
                        "    sub rsp, 16\n"
                    });
                    self.stack_aligned = true;
                    self.text.push_str("    movsd [rsp], xmm0\n");
                    self.generate_double_expr(lhs)?;
                    self.text.push_str(
                        "    movsd xmm1, [rsp]
    call fmod
",
                    );
                    self.text.push_str(if stack_was_aligned {
                        "    add rsp, 8\n"
                    } else {
                        "    add rsp, 16\n"
                    });
                    self.stack_aligned = stack_was_aligned;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(2),
            },
            "abs" => mathop(
                "    mov rax, (1 << 63) - 1
    movq xmm1, rax
    andpd xmm0, xmm1
",
            ),
            "floor" => mathop("    roundsd xmm0, xmm0, 1\n"),
            "ceil" => mathop("    roundsd xmm0, xmm0, 2\n"),
            "sqrt" => mathop("    sqrtsd xmm0, xmm0\n"),
            "ln" => libc_mathop(self, "log"),
            "log" => libc_mathop(self, "log10"),
            "e^" => libc_mathop(self, "exp"),
            "ten^" => libc_mathop(self, "exp10"),
            "sin" => libc_mathop(self, "sin"),
            "cos" => libc_mathop(self, "cos"),
            "tan" => libc_mathop(self, "tan"),
            "asin" => libc_mathop(self, "asin"),
            "acos" => libc_mathop(self, "acos"),
            "atan" => libc_mathop(self, "atan"),
            "pressing-key" => todo!(),
            "to-num" => match args {
                [operand] => {
                    self.generate_double_expr(operand)?;
                    Ok(Typ::Double)
                }
                _ => wrong_arg_count(1),
            },
            "random" => todo!(),
            _ => Err(Box::new(Error::UnknownFunction {
                span,
                func_name: func_name.to_owned(),
            })),
        }
    }

    pub(super) fn generate_bool_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.aligning_call("double_to_bool"),
            Typ::Bool => {}
            Typ::StaticStr => {
                self.aligning_call("static_str_to_bool");
            }
            Typ::OwnedString => {
                self.aligning_call("owned_string_to_bool");
            }
            Typ::Any => self.aligning_call("any_to_bool"),
        }
        Ok(())
    }

    pub(super) fn generate_double_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => {}
            Typ::Bool => self.aligning_call("bool_to_double"),
            Typ::StaticStr => {
                self.aligning_call("static_str_to_double");
            }
            Typ::OwnedString => {
                self.aligning_call("owned_string_to_double");
            }
            Typ::Any => {
                self.text.push_str(
                    "    mov rdi, rax
    mov rsi, rdx
",
                );
                self.aligning_call("any_to_double")
            }
        }
        Ok(())
    }

    pub(super) fn generate_cow_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.aligning_call("double_to_cow"),
            Typ::Bool => self.aligning_call("bool_to_static_str"),
            Typ::StaticStr | Typ::OwnedString => {}
            Typ::Any => {
                self.text.push_str(
                    "    mov rdi, rax
    mov rsi, rdx
",
                );
                self.aligning_call("any_to_cow")
            }
        }
        Ok(())
    }

    pub(super) fn generate_any_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.text.push_str(
                "    movq rdx, xmm0
    mov rax, 2
",
            ),
            Typ::Bool | Typ::StaticStr | Typ::OwnedString | Typ::Any => {}
        }
        Ok(())
    }

    fn generate_lit(&mut self, lit: &Value) -> Typ {
        match lit {
            Value::Num(num) => {
                let bits = num.to_bits();
                writeln!(
                    self.text,
                    "    mov rax, {bits}
    movq xmm0, rax"
                )
                .unwrap();
                Typ::Double
            }
            Value::String(s) => {
                let string_id = self.allocate_static_str(s);
                writeln!(
                    self.text,
                    "    lea rax, [{string_id}]
    mov rdx, {}",
                    s.len(),
                )
                .unwrap();
                Typ::StaticStr
            }
            Value::Bool(false) => {
                self.text.push_str("    xor eax, eax\n");
                Typ::Bool
            }
            Value::Bool(true) => {
                self.text.push_str("    mov eax, 1\n");
                Typ::Bool
            }
        }
    }
}
