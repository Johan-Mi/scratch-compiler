use super::{AsmProgram, LocalLabel};
use crate::{
    diagnostic::{Error, Result},
    ir::{expr::Expr, statement::Statement},
    span::Span,
};
use std::fmt::Write as _;

impl<'a> AsmProgram<'a> {
    pub(super) fn generate_statement(
        &mut self,
        stmt: &'a Statement,
    ) -> Result<()> {
        match stmt {
            Statement::ProcCall {
                proc_name,
                args,
                proc_span,
            } => self.generate_proc_call(proc_name, args, *proc_span),
            Statement::Do(stmts) => stmts
                .iter()
                .try_for_each(|stmt| self.generate_statement(stmt)),
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let else_label = LocalLabel(self.new_uid());
                let end_label = LocalLabel(self.new_uid());
                self.generate_bool_expr(condition)?;
                writeln!(
                    self,
                    "    test al, al
    jz {else_label}"
                )
                .unwrap();
                self.generate_statement(if_true)?;
                writeln!(self, "    jmp {end_label}").unwrap();
                self.emit(else_label);
                self.generate_statement(if_false)?;
                self.emit(end_label);
                Ok(())
            }
            Statement::Repeat { times, body } => {
                let loop_label = LocalLabel(self.new_uid());
                let after_loop = LocalLabel(self.new_uid());
                self.generate_double_expr(times)?;
                self.emit(
                    "    call double_to_usize
    push rax",
                );
                self.stack_aligned ^= true;
                self.emit(loop_label);
                writeln!(
                    self,
                    "    sub qword [rsp], 1
    jc {after_loop}"
                )
                .unwrap();
                self.generate_statement(body)?;
                writeln!(self, "    jmp {loop_label}").unwrap();
                self.emit(after_loop);
                self.emit("    add rsp, 8");
                self.stack_aligned ^= true;
                Ok(())
            }
            Statement::Forever(body) => {
                let loop_label = LocalLabel(self.new_uid());
                self.emit(loop_label);
                self.generate_statement(body)?;
                writeln!(self, "    jmp {loop_label}").unwrap();
                Ok(())
            }
            Statement::Until { condition, body }
            | Statement::While { condition, body } => {
                let loop_label = LocalLabel(self.new_uid());
                let after_loop = LocalLabel(self.new_uid());
                let end_condition = if matches!(stmt, Statement::Until { .. }) {
                    "jnz"
                } else {
                    "jz"
                };
                self.emit(loop_label);
                self.generate_bool_expr(condition)?;
                writeln!(
                    self,
                    "    test al, al
    {end_condition} {after_loop}",
                )
                .unwrap();
                self.generate_statement(body)?;
                self.emit("    jmp {loop_label}");
                self.emit(after_loop);
                Ok(())
            }
            Statement::For {
                counter: (counter_name, counter_span),
                times,
                body,
            } => {
                let var_id =
                    self.lookup_var(counter_name).ok_or_else(|| {
                        Box::new(Error::UnknownVar {
                            span: *counter_span,
                            var_name: counter_name.into(),
                        })
                    })?;
                let loop_label = LocalLabel(self.new_uid());
                let after_loop = LocalLabel(self.new_uid());
                self.generate_double_expr(times)?;
                let stack_was_aligned = self.stack_aligned;
                if !stack_was_aligned {
                    self.emit("    sub rsp, 8");
                }
                self.stack_aligned = true;
                self.emit(
                    "    call double_to_usize
    push rax
    push qword 0",
                );
                self.emit(loop_label);
                writeln!(
                    self,
                    "    pop rdi
    cmp rdi, [rsp]
    jae {after_loop}
    inc rdi
    push rdi
    call usize_to_double
    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]
    mov qword [{var_id}], 2
    movsd [{var_id}+8], xmm0
    call drop_any"
                )
                .unwrap();
                self.generate_statement(body)?;
                writeln!(self, "    jmp {loop_label}").unwrap();
                self.emit(after_loop);
                self.emit(if stack_was_aligned {
                    "    add rsp, 8"
                } else {
                    "    add rsp, 16"
                });
                self.stack_aligned = stack_was_aligned;
                Ok(())
            }
        }
    }

    fn generate_proc_call(
        &mut self,
        proc_name: &str,
        args: &'a [Expr],
        span: Span,
    ) -> Result<()> {
        let wrong_arg_count = |expected| {
            Err(Box::new(Error::BuiltinProcWrongArgCount {
                span,
                proc_name: proc_name.to_owned(),
                expected,
                got: args.len(),
            }))
        };

        match proc_name {
            "print" => match args {
                [message] => {
                    if let Expr::Lit(message) = message {
                        let message = message.to_cow_str();
                        let message_len = message.len();
                        let message_id = self.allocate_static_str(message);
                        writeln!(
                            self,
                            "    mov eax, 1
    mov edi, 1
    lea rsi, [{message_id}]
    mov rdx, {message_len}
    syscall"
                        )
                        .unwrap();
                    } else {
                        self.generate_cow_expr(message)?;
                        self.emit(
                            "    mov rsi, rax
    mov eax, 1
    mov edi, 1
    syscall
    mov rdi, rsi",
                        );
                        self.aligning_call("drop_cow");
                    }
                }
                _ => return wrong_arg_count(1),
            },
            ":=" | "+=" => match args {
                [Expr::Sym(var_name, var_span), value] => {
                    let var_id =
                        self.lookup_var(var_name).ok_or_else(|| {
                            Box::new(Error::UnknownVar {
                                span: *var_span,
                                var_name: var_name.clone(),
                            })
                        })?;
                    if proc_name == ":=" {
                        self.generate_any_expr(value)?;
                        writeln!(
                            self,
                            "    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]
    mov [{var_id}], rax
    mov [{var_id}+8], rdx"
                        )
                        .unwrap();
                        self.aligning_call("drop_any");
                    } else {
                        self.generate_double_expr(value)?;
                        writeln!(
                            self,
                            "    sub rsp, 8
    movsd [rsp], xmm0
    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]"
                        )
                        .unwrap();
                        self.stack_aligned ^= true;
                        self.aligning_call("any_to_double");
                        writeln!(
                            self,
                            "    addsd xmm0, [rsp]
    mov qword [{var_id}], 2
    movsd [{var_id}+8], xmm0
    add rsp, 8"
                        )
                        .unwrap();
                        self.stack_aligned ^= true;
                    }
                }
                _ => return wrong_arg_count(2),
            },
            "append" => match args {
                [Expr::Sym(list_name, list_span), value] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    self.generate_any_expr(value)?;
                    writeln!(
                        self,
                        "    lea rdi, [{list_id}]
    mov rsi, rax"
                    )
                    .unwrap();
                    self.aligning_call("list_append");
                }
                _ => return wrong_arg_count(2),
            },
            "delete" => match args {
                [Expr::Sym(list_name, list_span), value] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    self.generate_any_expr(value)?;
                    writeln!(
                        self,
                        "    mov rdi, rax
    mov rsi, rdx
    lea rdx, [{list_id}]"
                    )
                    .unwrap();
                    self.aligning_call("list_delete");
                }
                _ => return wrong_arg_count(2),
            },
            "delete-all" => match args {
                [Expr::Sym(list_name, list_span)] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    writeln!(self, "    lea rdi, [{list_id}]").unwrap();
                    self.aligning_call("list_delete_all");
                }
                _ => return wrong_arg_count(1),
            },
            "replace" => match args {
                [Expr::Sym(list_name, list_span), index, value] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    self.generate_any_expr(value)?;
                    self.emit(
                        "    push rsi
    push rdi",
                    );
                    self.generate_any_expr(index)?;
                    writeln!(
                        self,
                        "    lea r8, [{list_id}]
    pop rdx
    pop rcx"
                    )
                    .unwrap();
                    self.aligning_call("list_replace");
                }
                _ => return wrong_arg_count(3),
            },
            "stop-this-script" => match args {
                [] => {
                    self.emit("    mov rsp, rbp");
                    if let Some(stop_label) = self.proc_stop_label {
                        writeln!(self, "    jmp {stop_label}").unwrap();
                    } else {
                        self.emit(
                            "    pop rbp
    ret",
                        );
                    }
                }
                _ => return wrong_arg_count(0),
            },
            _ => self.generate_custom_proc_call(proc_name, args, span)?,
        }
        Ok(())
    }

    fn generate_custom_proc_call(
        &mut self,
        proc_name: &str,
        args: &'a [Expr],
        span: Span,
    ) -> Result<()> {
        let proc = self.custom_procs.get(proc_name).ok_or_else(|| {
            Error::UnknownProc {
                span,
                proc_name: proc_name.to_owned(),
            }
        })?;
        let proc_id = proc.id;

        if args.len() != proc.params.len() {
            return Err(Box::new(Error::CustomProcWrongArgCount {
                span,
                proc_name: proc_name.to_owned(),
                expected: proc.params.len(),
                got: args.len(),
            }));
        }

        let stack_was_aligned = self.stack_aligned;
        if !stack_was_aligned {
            self.emit("    sub rsp, 8");
        }
        self.stack_aligned = true;

        for arg in args {
            self.generate_any_expr(arg)?;
            self.emit(
                "    push rdx
    push rax",
            );
        }

        writeln!(self, "    call {proc_id}").unwrap();

        if !stack_was_aligned {
            self.emit("    add rsp, 8");
        }
        self.stack_aligned = stack_was_aligned;

        Ok(())
    }
}
