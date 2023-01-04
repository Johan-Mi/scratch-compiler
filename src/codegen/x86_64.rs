mod expr;
mod typ;

use crate::{
    diagnostic::{Error, Result},
    ir::{
        expr::Expr, proc::Procedure, sprite::Sprite, statement::Statement,
        Program,
    },
    span::Span,
    uid::Uid,
};
use std::{
    collections::HashMap,
    fmt::{self, Write as _},
    fs::File,
    io::Write as _,
    iter,
    path::Path,
};

pub fn write_asm_file(program: &Program, path: &Path) -> Result<()> {
    let asm_program = AsmProgram::try_from(program)?;
    let mut file = File::create(path).unwrap();
    write!(file, "{asm_program}").unwrap();

    Ok(())
}

#[derive(Default)]
struct AsmProgram<'a> {
    uid_generator: crate::uid::Generator,
    entry_points: Vec<Uid>,
    text: String,
    local_vars: HashMap<&'a str, Uid>,
    local_lists: HashMap<&'a str, Uid>,
    sprite_vars: HashMap<&'a str, Uid>,
    sprite_lists: HashMap<&'a str, Uid>,
    global_vars: HashMap<&'a str, Uid>,
    global_lists: HashMap<&'a str, Uid>,
    var_ids: Vec<Uid>,
    list_ids: Vec<Uid>,
    static_strs: Vec<(Uid, String)>,
    custom_procs: HashMap<&'a str, CustomProcedure<'a>>,
    proc_stop_label: Option<LocalLabel<Uid>>,
    proc_params: Vec<&'a str>,
    stack_aligned: bool,
}

impl<'a> TryFrom<&'a Program> for AsmProgram<'a> {
    type Error = Box<Error>;

    fn try_from(program: &'a Program) -> Result<Self> {
        let mut this = Self::default();

        this.global_vars = program
            .stage
            .variables
            .iter()
            .map(String::as_str)
            .zip(iter::repeat_with(|| this.new_uid()))
            .collect();
        this.global_lists = program
            .stage
            .lists
            .iter()
            .map(String::as_str)
            .zip(iter::repeat_with(|| this.new_uid()))
            .collect();
        this.var_ids.extend(this.global_vars.values());
        this.list_ids.extend(this.global_lists.values());

        this.generate_sprite(&program.stage)?;
        for sprite in program.sprites.values() {
            this.sprite_vars = sprite
                .variables
                .iter()
                .map(String::as_str)
                .zip(iter::repeat_with(|| this.new_uid()))
                .collect();
            this.sprite_lists = sprite
                .lists
                .iter()
                .map(String::as_str)
                .zip(iter::repeat_with(|| this.new_uid()))
                .collect();
            this.var_ids.extend(this.sprite_vars.values());
            this.list_ids.extend(this.sprite_lists.values());

            this.generate_sprite(sprite)?;
        }

        Ok(this)
    }
}

impl fmt::Write for AsmProgram<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.text.write_str(s)
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.text.write_char(c)
    }
}

impl<'a> AsmProgram<'a> {
    fn new_uid(&self) -> Uid {
        self.uid_generator.new_uid()
    }

    fn emit<T: Emit>(&mut self, t: T) {
        t.emit(self);
    }

    fn generate_sprite(&mut self, sprite: &'a Sprite) -> Result<()> {
        self.custom_procs = sprite
            .procedures
            .iter()
            .map(|(name, proc)| Ok(match &**name {
                "when-flag-clicked" | "when-cloned" | "when-received" => None,
                _ => {
                    let [proc] = &proc[..] else {
                        todo!("duplicate definition of custom procdeure `{name}`");
                    };
                    Some((
                        &**name,
                        CustomProcedure {
                            id: self.new_uid(),
                            params: proc
                                .params
                                .iter()
                                .map(|(param, span)| match param {
                                    Expr::Sym(sym, ..) => {
                                        Ok(&**sym)
                                    }
                                    _ => Err(Box::new(Error::InvalidParameterForCustomProcDef { span: *span })),
                                })
                                .collect::<Result<_>>()?,
                        },
                    ))
                }
            }))
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;

        for (name, procs) in &sprite.procedures {
            for proc in procs {
                self.generate_proc(name, proc)?;
            }
        }

        Ok(())
    }

    fn generate_proc(
        &mut self,
        name: &str,
        proc: &'a Procedure,
    ) -> Result<Uid> {
        self.local_vars = proc
            .variables
            .iter()
            .map(String::as_str)
            .zip(iter::repeat_with(|| self.new_uid()))
            .collect();
        self.var_ids.extend(self.local_vars.values());
        self.local_lists = proc
            .lists
            .iter()
            .map(String::as_str)
            .zip(iter::repeat_with(|| self.new_uid()))
            .collect();
        self.list_ids.extend(self.local_lists.values());
        self.proc_params = Vec::new();

        match name {
            "when-flag-clicked" => {
                assert!(proc.params.is_empty());
                self.proc_stop_label = None;
                let proc_id = self.new_uid();
                self.entry_points.push(proc_id);
                self.emit(Label(proc_id));
                self.emit(
                    "    push rbp
    mov rbp, rsp",
                );
                self.stack_aligned = true;
                self.generate_statement(&proc.body)?;
                self.emit(
                    "    pop rbp
    ret",
                );
                Ok(proc_id)
            }
            "when-cloned" => todo!(),
            "when-received" => todo!(),
            _ => {
                let CustomProcedure {
                    id: proc_id,
                    ref params,
                } = *self.custom_procs.get(name).unwrap();
                self.proc_params = params.clone();
                self.emit(Label(proc_id));
                self.emit(
                    "    push rbp
    mov rbp, rsp",
                );
                self.stack_aligned = true;
                self.proc_stop_label = if proc.params.is_empty() {
                    None
                } else {
                    Some(LocalLabel(self.new_uid()))
                };
                self.generate_statement(&proc.body)?;
                if let Some(stop_label) = self.proc_stop_label {
                    self.emit(stop_label);
                }

                // Drop parameters
                if proc.params.is_empty() {
                    self.emit(
                        "    pop rbp
    ret",
                    );
                } else {
                    for i in 0..proc.params.len() {
                        writeln!(
                            self,
                            "    mov rdi, [rsp+{}]
    mov rsi, [rsp+{}]
    call drop_any",
                            i * 16 + 8,
                            i * 16 + 16
                        )
                        .unwrap();
                    }
                    writeln!(
                        self,
                        "    pop rbp
    ret {}",
                        proc.params.len() * 16
                    )
                    .unwrap();
                }

                Ok(proc_id)
            }
        }
    }

    fn generate_statement(&mut self, stmt: &Statement) -> Result<()> {
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
                    "    test rax, rax
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
                    "call double_to_usize
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
                    "    test rax, rax
    {end_condition} {after_loop}",
                )
                .unwrap();
                self.generate_statement(body)?;
                self.emit("    jmp {loop_label}");
                self.emit(after_loop);
                Ok(())
            }
            Statement::For {
                counter,
                times,
                body,
            } => {
                let var_id = self.lookup_var(&counter.0).unwrap();
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
        args: &[Expr],
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
                        let message_id = self.allocate_static_str(&message);
                        writeln!(
                            self,
                            "    mov rax, 1
    mov rdi, 1
    lea rsi, [{message_id}]
    mov rdx, {}
    syscall",
                            message.len(),
                        )
                        .unwrap();
                    } else {
                        let stack_was_aligned = self.stack_aligned;
                        if !stack_was_aligned {
                            self.emit("    sub rsp, 8");
                        }
                        self.stack_aligned = true;

                        self.generate_cow_expr(message)?;
                        self.emit(
                            "    push rdx
    push rax
    mov rsi, rax
    mov rax, 1
    mov rdi, 1
    syscall
    call drop_pop_cow",
                        );

                        if !stack_was_aligned {
                            self.emit("    add rsp, 8");
                        }
                        self.stack_aligned = stack_was_aligned;
                    }
                }
                _ => return wrong_arg_count(1),
            },
            ":=" => match args {
                [Expr::Sym(var_name, _), value] => {
                    let var_id = self.lookup_var(var_name).unwrap();
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
                        "    lea rdx, [{list_id}]
    mov rdi, rax
    mov rsi, rdx"
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

    fn allocate_static_str(&mut self, s: &str) -> Uid {
        let uid = self.new_uid();
        self.static_strs.push((uid, s.to_owned()));
        uid
    }

    fn lookup_var(&self, name: &str) -> Option<Uid> {
        self.local_vars
            .get(name)
            .or_else(|| self.sprite_vars.get(name))
            .or_else(|| self.global_vars.get(name))
            .copied()
    }

    fn lookup_list(&self, name: &str, span: Span) -> Result<Uid> {
        self.local_lists
            .get(name)
            .or_else(|| self.sprite_lists.get(name))
            .or_else(|| self.global_lists.get(name))
            .copied()
            .ok_or_else(|| {
                Box::new(Error::UnknownList {
                    span,
                    list_name: name.into(),
                })
            })
    }

    fn generate_custom_proc_call(
        &mut self,
        proc_name: &str,
        args: &[Expr],
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

    fn aligning_call(&mut self, proc: impl fmt::Display) {
        if self.stack_aligned {
            writeln!(self, "    call {proc}").unwrap();
        } else {
            writeln!(
                self,
                "    sub rsp, 8
    call {proc}
    add rsp, 8"
            )
            .unwrap();
        }
    }
}

impl fmt::Display for AsmProgram<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            concat!(
                include_str!("x86_64/prelude.s"),
                "\nmain:\n    sub rsp, 8"
            )
        )?;
        for entry_point in &self.entry_points {
            writeln!(f, "    call {entry_point}")?;
        }
        writeln!(
            f,
            "    add rsp, 8
    xor eax, eax
    ret

{}
section .data
align 8",
            self.text,
        )?;
        for var_id in &self.var_ids {
            writeln!(f, "{var_id}: dq 2, 0")?;
        }
        for (str_id, s) in &self.static_strs {
            write!(f, "staticstr {str_id}, db \"\"")?;
            for byte in s.bytes() {
                write!(f, ",{byte}")?;
            }
            writeln!(f)?;
        }
        f.write_str("\nsection .bss\n")?;
        for list_id in &self.list_ids {
            writeln!(f, "{list_id}: resq 3")?;
        }
        Ok(())
    }
}

trait Emit {
    fn emit(self, program: &mut AsmProgram);
}

impl Emit for &str {
    fn emit(self, program: &mut AsmProgram) {
        program.text.push_str(self);
        program.text.push('\n');
    }
}

struct Label<T>(T);

impl<T: fmt::Display> Emit for Label<T> {
    fn emit(self, program: &mut AsmProgram) {
        writeln!(program, "{}:", self.0).unwrap();
    }
}

#[derive(Clone, Copy)]
struct LocalLabel<T>(T);

impl<T: fmt::Display> fmt::Display for LocalLabel<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, ".{}", self.0)
    }
}

impl<T: fmt::Display> Emit for LocalLabel<T> {
    fn emit(self, program: &mut AsmProgram) {
        writeln!(program, "{self}:").unwrap();
    }
}

struct CustomProcedure<'a> {
    id: Uid,
    pub params: Vec<&'a str>,
}
