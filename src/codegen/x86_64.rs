use crate::{
    diagnostic::{Error, Result},
    ir::{expr::Expr, proc::Procedure, statement::Statement, Program},
    span::Span,
    uid::Uid,
};
use sb3_stuff::Value;
use std::{
    collections::HashMap,
    fmt::{self, Write as _},
    fs::File,
    io::Write as _,
    iter,
    path::Path,
};

pub fn write_asm_file(program: &Program, path: &Path) -> Result<()> {
    let mut asm_program = AsmProgram::default();

    for (name, procs) in iter::once(&program.stage)
        .chain(program.sprites.values())
        .flat_map(|sprite| &sprite.procedures)
    {
        for proc in procs {
            asm_program.generate_proc(name, proc)?;
        }
    }

    let mut file = File::create(path).unwrap();
    write!(file, "{asm_program}").unwrap();

    Ok(())
}

#[derive(Default)]
struct AsmProgram {
    uid_generator: crate::uid::Generator,
    entry_points: Vec<Uid>,
    text: String,
    local_vars: HashMap<String, Uid>,
    var_ids: Vec<Uid>,
}

impl AsmProgram {
    fn new_uid(&self) -> Uid {
        self.uid_generator.new_uid()
    }

    fn emit<T: Emit>(&mut self, t: T) {
        t.emit(self);
    }

    fn generate_proc(&mut self, name: &str, proc: &Procedure) -> Result<Uid> {
        self.local_vars = proc
            .variables
            .iter()
            .map(|name| {
                let uid = self.new_uid();
                self.var_ids.push(uid);
                (name.clone(), uid)
            })
            .collect();

        match name {
            "when-flag-clicked" => {
                assert!(proc.params.is_empty());
                let proc_id = self.new_uid();
                self.entry_points.push(proc_id);
                self.emit(Label(proc_id));
                self.generate_statement(&proc.body)?;
                self.text.push_str("    ret\n");
                Ok(proc_id)
            }
            _ => todo!(),
        }
    }

    fn generate_statement(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::ProcCall {
                proc_name, args, ..
            } => self.generate_proc_call(proc_name, args),
            Statement::Do(stmts) => stmts
                .iter()
                .try_for_each(|stmt| self.generate_statement(stmt)),
            Statement::IfElse {
                condition,
                if_true,
                if_false,
            } => {
                let else_label = self.new_uid();
                let end_label = self.new_uid();
                self.generate_expr(condition)?;
                self.get_bool();
                writeln!(
                    self.text,
                    "    test rax, rax
    jz {else_label}"
                )
                .unwrap();
                self.generate_statement(if_true)?;
                writeln!(self.text, "    jmp {end_label}").unwrap();
                self.emit(Label(else_label));
                self.generate_statement(if_false)?;
                self.emit(Label(end_label));
                self.drop_pop();
                Ok(())
            }
            Statement::Repeat { times, body } => {
                let loop_label = self.new_uid();
                let after_loop = self.new_uid();
                self.text.push_str("    sub rsp, 8\n");
                self.generate_expr(times)?;
                self.get_double();
                self.text.push_str(
                    "    call double_to_usize
    mov [rsp+16], rax\n",
                );
                self.drop_pop();
                self.emit(Label(loop_label));
                writeln!(
                    self.text,
                    "    pop rax
    test rax, rax
    jz {after_loop}
    dec rax
    push rax"
                )
                .unwrap();
                self.generate_statement(body)?;
                writeln!(self.text, "    jmp {loop_label}").unwrap();
                self.emit(Label(after_loop));
                Ok(())
            }
            Statement::Forever(body) => {
                let loop_label = self.new_uid();
                self.emit(Label(loop_label));
                self.generate_statement(body)?;
                writeln!(self.text, "    jmp {loop_label}").unwrap();
                Ok(())
            }
            Statement::Until { condition, body }
            | Statement::While { condition, body } => {
                let loop_label = self.new_uid();
                let after_loop = self.new_uid();
                let end_condition = if matches!(stmt, Statement::Until { .. }) {
                    "jnz"
                } else {
                    "jz"
                };
                self.text.push_str("    sub rsp, 8\n");
                self.emit(Label(loop_label));
                self.generate_expr(condition)?;
                self.get_bool();
                self.text.push_str("    mov [rsp+16], rax\n");
                self.drop_pop();
                writeln!(
                    self.text,
                    "    mov rax, [rsp]
    test rax, rax
    {} {after_loop}",
                    end_condition,
                )
                .unwrap();
                self.generate_statement(body)?;
                self.text.push_str("    jmp {loop_label}\n");
                self.emit(Label(after_loop));
                Ok(())
            }
            Statement::For { .. } => todo!(),
        }
    }

    fn generate_proc_call(
        &mut self,
        proc_name: &str,
        args: &[Expr],
    ) -> Result<()> {
        match proc_name {
            "print" => match args {
                [message] => {
                    if let Expr::Lit(message) = message {
                        let message = message.to_cow_str();
                        let message_id = self.allocate_static_str(&message);
                        writeln!(
                            self.text,
                            "    mov rax, 1
    mov rdi, 1
    mov rsi, {message_id}
    mov rdx, {}
    syscall",
                            message.len(),
                        )
                        .unwrap();
                    } else {
                        self.generate_expr(message)?;
                        self.cowify();
                        self.text.push_str(
                            "    mov rdx, rsi
    mov rsi, rdi
    mov rax, 1
    mov rdi, 1
    syscall
",
                        );
                        self.drop_pop();
                    }
                }
                _ => todo!(),
            },
            ":=" => match args {
                [Expr::Sym(var_name, _), value] => {
                    let var_id = self.lookup_var(var_name).unwrap();
                    writeln!(
                        self.text,
                        "    push qword [{var_id}+8]
    push qword [{var_id}]"
                    )
                    .unwrap();
                    self.generate_expr(value)?;
                    writeln!(
                        self.text,
                        "    pop rax
    mov [{var_id}], rax
    pop rax
    mov [{var_id}+8], rax"
                    )
                    .unwrap();
                    self.drop_pop();
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> Result<()> {
        match expr {
            Expr::Lit(lit) => {
                self.push_lit(lit);
                Ok(())
            }
            Expr::Sym(sym, _) => self.generate_symbol(sym),
            Expr::FuncCall(func_name, span, args) => {
                self.generate_func_call(func_name, args, *span)
            }
            Expr::AddSub(_, _) => todo!(),
            Expr::MulDiv(_, _) => todo!(),
        }
    }

    fn generate_symbol(&mut self, sym: &str) -> Result<()> {
        if let Some(var_id) = self.lookup_var(sym) {
            writeln!(
                self.text,
                "    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]
    call clone_value"
            )
            .unwrap();
            Ok(())
        } else {
            todo!()
        }
    }

    fn generate_func_call(
        &mut self,
        func_name: &str,
        args: &[Expr],
        span: Span,
    ) -> Result<()> {
        match func_name {
            "!!" => todo!(),
            "++" => match args {
                [single] => self.generate_expr(single),
                [lhs, rhs] => {
                    self.text.push_str("    sub rsp, 16\n");
                    self.generate_expr(rhs)?;
                    self.cowify();
                    self.generate_expr(lhs)?;
                    self.cowify();
                    self.text.push_str(
                        "    mov rdi, [rsp+24]
    add rdi, rsi
    mov [rsp+40], rdi
    call malloc
    mov [rsp+32], rax
    mov rdi, rax
    mov rdx, rsi
    mov rsi, [rsp]
    call memcpy
    mov rdi, rax
    add rdi, [rsp+8]
    mov rsi, [rsp+16]
    mov rdx, [rsp+24]
    call memcpy
",
                    );

                    self.drop_pop();
                    self.drop_pop();
                    Ok(())
                }
                _ => todo!(),
            },
            "and" => todo!(),
            "or" => todo!(),
            "not" => match args {
                [operand] => {
                    self.text.push_str("    sub rsp, 16\n");
                    self.generate_expr(operand)?;
                    self.get_bool();
                    self.text.push_str("    mov [rsp+16], rax\n");
                    self.drop_pop();
                    Ok(())
                }
                _ => todo!(),
            },
            "=" => todo!(),
            "<" => todo!(),
            ">" => todo!(),
            "length" => todo!(),
            "str-length" => match args {
                [s] => {
                    self.text.push_str("    sub rsp, 8\n");
                    self.generate_expr(s)?;
                    self.cowify();
                    self.text.push_str(
                        "    call str_length
    mov rdi, rax
    call usize_to_double
    mov [rsp+16], rax
",
                    );
                    self.drop_pop();
                    self.text.push_str("    push 2\n");
                    Ok(())
                }
                _ => todo!(),
            },
            "char-at" => match args {
                [s, index] => {
                    self.text.push_str("    sub rsp, 16\n");
                    self.generate_expr(s)?;
                    self.cowify();
                    self.generate_expr(index)?;
                    self.get_double();
                    self.text.push_str(
                        "    call double_to_usize
    mov rdx, rax
    mov rdi, [rsp+16]
    mov rsi, [rsp+24]
    call char_at
    mov [rsp+32], rax
    mov [rsp+40], rdx\n",
                    );
                    self.drop_pop();
                    self.drop_pop();
                    Ok(())
                }
                _ => todo!(),
            },
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
            "pressing-key" => todo!(),
            "to-num" => match args {
                [operand] => {
                    self.text.push_str("    sub rsp, 8\n");
                    self.generate_expr(operand)?;
                    self.get_double();
                    self.text.push_str("    movq [rsp+16], xmm0\n");
                    self.drop_pop();
                    self.text.push_str("    push 2\n");
                    Ok(())
                }
                _ => todo!(),
            },
            "random" => todo!(),
            _ => Err(Box::new(Error::UnknownFunction {
                span,
                func_name: func_name.to_owned(),
            })),
        }
    }

    fn allocate_static_str(&mut self, s: &str) -> Uid {
        let uid = self.new_uid();
        write!(self.text, "staticstr {uid}, db ").unwrap();
        for (i, byte) in s.bytes().enumerate() {
            if i == 0 {
                write!(self.text, "{byte}").unwrap();
            } else {
                write!(self.text, ",{byte}").unwrap();
            }
        }
        self.text.push('\n');
        uid
    }

    fn push_lit(&mut self, lit: &Value) {
        match lit {
            Value::Num(num) => {
                let bits = num.to_bits();
                writeln!(
                    self.text,
                    "    mov rax, {bits}
    push rax
    push 2",
                )
                .unwrap();
            }
            Value::String(s) => {
                let string_id = self.allocate_static_str(s);
                let len = s.len();
                if let Ok(len) = u32::try_from(len) {
                    writeln!(self.text, "    push {len}").unwrap();
                } else {
                    writeln!(
                        self.text,
                        "    mov rax, {len}
    push rax"
                    )
                    .unwrap();
                }
                writeln!(
                    self.text,
                    "    mov rax, {string_id}
    push rax",
                )
                .unwrap();
            }
            Value::Bool(false) => {
                writeln!(
                    self.text,
                    "    push 0
    push 0",
                )
                .unwrap();
            }
            Value::Bool(true) => {
                writeln!(
                    self.text,
                    "    push 0
    push 1",
                )
                .unwrap();
            }
        }
    }

    fn cowify(&mut self) {
        self.text.push_str("    call cowify\n");
    }

    fn drop_pop(&mut self) {
        self.text.push_str("    call drop_pop\n");
    }

    fn get_bool(&mut self) {
        self.text.push_str("    call get_bool\n");
    }

    fn get_double(&mut self) {
        self.text.push_str("    call get_double\n");
    }

    fn lookup_var(&self, name: &str) -> Option<Uid> {
        self.local_vars.get(name).copied()
    }
}

impl fmt::Display for AsmProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, concat!(include_str!("x86_64/prelude.s"), "\nmain:\n"))?;
        for entry_point in &self.entry_points {
            writeln!(f, "    call {entry_point}")?;
        }
        writeln!(
            f,
            r#"    mov rax, 60
    mov rdi, 0
    syscall

{}
section .data
align 8"#,
            self.text,
        )?;
        for var_id in &self.var_ids {
            writeln!(f, "{var_id}: dq 2, 0")?;
        }
        Ok(())
    }
}

trait Emit {
    fn emit(self, program: &mut AsmProgram);
}

struct Label<T>(T);

impl<T: fmt::Display> Emit for Label<T> {
    fn emit(self, program: &mut AsmProgram) {
        writeln!(program.text, "{}:", self.0).unwrap();
    }
}
