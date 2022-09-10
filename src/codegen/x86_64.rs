mod typ;

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
use typ::Typ;

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
                self.generate_bool_expr(condition)?;
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
                Ok(())
            }
            Statement::Repeat { times, body } => {
                let loop_label = self.new_uid();
                let after_loop = self.new_uid();
                self.generate_double_expr(times)?;
                self.text.push_str(
                    "    call double_to_usize
    push rax\n",
                );
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
                self.emit(Label(loop_label));
                self.generate_bool_expr(condition)?;
                writeln!(
                    self.text,
                    "    test rax, rax
    {end_condition} {after_loop}",
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
                        self.generate_cow_expr(message)?;
                        self.text.push_str(
                            "    push rdx
    push rax
    mov rsi, rax
    mov rax, 1
    mov rdi, 1
    syscall
    call drop_pop_cow
",
                        );
                    }
                }
                _ => todo!(),
            },
            ":=" => match args {
                [Expr::Sym(var_name, _), value] => {
                    let var_id = self.lookup_var(var_name).unwrap();
                    self.generate_any_expr(value)?;
                    writeln!(
                        self.text,
                        "    push qword [{var_id}+8]
    push qword [{var_id}]
    mov [{var_id}], rax
    mov [{var_id}+8], rdx
    call drop_pop_any"
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> Result<Typ> {
        match expr {
            Expr::Lit(lit) => Ok(self.push_lit(lit)),
            Expr::Sym(sym, _) => self.generate_symbol(sym),
            Expr::FuncCall(func_name, span, args) => {
                self.generate_func_call(func_name, args, *span)
            }
            Expr::AddSub(_, _) => todo!(),
            Expr::MulDiv(_, _) => todo!(),
        }
    }

    fn generate_symbol(&mut self, sym: &str) -> Result<Typ> {
        if let Some(var_id) = self.lookup_var(sym) {
            writeln!(
                self.text,
                "    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]
    call clone_any"
            )
            .unwrap();
            Ok(Typ::Any)
        } else {
            todo!()
        }
    }

    fn generate_func_call(
        &mut self,
        func_name: &str,
        args: &[Expr],
        span: Span,
    ) -> Result<Typ> {
        match func_name {
            "!!" => todo!(),
            "++" => match args {
                [single] => self.generate_expr(single),
                [lhs, rhs] => {
                    self.generate_cow_expr(rhs)?;
                    self.text.push_str(
                        "    push rdx
    sub rsp, 8
    push rdx
    push rax
",
                    );
                    self.generate_cow_expr(lhs)?;
                    self.text.push_str(
                        "    add [rsp+24], rdx
    push rdx
    push rax
    mov rdi, [rsp+40]
    call malloc
    mov [rsp+32], rax
    mov rdi, rax
    mov rsi, [rsp]
    mov rdx, [rsp+8]
    call memcpy
    mov rdi, rax
    add rdi, [rsp+8]
    mov rsi, [rsp+16]
    mov rdx, [rsp+24]
    call memcpy
    call drop_pop_cow
    call drop_pop_cow
    pop rax
    pop rdx
",
                    );
                    Ok(Typ::OwnedString)
                }
                _ => todo!(),
            },
            "and" => todo!(),
            "or" => todo!(),
            "not" => match args {
                [operand] => {
                    self.generate_bool_expr(operand)?;
                    self.text.push_str("    xor rax, 1\n");
                    Ok(Typ::Bool)
                }
                _ => todo!(),
            },
            "=" => todo!(),
            "<" => todo!(),
            ">" => todo!(),
            "length" => todo!(),
            "str-length" => match args {
                [s] => {
                    self.generate_cow_expr(s)?;
                    self.text.push_str(
                        "    sub rsp, 8
    push rdx
    push rax
    call str_length
    mov rdi, rax
    call usize_to_double
    movq [rsp+16], xmm0
    call drop_pop_cow
    movq xmm0, [rsp]
    add rsp, 8
",
                    );
                    Ok(Typ::Double)
                }
                _ => todo!(),
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
                    Ok(Typ::OwnedString)
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
                    self.generate_double_expr(operand)?;
                    Ok(Typ::Double)
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

    fn push_lit(&mut self, lit: &Value) -> Typ {
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
                    "    mov rax, {string_id}
    mov rdx, {}",
                    s.len(),
                )
                .unwrap();
                Typ::StaticStr
            }
            Value::Bool(false) => {
                self.text.push_str("    mov rax, 0\n");
                Typ::Bool
            }
            Value::Bool(true) => {
                self.text.push_str("    mov rax, 1\n");
                Typ::Bool
            }
        }
    }

    fn lookup_var(&self, name: &str) -> Option<Uid> {
        self.local_vars.get(name).copied()
    }

    fn generate_bool_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.text.push_str("    call double_to_bool\n"),
            Typ::Bool => {}
            Typ::StaticStr => {
                self.text.push_str("    call static_str_to_bool\n")
            }
            Typ::OwnedString => {
                self.text.push_str("    call owned_string_to_bool\n")
            }
            Typ::Any => self.text.push_str("    call any_to_bool\n"),
        }
        Ok(())
    }

    fn generate_double_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => {}
            Typ::Bool => self.text.push_str("    call bool_to_double\n"),
            Typ::StaticStr => {
                self.text.push_str("    call static_str_to_double\n")
            }
            Typ::OwnedString => {
                self.text.push_str("    call owned_string_to_double\n")
            }
            Typ::Any => self.text.push_str(
                "    mov rdi, rax
    mov rsi, rdx
    call any_to_double
",
            ),
        }
        Ok(())
    }

    fn generate_cow_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.text.push_str("    call double_to_string\n"),
            Typ::Bool => self.text.push_str("    call bool_to_static_str\n"),
            Typ::StaticStr | Typ::OwnedString => {}
            Typ::Any => self.text.push_str(
                "    mov rdi, rax
    mov rsi, rdx
    call any_to_cow
",
            ),
        }
        Ok(())
    }

    fn generate_any_expr(&mut self, expr: &Expr) -> Result<()> {
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
