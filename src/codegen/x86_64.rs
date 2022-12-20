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
use sb3_stuff::Value;
use smol_str::SmolStr;
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
    let asm_program = AsmProgram::try_from(program)?;
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
    local_lists: HashMap<String, Uid>,
    sprite_vars: HashMap<String, Uid>,
    sprite_lists: HashMap<String, Uid>,
    global_vars: HashMap<String, Uid>,
    global_lists: HashMap<String, Uid>,
    var_ids: Vec<Uid>,
    list_ids: Vec<Uid>,
    static_strs: Vec<(Uid, String)>,
    custom_procs: HashMap<String, CustomProcedure>,
    proc_stop_label: Option<Uid>,
}

impl TryFrom<&Program> for AsmProgram {
    type Error = Box<Error>;

    fn try_from(program: &Program) -> Result<Self> {
        let mut this = Self::default();

        this.global_vars = program
            .stage
            .variables
            .iter()
            .cloned()
            .zip(iter::repeat_with(|| this.new_uid()))
            .collect();
        this.global_lists = program
            .stage
            .lists
            .iter()
            .cloned()
            .zip(iter::repeat_with(|| this.new_uid()))
            .collect();
        this.var_ids.extend(this.global_vars.values());
        this.list_ids.extend(this.global_lists.values());

        this.generate_sprite(&program.stage)?;
        for sprite in program.sprites.values() {
            this.sprite_vars = sprite
                .variables
                .iter()
                .cloned()
                .zip(iter::repeat_with(|| this.new_uid()))
                .collect();
            this.sprite_lists = sprite
                .lists
                .iter()
                .cloned()
                .zip(iter::repeat_with(|| this.new_uid()))
                .collect();
            this.var_ids.extend(this.sprite_vars.values());
            this.list_ids.extend(this.sprite_lists.values());

            this.generate_sprite(sprite)?;
        }

        Ok(this)
    }
}

impl AsmProgram {
    fn new_uid(&self) -> Uid {
        self.uid_generator.new_uid()
    }

    fn emit<T: Emit>(&mut self, t: T) {
        t.emit(self);
    }

    fn generate_sprite(&mut self, sprite: &Sprite) -> Result<()> {
        self.custom_procs = sprite
            .procedures
            .iter()
            .filter_map(|(name, proc)| match &**name {
                "when-flag-clicked" | "when-cloned" | "when-received" => None,
                _ => {
                    assert_eq!(
                        1,
                        proc.len(),
                        "duplicate definition of custom procdeure `{name}`"
                    );
                    Some((
                        name.into(),
                        CustomProcedure {
                            id: self.new_uid(),
                            params: proc[0]
                                .params
                                .iter()
                                .map(|param| match param {
                                    Expr::Sym(sym, ..) => {
                                        (sym.clone(), self.new_uid())
                                    }
                                    _ => todo!(
                                        "invalid parameter to custom\
                                    procedure definition:\n{param:#?}"
                                    ),
                                })
                                .collect(),
                        },
                    ))
                }
            })
            .collect();

        for (name, procs) in &sprite.procedures {
            for proc in procs {
                self.generate_proc(name, proc)?;
            }
        }

        Ok(())
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
        self.local_lists = proc
            .lists
            .iter()
            .map(|name| {
                let uid = self.new_uid();
                self.list_ids.push(uid);
                (name.clone(), uid)
            })
            .collect();

        match name {
            "when-flag-clicked" => {
                assert!(proc.params.is_empty());
                self.proc_stop_label = None;
                let proc_id = self.new_uid();
                self.entry_points.push(proc_id);
                self.emit(Label(proc_id));
                self.generate_statement(&proc.body)?;
                self.text.push_str("    ret\n");
                Ok(proc_id)
            }
            "when-cloned" => todo!(),
            "when-received" => todo!(),
            _ => {
                let proc_id = self.custom_procs.get(name).as_ref().unwrap().id;
                self.emit(Label(proc_id));
                self.text.push_str(
                    "    push rbp
    mov rbp, rsp
",
                );
                let stop_label = self.new_uid();
                self.proc_stop_label = Some(stop_label);
                self.generate_statement(&proc.body)?;
                self.emit(LocalLabel(stop_label));
                self.text.push_str("    pop rbp\n");

                // Drop parameters
                if let Some(drop_calls_needed) =
                    proc.params.len().checked_sub(1)
                {
                    for _ in 0..drop_calls_needed {
                        self.text.push_str("    call drop_pop_any\n");
                    }
                    // Use a tail call for the final parameter
                    self.text.push_str("    jmp drop_pop_any\n");
                } else {
                    self.text.push_str("    ret\n");
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
                    self.text,
                    "    test rax, rax
    jz {else_label}"
                )
                .unwrap();
                self.generate_statement(if_true)?;
                writeln!(self.text, "    jmp {end_label}").unwrap();
                self.emit(else_label);
                self.generate_statement(if_false)?;
                self.emit(end_label);
                Ok(())
            }
            Statement::Repeat { times, body } => {
                let loop_label = LocalLabel(self.new_uid());
                let after_loop = LocalLabel(self.new_uid());
                self.generate_double_expr(times)?;
                self.text.push_str(
                    "    call double_to_usize
    push rax\n",
                );
                self.emit(loop_label);
                writeln!(
                    self.text,
                    "    sub qword [rsp], 1
    jc {after_loop}"
                )
                .unwrap();
                self.generate_statement(body)?;
                writeln!(self.text, "    jmp {loop_label}").unwrap();
                self.emit(after_loop);
                self.text.push_str("    add rsp, 8\n");
                Ok(())
            }
            Statement::Forever(body) => {
                let loop_label = LocalLabel(self.new_uid());
                self.emit(loop_label);
                self.generate_statement(body)?;
                writeln!(self.text, "    jmp {loop_label}").unwrap();
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
                    self.text,
                    "    test rax, rax
    {end_condition} {after_loop}",
                )
                .unwrap();
                self.generate_statement(body)?;
                self.text.push_str("    jmp {loop_label}\n");
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
                self.text.push_str(
                    "    call double_to_usize
    push rax
    push qword 0
",
                );
                self.emit(loop_label);
                writeln!(
                    self.text,
                    "    pop rdi
    cmp rdi, [rsp]
    jae {after_loop}
    inc rdi
    push rdi
    push qword [{var_id}]
    mov qword [{var_id}], 2
    call usize_to_double
    movq [{var_id}+8], xmm0
    pop rdi
    call drop_any"
                )
                .unwrap();
                self.generate_statement(body)?;
                writeln!(self.text, "    jmp {loop_label}").unwrap();
                self.emit(after_loop);
                self.text.push_str("    add rsp, 8\n");
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
    lea rsi, [{message_id}]
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
                        "    mov rdi, [{var_id}]
    mov rsi, [{var_id}+8]
    mov [{var_id}], rax
    mov [{var_id}+8], rdx
    call drop_any"
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            "append" => match args {
                [Expr::Sym(list_name, list_span), value] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    self.generate_any_expr(value)?;
                    writeln!(
                        self.text,
                        "    lea rdi, [{list_id}]
    mov rsi, rax
    call list_append"
                    )
                    .unwrap();
                }
                _ => todo!(),
            },
            "stop-this-script" => match args {
                [] => {
                    self.text.push_str("    mov rsp, rbp\n");
                    if let Some(stop_label) = self.proc_stop_label {
                        writeln!(self.text, "    jmp .{stop_label}").unwrap();
                    } else {
                        self.text.push_str("    ret\n");
                    }
                }
                _ => todo!(),
            },
            _ => self.generate_custom_proc_call(proc_name, args, span)?,
        }
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> Result<Typ> {
        match expr {
            Expr::Lit(lit) => Ok(self.push_lit(lit)),
            Expr::Sym(sym, sym_span) => self.generate_symbol(sym, *sym_span),
            Expr::FuncCall(func_name, span, args) => {
                self.generate_func_call(func_name, args, *span)
            }
            Expr::AddSub(positives, negatives) => {
                match (&positives[..], &negatives[..]) {
                    ([], []) => {
                        self.push_lit(&Value::Num(0.0));
                    }
                    ([initial, positives @ ..], negatives) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movq [rsp], xmm0
",
                        );
                        for term in positives {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    addsd xmm0, [rsp]
    movq [rsp], xmm0
",
                            );
                        }
                        for term in negatives {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    movq xmm1, [rsp]
    subsd xmm1, xmm0
    movq [rsp], xmm1
",
                            );
                        }
                        self.text.push_str(
                            "    movq xmm0, [rsp]
    add rsp, 8
",
                        );
                    }
                    ([], [initial, negatives @ ..]) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movq [rsp], xmm0
",
                        );
                        for term in negatives {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    addsd xmm0, [rsp]
    movq [rsp], xmm0
",
                            );
                        }
                        self.text.push_str(
                            "    mov rax, (1 << 63)
    xor [rsp], rax
    movq xmm0, [rsp]
    add rsp, 8
",
                        );
                    }
                }
                Ok(Typ::Double)
            }
            Expr::MulDiv(numerators, denominators) => {
                match (&numerators[..], &denominators[..]) {
                    ([], []) => {
                        self.push_lit(&Value::Num(1.0));
                    }
                    ([initial, numerators @ ..], denominators) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movq [rsp], xmm0
",
                        );
                        for term in numerators {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    mulsd xmm0, [rsp]
    movq [rsp], xmm0
",
                            );
                        }
                        for term in denominators {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    movq xmm1, [rsp]
    divsd xmm1, xmm0
    movq [rsp], xmm1
",
                            );
                        }
                        self.text.push_str(
                            "    movq xmm0, [rsp]
    add rsp, 8
",
                        );
                    }
                    ([], [initial, denominators @ ..]) => {
                        self.generate_double_expr(initial)?;
                        self.text.push_str(
                            "    sub rsp, 8
    movq [rsp], xmm0
",
                        );
                        for term in denominators {
                            self.generate_double_expr(term)?;
                            self.text.push_str(
                                "    mulsd xmm0, [rsp]
    movq [rsp], xmm0
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
    mov rsi, [{var_id}+8]
    call clone_any"
            )
            .unwrap();
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
        func_name: &str,
        args: &[Expr],
        span: Span,
    ) -> Result<Typ> {
        let mut mathop = |code| match args {
            [operand] => {
                self.generate_double_expr(operand)?;
                self.text.push_str(code);
                Ok(Typ::Double)
            }
            _ => todo!(),
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
    lea rdx, [{list_id}]
    call list_get"
                    )
                    .unwrap();
                    Ok(Typ::Any)
                }
                _ => todo!(),
            },
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
                    Ok(Typ::OwnedString)
                }
                _ => todo!(),
            },
            "and" | "or" => match args {
                [] => Ok(self.push_lit(&Value::Bool(func_name == "and"))),
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
                _ => todo!(),
            },
            "=" => match args {
                [lhs, rhs] => {
                    match self.generate_expr(lhs)? {
                        Typ::Double => {
                            self.text.push_str(
                                "    sub rsp, 8
    movq [rsp], xmm0
",
                            );
                            match self.generate_expr(rhs)? {
                                Typ::Double => self.text.push_str(
                                    "    movq xmm1, [rsp]
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
                        }
                        Typ::Bool => todo!(),
                        Typ::StaticStr => todo!(),
                        Typ::OwnedString => todo!(),
                        Typ::Any => todo!(),
                    };
                    Ok(Typ::Bool)
                }
                _ => todo!(),
            },
            "<" | ">" => match args {
                [lhs, rhs] => {
                    match self.generate_expr(lhs)? {
                        Typ::Double => {
                            self.text.push_str(
                                "    sub rsp, 8
    movq [rsp], xmm0
",
                            );
                            match self.generate_expr(rhs)? {
                                Typ::Double => {
                                    let condition = if func_name == "<" {
                                        'b'
                                    } else {
                                        'a'
                                    };
                                    writeln!(
                                        self.text,
                                        "    movq xmm1, [rsp]
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
                        }
                        Typ::Bool => todo!(),
                        Typ::StaticStr => todo!(),
                        Typ::OwnedString => todo!(),
                        Typ::Any => todo!(),
                    };
                    Ok(Typ::Bool)
                }
                _ => todo!(),
            },
            "length" => match args {
                [Expr::Sym(list_name, list_span)] => {
                    let list_id = self.lookup_list(list_name, *list_span)?;
                    writeln!(
                        self.text,
                        "    mov rdi, [{list_id}+8]
    call usize_to_double"
                    )
                    .unwrap();
                    Ok(Typ::Double)
                }
                _ => todo!(),
            },
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
            "abs" => mathop(
                "    mov rax, (1 << 63) - 1
    movq xmm1, rax
    andpd xmm0, xmm1
",
            ),
            "floor" => mathop("    roundsd xmm0, xmm0, 1\n"),
            "ceil" => mathop("    roundsd xmm0, xmm0, 2\n"),
            "sqrt" => mathop("    sqrtsd xmm0, xmm0\n"),
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
        self.static_strs.push((uid, s.to_owned()));
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

    fn generate_bool_expr(&mut self, expr: &Expr) -> Result<()> {
        match self.generate_expr(expr)? {
            Typ::Double => self.text.push_str("    call double_to_bool\n"),
            Typ::Bool => {}
            Typ::StaticStr => {
                self.text.push_str("    call static_str_to_bool\n");
            }
            Typ::OwnedString => {
                self.text.push_str("    call owned_string_to_bool\n");
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
                self.text.push_str("    call static_str_to_double\n");
            }
            Typ::OwnedString => {
                self.text.push_str("    call owned_string_to_double\n");
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
            Typ::Double => self.text.push_str("    call double_to_cow\n"),
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

    fn generate_custom_proc_call(
        &mut self,
        proc_name: &str,
        args: &[Expr],
        span: Span,
    ) -> Result<()> {
        let proc = self.custom_procs.get(proc_name).ok_or_else(|| {
            Box::new(Error::UnknownProc {
                span,
                proc_name: proc_name.to_owned(),
            })
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

        for arg in args {
            self.generate_any_expr(arg)?;
            self.text.push_str(
                "    push rsi
    push rdi
",
            );
        }

        writeln!(self.text, "    call {proc_id}").unwrap();
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
            r#"    xor eax, eax
    ret

{}
section .data
align 8"#,
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

struct Label<T>(T);

impl<T: fmt::Display> Emit for Label<T> {
    fn emit(self, program: &mut AsmProgram) {
        writeln!(program.text, "{}:", self.0).unwrap();
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
        writeln!(program.text, "{self}:").unwrap();
    }
}

struct CustomProcedure {
    id: Uid,
    pub params: Vec<(SmolStr, Uid)>,
}
