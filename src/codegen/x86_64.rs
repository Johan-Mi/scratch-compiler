mod expr;
mod statement;
mod typ;

use crate::{
    diagnostic::{Error, Result},
    ir::{expr::Expr, proc::Procedure, sprite::Sprite, Program},
    span::Span,
    uid::Uid,
};
use sb3_stuff::Value;
use std::{
    borrow::Cow,
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
    broadcasts: HashMap<String, Vec<Uid>>,
    text: String,
    local_vars: HashMap<&'a str, Uid>,
    local_lists: HashMap<&'a str, Uid>,
    sprite_vars: HashMap<&'a str, Uid>,
    sprite_lists: HashMap<&'a str, Uid>,
    global_vars: HashMap<&'a str, Uid>,
    global_lists: HashMap<&'a str, Uid>,
    var_ids: Vec<Uid>,
    list_ids: Vec<Uid>,
    static_strs: HashMap<Cow<'a, str>, Uid>,
    custom_procs: HashMap<&'a str, CustomProcedure<'a>>,
    proc_stop_label: Option<LocalLabel<Uid>>,
    proc_params: Vec<&'a str>,
    stack_aligned: bool,
    uses_drand48: bool,
    uses_ask: bool,
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
        t.emit_to(self);
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

    fn generate_proc(&mut self, name: &str, proc: &'a Procedure) -> Result<()> {
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
                Ok(())
            }
            "when-cloned" => todo!(),
            "when-received" => {
                let [(Expr::Lit(Value::String(broadcast_name)), _)] =
                    &proc.params[..]
                else {
                    todo!();
                };
                self.proc_stop_label = None;
                let proc_id = self.new_uid();
                self.broadcasts
                    .entry(broadcast_name.to_lowercase())
                    .or_default()
                    .push(proc_id);
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
                Ok(())
            }
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
    call drop_any",
                            i * 16 + 16,
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

                Ok(())
            }
        }
    }

    fn allocate_static_str(&mut self, s: Cow<'a, str>) -> Uid {
        *self
            .static_strs
            .entry(s)
            .or_insert_with(|| self.uid_generator.new_uid())
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
        if self.uses_drand48 {
            f.write_str(
                "    xor edi, edi
    call time wrt ..plt
    mov rdi, rax
    call srand48 wrt ..plt
",
            )?;
        }
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
        if self.uses_ask {
            // 7 is a bogus static str pointer; the exact address doesn't
            // matter since the length is 0, meaning that nothing will ever be
            // read from it. All that matters is that it's odd and greater
            // than 1, which gives it the correct type.
            f.write_str("answer: dq 7, 0\n")?;
        }
        for var_id in &self.var_ids {
            writeln!(f, "{var_id}: dq 2, 0")?;
        }
        for (s, str_id) in &self.static_strs {
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
    fn emit_to(self, program: &mut AsmProgram);
}

impl Emit for &str {
    fn emit_to(self, program: &mut AsmProgram) {
        program.text.push_str(self);
        program.text.push('\n');
    }
}

struct Label<T>(T);

impl<T: fmt::Display> Emit for Label<T> {
    fn emit_to(self, program: &mut AsmProgram) {
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
    fn emit_to(self, program: &mut AsmProgram) {
        writeln!(program, "{self}:").unwrap();
    }
}

struct CustomProcedure<'a> {
    id: Uid,
    pub params: Vec<&'a str>,
}
