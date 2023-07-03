mod broadcast;
mod expr;
mod statement;
mod typ;

use crate::{
    diagnostic::{Error, Result},
    ir::{self, expr::Expr, proc::Procedure, sprite::Sprite},
};
use broadcast::Broadcasts;
use codemap::Span;
use cranelift::{
    codegen::{
        ir::{FuncRef, Function, Inst, UserFuncName},
        Context,
    },
    prelude::{
        isa::{CallConv, TargetFrontendConfig},
        types::*,
        *,
    },
};
use cranelift_module::{DataContext, DataId, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use sb3_stuff::Value as Immediate;
use std::{
    borrow::Cow, collections::HashMap, fs::File, io::Write, iter, path::Path,
};

pub fn write_object_file(program: &ir::Program, path: &Path) -> Result<()> {
    env_logger::init();

    let mut settings = settings::builder();
    settings.enable("enable_verifier").unwrap();
    settings.enable("is_pic").unwrap();
    settings.set("opt_level", "speed_and_size").unwrap();
    settings.enable("unwind_info").unwrap();
    let flags = settings::Flags::new(settings);
    let isa = isa::lookup_by_name("x86_64-unknown-linux-gnu")
        .unwrap()
        .finish(flags)
        .unwrap();
    let target_frontend_config = isa.frontend_config();

    let mut ctx = Context::new();
    let object_builder = ObjectBuilder::new(
        isa,
        Vec::new(),
        cranelift_module::default_libcall_names(),
    )
    .unwrap();
    let mut object_module = ObjectModule::new(object_builder);
    let mut func_ctx = FunctionBuilderContext::new();

    let global_vars = program
        .stage
        .variables
        .iter()
        .map(String::as_str)
        .zip(iter::repeat_with(|| {
            object_module.declare_anonymous_data(true, false).unwrap()
        }))
        .collect();

    let global_lists = program
        .stage
        .lists
        .iter()
        .map(String::as_str)
        .zip(iter::repeat_with(|| {
            object_module.declare_anonymous_data(true, false).unwrap()
        }))
        .collect();

    let mut p = Program {
        target_frontend_config,
        object_module,
        data_ctx: DataContext::new(),
        entry_points: Vec::new(),
        variable_counter: 0,
        extern_function_signatures: extern_function_signatures(),
        extern_functions: HashMap::new(),
        local_vars: HashMap::new(),
        local_lists: HashMap::new(),
        sprite_vars: HashMap::new(),
        sprite_lists: HashMap::new(),
        global_vars,
        global_lists,
        static_strs: HashMap::new(),
        custom_procs: HashMap::new(),
        proc_params: HashMap::new(),
        broadcasts: HashMap::new(),
        answer: None,
        main_broadcast_handler: None,
        uses_drand48: false,
        stop_block: None,
    };

    p.generate_sprite(&program.stage, "Stage", &mut ctx, &mut func_ctx)?;
    for (name, sprite) in &program.sprites {
        p.generate_sprite(sprite, name, &mut ctx, &mut func_ctx)?;
    }
    let main_signature = Signature {
        params: Vec::new(),
        returns: vec![AbiParam::new(I32)],
        call_conv: CallConv::SystemV,
    };
    ctx.clear();
    ctx.func = Function::with_name_signature(
        UserFuncName::default(),
        main_signature.clone(),
    );
    let mut fb = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
    let block = fb.create_block();
    fb.switch_to_block(block);
    fb.seal_block(block);

    if p.uses_drand48 {
        let tloc = fb.ins().iconst(I64, 0);
        let time = p.call_extern("time", &[tloc], &mut fb);
        let time = fb.inst_results(time)[0];
        p.call_extern("srand48", &[time], &mut fb);
    }

    for entry_point in &p.entry_points {
        let func_ref =
            p.object_module.declare_func_in_func(*entry_point, fb.func);
        fb.ins().call(func_ref, &[]);
    }
    let exit_code = fb.ins().iconst(I32, 0);
    fb.ins().return_(&[exit_code]);
    fb.finalize();
    let main_func_id = p
        .object_module
        .declare_function("main", Linkage::Export, &main_signature)
        .unwrap();
    p.object_module
        .define_function(main_func_id, &mut ctx)
        .unwrap();

    for &var_id in p.global_vars.values() {
        define_variable(var_id, &mut p.data_ctx, &mut p.object_module);
    }

    for &list_id in p.global_lists.values() {
        define_list(list_id, &mut p.data_ctx, &mut p.object_module);
    }

    if let Some(answer) = p.answer {
        p.data_ctx.clear();
        p.data_ctx.set_align(8);
        p.data_ctx.define(Box::new([
            7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ]));
        p.object_module.define_data(answer, &p.data_ctx).unwrap();
    }

    p.generate_broadcast_handlers(&mut ctx, &mut func_ctx);

    for (s, id) in &p.static_strs {
        p.data_ctx.clear();
        p.data_ctx.set_align(2);
        // The (arbitrary) null byte makes the string unaligned, which marks it
        // as static.
        p.data_ctx.define(format!("\0{s}").into_boxed_str().into());
        p.object_module.define_data(*id, &p.data_ctx).unwrap();
    }

    let object_bytes = p.object_module.finish().emit().unwrap();
    let mut file = File::create(path).unwrap();
    file.write_all(&object_bytes).unwrap();

    Ok(())
}

struct Program<'a> {
    target_frontend_config: TargetFrontendConfig,
    object_module: ObjectModule,
    data_ctx: DataContext,
    entry_points: Vec<FuncId>,
    variable_counter: u32,
    extern_function_signatures: HashMap<&'static str, Signature>,
    extern_functions: HashMap<&'static str, FuncId>,
    local_vars: HashMap<&'a str, DataId>,
    local_lists: HashMap<&'a str, DataId>,
    sprite_vars: HashMap<&'a str, DataId>,
    sprite_lists: HashMap<&'a str, DataId>,
    global_vars: HashMap<&'a str, DataId>,
    global_lists: HashMap<&'a str, DataId>,
    static_strs: HashMap<Cow<'a, str>, DataId>,
    custom_procs: HashMap<&'a str, CustomProc<'a>>,
    proc_params: HashMap<&'a str, (Value, Value)>,
    broadcasts: Broadcasts<'a>,
    main_broadcast_handler: Option<FuncId>,
    answer: Option<DataId>,
    uses_drand48: bool,
    stop_block: Option<Block>,
}

impl<'a> Program<'a> {
    fn generate_sprite(
        &mut self,
        sprite: &'a Sprite,
        name: &str,
        ctx: &mut Context,
        func_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        self.sprite_vars.clear();
        self.sprite_vars.extend(
            sprite
                .variables
                .iter()
                .map(String::as_str)
                .zip(iter::repeat_with(|| {
                    self.object_module
                        .declare_anonymous_data(true, false)
                        .unwrap()
                })),
        );

        self.sprite_lists.clear();
        self.sprite_lists
            .extend(sprite.lists.iter().map(String::as_str).zip(
                iter::repeat_with(|| {
                    self.object_module
                        .declare_anonymous_data(true, false)
                        .unwrap()
                }),
            ));

        // Prevent duplicate definitions of global variables/lists.
        if name != "Stage" {
            for &var_id in self.sprite_vars.values() {
                define_variable(
                    var_id,
                    &mut self.data_ctx,
                    &mut self.object_module,
                );
            }

            for &list_id in self.sprite_lists.values() {
                define_list(
                    list_id,
                    &mut self.data_ctx,
                    &mut self.object_module,
                );
            }
        }

        self.custom_procs = sprite
            .procedures
            .iter()
            .map(|(name, proc)| Ok(match &**name {
                "when-flag-clicked" | "when-cloned" | "when-received" => None,
                _ => {
                    let [proc] = &proc[..] else {
                        todo!("duplicate definition of custom procdeure `{name}`");
                    };

                    let param_names = proc.params.iter().map(|(param, span)| {
                        match param {
                            Expr::Sym(param_name, _) => Ok(&**param_name),
                            _ => Err(Box::new(
                                Error::InvalidParameterForCustomProcDef {
                                    span: *span
                                },
                            )),
                        }
                    }).collect::<Result<_>>()?;
                    let params = iter::repeat_with(|| AbiParam::new(I64))
                        .take(proc.params.len() * 2)
                        .collect();
                    let id = self
                        .object_module
                        .declare_anonymous_function(
                            &Signature {
                                params,
                                returns: Vec::new(),
                                call_conv: CallConv::SystemV
                            },
                        ).unwrap();
                    Some((&**name, CustomProc { id, param_names }))
                }
            }))
            .filter_map(Result::transpose)
            .collect::<Result<_>>()?;

        for (name, procs) in &sprite.procedures {
            for proc in procs {
                self.generate_proc(name, proc, ctx, func_ctx)?;
            }
        }

        Ok(())
    }

    fn generate_proc(
        &mut self,
        name: &str,
        proc: &'a Procedure,
        ctx: &mut Context,
        func_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        self.local_vars.clear();
        self.local_vars
            .extend(proc.variables.iter().map(String::as_str).zip(
                iter::repeat_with(|| {
                    self.object_module
                        .declare_anonymous_data(true, false)
                        .unwrap()
                }),
            ));

        self.local_lists.clear();
        self.local_lists
            .extend(proc.lists.iter().map(String::as_str).zip(
                iter::repeat_with(|| {
                    self.object_module
                        .declare_anonymous_data(true, false)
                        .unwrap()
                }),
            ));

        for &var_id in self.local_vars.values() {
            define_variable(
                var_id,
                &mut self.data_ctx,
                &mut self.object_module,
            );
        }

        for &list_id in self.local_lists.values() {
            define_list(list_id, &mut self.data_ctx, &mut self.object_module);
        }

        ctx.clear();
        self.proc_params.clear();
        self.stop_block = None;

        match name {
            "when-flag-clicked" => {
                assert!(proc.params.is_empty());
                let signature = Signature::new(CallConv::SystemV);
                let func_id = self
                    .object_module
                    .declare_anonymous_function(&signature)
                    .unwrap();
                self.entry_points.push(func_id);
                ctx.func = Function::with_name_signature(
                    UserFuncName::default(),
                    signature,
                );
                let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);
                let entry = fb.create_block();
                fb.switch_to_block(entry);
                fb.seal_block(entry);
                if self.generate_statement(&proc.body, &mut fb)?.is_continue() {
                    fb.ins().return_(&[]);
                }
                fb.finalize();
                self.object_module.define_function(func_id, ctx).unwrap();
            }
            "when-received" => {
                let [(Expr::Imm(Immediate::String(broadcast_name)), _)] =
                    &proc.params[..]
                else {
                    todo!();
                };
                let signature = Signature::new(CallConv::SystemV);
                let func_id = self
                    .object_module
                    .declare_anonymous_function(&signature)
                    .unwrap();
                self.broadcasts
                    .entry(broadcast_name)
                    .or_insert_with(|| {
                        (
                            self.object_module
                                .declare_anonymous_function(&Signature::new(
                                    CallConv::SystemV,
                                ))
                                .unwrap(),
                            Vec::new(),
                        )
                    })
                    .1
                    .push(func_id);
                ctx.func = Function::with_name_signature(
                    UserFuncName::default(),
                    signature,
                );
                let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);
                let entry = fb.create_block();
                fb.switch_to_block(entry);
                fb.seal_block(entry);
                if self.generate_statement(&proc.body, &mut fb)?.is_continue() {
                    fb.ins().return_(&[]);
                }
                fb.finalize();
                self.object_module.define_function(func_id, ctx).unwrap();
            }
            _ => {
                let func_id = self.custom_procs[name].id;
                let signature = self
                    .object_module
                    .declarations()
                    .get_function_decl(func_id)
                    .signature
                    .clone();
                ctx.func = Function::with_name_signature(
                    UserFuncName::default(),
                    signature,
                );
                let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);
                let entry = fb.create_block();
                fb.switch_to_block(entry);
                fb.seal_block(entry);
                fb.append_block_params_for_function_params(entry);
                self.proc_params.extend(
                    proc.params
                        .iter()
                        .map(|(param, _)| match param {
                            Expr::Sym(param, _) => &**param,
                            _ => unreachable!(),
                        })
                        .zip(
                            fb.block_params(entry)
                                .chunks_exact(2)
                                .map(|chunk| (chunk[0], chunk[1])),
                        ),
                );
                if !self.proc_params.is_empty() {
                    self.stop_block = Some(fb.create_block());
                }
                if self.generate_statement(&proc.body, &mut fb)?.is_continue() {
                    if let Some(stop_block) = self.stop_block {
                        fb.ins().jump(stop_block, &[]);
                        fb.switch_to_block(stop_block);
                        fb.seal_block(stop_block);
                        let params = fb
                            .block_params(entry)
                            .iter()
                            .copied()
                            .step_by(2)
                            .collect::<Vec<_>>();
                        for param in params {
                            self.call_extern("drop_any", &[param], &mut fb);
                        }
                    }
                    fb.ins().return_(&[]);
                }
                fb.finalize();
                self.object_module.define_function(func_id, ctx).unwrap();
            }
        }

        Ok(())
    }

    fn new_variable(&mut self) -> Variable {
        self.variable_counter += 1;
        Variable::from_u32(self.variable_counter - 1)
    }

    fn allocate_static_str(
        &mut self,
        s: Cow<'a, str>,
        fb: &mut FunctionBuilder,
    ) -> (Value, Value) {
        let len = s.len();
        let data_id = *self.static_strs.entry(s).or_insert_with(|| {
            self.object_module
                .declare_anonymous_data(false, false)
                .unwrap()
        });
        let global_value =
            self.object_module.declare_data_in_func(data_id, fb.func);
        // Offset by 1 byte since static strs start on odd addresses.
        let global_value = fb.create_global_value(GlobalValueData::IAddImm {
            base: global_value,
            offset: 1.into(),
            global_type: I64,
        });
        (
            fb.ins().global_value(I64, global_value),
            fb.ins().iconst(I64, len as i64),
        )
    }

    fn call_extern(
        &mut self,
        func_name: &'static str,
        args: &[Value],
        fb: &mut FunctionBuilder,
    ) -> Inst {
        let func_id =
            *self.extern_functions.entry(func_name).or_insert_with(|| {
                let Some(signature) =
                    self.extern_function_signatures.get(func_name)
                else {
                    panic!("extern function `{func_name}` missing signature");
                };
                self.object_module
                    .declare_function(func_name, Linkage::Import, signature)
                    .unwrap()
            });
        let func_ref =
            self.object_module.declare_func_in_func(func_id, fb.func);
        fb.ins().call(func_ref, args)
    }

    fn lookup_var(
        &self,
        name: &str,
        fb: &mut FunctionBuilder,
    ) -> Option<Value> {
        let data_id = *self
            .local_vars
            .get(name)
            .or_else(|| self.sprite_vars.get(name))
            .or_else(|| self.global_vars.get(name))?;
        let global_value =
            self.object_module.declare_data_in_func(data_id, fb.func);
        Some(fb.ins().global_value(I64, global_value))
    }

    fn lookup_list(
        &self,
        name: &str,
        span: Span,
        fb: &mut FunctionBuilder,
    ) -> Result<Value> {
        let data_id = *self
            .local_lists
            .get(name)
            .or_else(|| self.sprite_lists.get(name))
            .or_else(|| self.global_lists.get(name))
            .ok_or_else(|| Error::UnknownList {
                span,
                list_name: name.into(),
            })?;
        let global_value =
            self.object_module.declare_data_in_func(data_id, fb.func);
        Ok(fb.ins().global_value(I64, global_value))
    }

    fn answer(&mut self, fb: &mut FunctionBuilder) -> Value {
        let data_id = *self.answer.get_or_insert_with(|| {
            self.object_module
                .declare_anonymous_data(true, false)
                .unwrap()
        });
        let global_value =
            self.object_module.declare_data_in_func(data_id, fb.func);
        fb.ins().global_value(I64, global_value)
    }

    fn main_broadcast_handler(&mut self, fb: &mut FunctionBuilder) -> FuncRef {
        let func_id = *self.main_broadcast_handler.get_or_insert_with(|| {
            self.object_module
                .declare_anonymous_function(&Signature {
                    params: vec![AbiParam::new(I64), AbiParam::new(I64)],
                    returns: Vec::new(),
                    call_conv: CallConv::SystemV,
                })
                .unwrap()
        });
        self.object_module.declare_func_in_func(func_id, fb.func)
    }
}

fn define_variable(
    id: DataId,
    data_ctx: &mut DataContext,
    object_module: &mut ObjectModule,
) {
    data_ctx.clear();
    data_ctx.set_align(8);
    data_ctx.define(Box::new([2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));
    object_module.define_data(id, data_ctx).unwrap();
}

fn define_list(
    id: DataId,
    data_ctx: &mut DataContext,
    object_module: &mut ObjectModule,
) {
    data_ctx.clear();
    data_ctx.set_align(8);
    data_ctx.define_zeroinit(24);
    object_module.define_data(id, data_ctx).unwrap();
}

fn extern_function_signatures() -> HashMap<&'static str, Signature> {
    macro_rules! sig {
        ($name:literal: $($params:ident),* -> $($returns:ident),*) => {
            ($name, Signature {
                params: vec![$(AbiParam::new($params)),*],
                returns: vec![$(AbiParam::new($returns)),*],
                call_conv: CallConv::SystemV,
            })
        };
    }

    HashMap::from([
        sig! { "any_eq_any": I64, I64, I64, I64 -> I8 },
        sig! { "any_eq_bool": I64, I64, I8 -> I8 },
        sig! { "any_eq_double": I64, I64, F64 -> I8 },
        sig! { "any_eq_str": I64, I64, I64, I64 -> I8 },
        sig! { "any_lt_any": I64, I64, I64, I64 -> I8 },
        sig! { "any_lt_bool": I64, I64, I8 -> I8 },
        sig! { "any_lt_double": I64, I64, F64 -> I8 },
        sig! { "any_lt_str": I64, I64, I64, I64 -> I8 },
        sig! { "any_to_bool": I64, I64 -> I8 },
        sig! { "any_to_cow": I64, I64 -> I64, I64 },
        sig! { "any_to_double": I64, I64 -> F64 },
        sig! { "ask": I64, I64 -> I64, I64 },
        sig! { "bool_lt_any": I8, I64, I64 -> I8 },
        sig! { "bool_to_str": I8 -> I64, I64 },
        sig! { "char_at": I64, I64, I64 -> I64, I64 },
        sig! { "clone_any": I64, I64 -> I64, I64 },
        sig! { "clone_cow": I64, I64 -> I64, I64 },
        sig! { "double_lt_any": I64, I64, F64 -> I8 },
        sig! { "double_to_cow": F64 -> I64, I64 },
        sig! { "drop_any": I64 -> },
        sig! { "drop_cow": I64 -> },
        sig! { "exit": I32 -> },
        sig! { "fmod": F64, F64 -> F64 },
        sig! { "free": I64 -> },
        sig! { "list_append": I64, I64, I64 -> },
        sig! { "list_delete": I64, I64, I64 -> },
        sig! { "list_delete_all": I64 -> },
        sig! { "list_get": I64, I64, I64 -> I64, I64 },
        sig! { "list_replace": I64, I64, I64, I64, I64 -> },
        sig! { "malloc": I64 -> I64 },
        sig! { "random_between": F64, F64 -> F64 },
        sig! { "srand48": I64 -> },
        sig! { "str_eq_str": I64, I64, I64, I64 -> I8 },
        sig! { "str_length": I64, I64 -> I64 },
        sig! { "str_lt_any": I64, I64, I64, I64 -> I8 },
        sig! { "str_lt_str": I64, I64, I64, I64 -> I8 },
        sig! { "time": I64 -> I64 },
        sig! { "wait_seconds": F64 -> },
        sig! { "write": I32, I64, I64 -> I64 },
        sig! { "log": F64 -> F64 },
        sig! { "log10": F64 -> F64 },
        sig! { "exp": F64 -> F64 },
        sig! { "exp10": F64 -> F64 },
        sig! { "sin": F64 -> F64 },
        sig! { "cos": F64 -> F64 },
        sig! { "tan": F64 -> F64 },
        sig! { "asin": F64 -> F64 },
        sig! { "acos": F64 -> F64 },
        sig! { "atan": F64 -> F64 },
    ])
}

struct CustomProc<'a> {
    id: FuncId,
    param_names: Vec<&'a str>,
}
