use super::Program;
use cranelift::{
    codegen::{
        ir::{Function, UserFuncName},
        Context,
    },
    prelude::{isa::CallConv, types::*, *},
};
use cranelift_module::{FuncId, Module};
use std::{borrow::Cow, collections::HashMap};

pub(super) type Broadcasts<'a> = HashMap<&'a str, (FuncId, Vec<FuncId>)>;

impl Program<'_> {
    pub(super) fn generate_broadcast_handlers(
        &mut self,
        ctx: &mut Context,
        func_ctx: &mut FunctionBuilderContext,
    ) {
        let Some(main_broadcast_handler) = self.main_broadcast_handler else {
            return;
        };

        for (handler_id, receievers) in self.broadcasts.values() {
            ctx.clear();
            ctx.func = Function::with_name_signature(
                UserFuncName::default(),
                Signature::new(CallConv::SystemV),
            );
            let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);

            let block = fb.create_block();
            fb.switch_to_block(block);
            fb.seal_block(block);
            for receiver in receievers {
                let receiver = self.object_module.declare_func_in_func(*receiver, fb.func);
                fb.ins().call(receiver, &[]);
            }
            fb.ins().return_(&[]);

            fb.finalize();
            self.object_module
                .define_function(*handler_id, ctx)
                .unwrap();
        }

        ctx.clear();
        ctx.func = Function::with_name_signature(
            UserFuncName::default(),
            Signature {
                params: vec![AbiParam::new(I64), AbiParam::new(I64)],
                returns: Vec::new(),
                call_conv: CallConv::SystemV,
            },
        );
        let mut fb = FunctionBuilder::new(&mut ctx.func, func_ctx);

        let entry = fb.create_block();
        fb.switch_to_block(entry);
        fb.seal_block(entry);
        fb.append_block_params_for_function_params(entry);
        let ptr = fb.block_params(entry)[0];
        let len = fb.block_params(entry)[1];
        for (name, (handler_id, _)) in &self.broadcasts.clone() {
            let yes_matched = fb.create_block();
            let next = fb.create_block();
            let name = self.allocate_static_str(Cow::Borrowed(name), &mut fb);
            let did_match = self.call_extern("str_eq_str", &[name.0, name.1, ptr, len], &mut fb);
            let did_match = fb.inst_results(did_match)[0];
            fb.ins().brif(did_match, yes_matched, &[], next, &[]);
            fb.switch_to_block(yes_matched);
            fb.seal_block(yes_matched);
            let handler = self
                .object_module
                .declare_func_in_func(*handler_id, fb.func);
            fb.ins().call(handler, &[]);
            fb.ins().return_(&[]);
            fb.switch_to_block(next);
            fb.seal_block(next);
        }
        fb.ins().return_(&[]);

        fb.finalize();
        self.object_module
            .define_function(main_broadcast_handler, ctx)
            .unwrap();
    }
}
