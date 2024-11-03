use super::Program;
use crate::{
    diagnostic::{Error, Result},
    ir::{expr::Expr, statement::Statement},
};
use codemap::Span;
use cranelift::prelude::{types::*, *};
use cranelift_module::Module;
use sb3_stuff::Value as Immediate;
use std::ops::ControlFlow;

const CONTINUE: ControlFlow<()> = ControlFlow::Continue(());
const BREAK: ControlFlow<()> = ControlFlow::Break(());

impl<'a> Program<'a> {
    pub(super) fn generate_statement(
        &mut self,
        stmt: &'a Statement,
        fb: &mut FunctionBuilder,
    ) -> Result<ControlFlow<()>> {
        match stmt {
            Statement::ProcCall {
                proc_name,
                args,
                proc_span,
            } => self.generate_proc_call(proc_name, args, *proc_span, fb),
            Statement::Do(stmts) => {
                match stmts
                    .iter()
                    .try_for_each(|stmt| match self.generate_statement(stmt, fb) {
                        Ok(CONTINUE) => ControlFlow::Continue(()),
                        Ok(BREAK) => ControlFlow::Break(Ok(())),
                        Err(err) => ControlFlow::Break(Err(err)),
                    }) {
                    ControlFlow::Continue(()) => Ok(CONTINUE),
                    ControlFlow::Break(Ok(())) => Ok(BREAK),
                    ControlFlow::Break(Err(err)) => Err(err),
                }
            }
            Statement::IfElse {
                condition,
                then,
                else_,
                ..
            } => {
                let then_block = fb.create_block();
                let else_block = fb.create_block();
                let after = fb.create_block();
                let condition = self.generate_bool_expr(condition, fb)?;
                fb.ins().brif(condition, then_block, &[], else_block, &[]);
                fb.seal_block(else_block);
                fb.seal_block(then_block);
                fb.switch_to_block(then_block);
                if self.generate_statement(then, fb)?.is_continue() {
                    fb.ins().jump(after, &[]);
                }
                fb.switch_to_block(else_block);
                if self.generate_statement(else_, fb)?.is_continue() {
                    fb.ins().jump(after, &[]);
                }
                fb.switch_to_block(after);
                fb.seal_block(after);
                Ok(CONTINUE)
            }
            Statement::Repeat { times, body } => {
                let counter = self.new_variable();
                fb.declare_var(counter, I64);
                let times = self.generate_double_expr(times, fb)?;
                let times = fb.ins().fcvt_to_uint_sat(I64, times);
                fb.def_var(counter, times);
                let loop_start = fb.create_block();
                let loop_body = fb.create_block();
                let after = fb.create_block();
                fb.ins().jump(loop_start, &[]);
                fb.switch_to_block(loop_start);
                let remaining_times = fb.use_var(counter);
                fb.ins().brif(remaining_times, loop_body, &[], after, &[]);
                fb.seal_block(after);
                fb.seal_block(loop_body);
                fb.switch_to_block(loop_body);
                if self.generate_statement(body, fb)?.is_continue() {
                    let next_count = fb.ins().iadd_imm(remaining_times, -1);
                    fb.def_var(counter, next_count);
                    fb.ins().jump(loop_start, &[]);
                }
                fb.seal_block(loop_start);
                fb.switch_to_block(after);
                Ok(CONTINUE)
            }
            Statement::Forever(body) => {
                let loop_start = fb.create_block();
                fb.ins().jump(loop_start, &[]);
                fb.switch_to_block(loop_start);
                if self.generate_statement(body, fb)?.is_continue() {
                    fb.ins().jump(loop_start, &[]);
                }
                fb.seal_block(loop_start);
                Ok(BREAK)
            }
            Statement::Until { condition, body } | Statement::While { condition, body } => {
                let loop_start = fb.create_block();
                let loop_body = fb.create_block();
                let after = fb.create_block();
                fb.ins().jump(loop_start, &[]);
                fb.switch_to_block(loop_start);
                let condition = self.generate_bool_expr(condition, fb)?;
                if matches!(stmt, Statement::While { .. }) {
                    fb.ins().brif(condition, loop_body, &[], after, &[]);
                } else {
                    fb.ins().brif(condition, after, &[], loop_body, &[]);
                }
                fb.seal_block(after);
                fb.seal_block(loop_body);
                fb.switch_to_block(loop_body);
                if self.generate_statement(body, fb)?.is_continue() {
                    fb.ins().jump(loop_start, &[]);
                }
                fb.seal_block(loop_start);
                fb.switch_to_block(after);
                Ok(CONTINUE)
            }
            Statement::For {
                counter,
                times,
                body,
            } => {
                let var = self
                    .lookup_var(&counter.0, fb)
                    .ok_or_else(|| Error::UnknownVar {
                        span: counter.1,
                        var_name: (&*counter.0).into(),
                    })?;

                let loop_start = fb.create_block();
                let loop_body = fb.create_block();
                let after = fb.create_block();
                let times = self.generate_double_expr(times, fb)?;
                let times = fb.ins().fcvt_to_uint_sat(I64, times);
                let counter = self.new_variable();
                fb.declare_var(counter, I64);
                let zero = fb.ins().iconst(I64, 0);
                fb.def_var(counter, zero);
                fb.ins().jump(loop_start, &[]);
                fb.switch_to_block(loop_start);
                let old_count = fb.use_var(counter);
                let should_break = fb.ins().icmp(IntCC::Equal, old_count, times);
                fb.ins().brif(should_break, after, &[], loop_body, &[]);
                fb.seal_block(loop_body);
                fb.switch_to_block(loop_body);
                let new_count = fb.ins().iadd_imm(old_count, 1);
                fb.def_var(counter, new_count);

                let new_count_as_f64 = fb.ins().fcvt_from_uint(F64, new_count);
                let mem_flags = MemFlags::trusted();
                let old_var_value = fb.ins().load(I64, mem_flags, var, 0);
                self.call_extern("drop_any", &[old_var_value], fb);
                let number_type_tag = fb.ins().iconst(I64, 2);
                fb.ins().store(mem_flags, number_type_tag, var, 0);
                fb.ins().store(mem_flags, new_count_as_f64, var, 8);

                if self.generate_statement(body, fb)?.is_continue() {
                    fb.ins().jump(loop_start, &[]);
                }
                fb.seal_block(loop_start);
                fb.switch_to_block(after);
                fb.seal_block(after);
                Ok(CONTINUE)
            }
        }
    }

    fn generate_proc_call(
        &mut self,
        proc_name: &str,
        args: &'a [Expr],
        span: Span,
        fb: &mut FunctionBuilder,
    ) -> Result<ControlFlow<()>> {
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
                    let (ptr, len) = self.generate_cow_expr(message, fb)?;
                    let fd = fb.ins().iconst(I32, 1); // STDOUT_FILENO
                    self.call_extern("write", &[fd, ptr, len], fb);
                    self.call_extern("drop_cow", &[ptr], fb);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(1),
            },
            ":=" => match args {
                [Expr::Sym(var_name, var_span), value] => {
                    let var = self
                        .lookup_var(var_name, fb)
                        .ok_or_else(|| Error::UnknownVar {
                            span: *var_span,
                            var_name: var_name.clone(),
                        })?;
                    let new = self.generate_any_expr(value, fb)?;
                    let mem_flags = MemFlags::trusted();
                    let old = fb.ins().load(I64, mem_flags, var, 0);
                    self.call_extern("drop_any", &[old], fb);
                    fb.ins().store(mem_flags, new.0, var, 0);
                    fb.ins().store(mem_flags, new.1, var, 8);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(2),
            },
            "+=" => match args {
                [Expr::Sym(var_name, var_span), amount] => {
                    let var = self
                        .lookup_var(var_name, fb)
                        .ok_or_else(|| Error::UnknownVar {
                            span: *var_span,
                            var_name: var_name.clone(),
                        })?;
                    let amount = self.generate_double_expr(amount, fb)?;
                    let mem_flags = MemFlags::trusted();
                    let old_low = fb.ins().load(I64, mem_flags, var, 0);
                    let old_high = fb.ins().load(I64, mem_flags, var, 8);
                    let old = self.call_extern("any_to_double", &[old_low, old_high], fb);
                    let old = fb.inst_results(old)[0];
                    let new = fb.ins().fadd(old, amount);
                    let number_type_tag = fb.ins().iconst(I64, 2);
                    fb.ins().store(mem_flags, number_type_tag, var, 0);
                    fb.ins().store(mem_flags, new, var, 8);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(2),
            },
            "append" => match args {
                [Expr::Sym(list_name, list_span), value] => {
                    let list = self.lookup_list(list_name, *list_span, fb)?;
                    let value = self.generate_any_expr(value, fb)?;
                    self.call_extern("list_append", &[list, value.0, value.1], fb);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(2),
            },
            "delete" => match args {
                [Expr::Sym(list_name, list_span), value] => {
                    let list = self.lookup_list(list_name, *list_span, fb)?;
                    let value = self.generate_any_expr(value, fb)?;
                    self.call_extern("list_delete", &[value.0, value.1, list], fb);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(2),
            },
            "delete-all" => match args {
                [Expr::Sym(list_name, list_span)] => {
                    let list = self.lookup_list(list_name, *list_span, fb)?;
                    self.call_extern("list_delete_all", &[list], fb);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(1),
            },
            "replace" => match args {
                [Expr::Sym(list_name, list_span), index, value] => {
                    let list = self.lookup_list(list_name, *list_span, fb)?;
                    let index = self.generate_any_expr(index, fb)?;
                    let value = self.generate_any_expr(value, fb)?;
                    self.call_extern(
                        "list_replace",
                        &[index.0, index.1, value.0, value.1, list],
                        fb,
                    );
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(3),
            },
            "stop-this-script" => match args {
                [] => {
                    if let Some(stop_block) = self.stop_block {
                        fb.ins().jump(stop_block, &[]);
                    } else {
                        fb.ins().return_(&[]);
                    }
                    Ok(BREAK)
                }
                _ => wrong_arg_count(0),
            },
            "stop-all" => match args {
                [] => {
                    let exit_code = fb.ins().iconst(I32, 0);
                    self.call_extern("exit", &[exit_code], fb);
                    fb.ins().trap(TrapCode::UnreachableCodeReached);
                    Ok(BREAK)
                }
                _ => wrong_arg_count(0),
            },
            "ask" => match args {
                [question] => {
                    let question = self.generate_cow_expr(question, fb)?;
                    let new = self.call_extern("ask", &<[_; 2]>::from(question), fb);
                    let new_ptr = fb.inst_results(new)[0];
                    let new_len = fb.inst_results(new)[1];
                    self.call_extern("drop_cow", &[question.0], fb);
                    let answer = self.answer(fb);
                    let mem_flags = MemFlags::trusted();
                    let old = fb.ins().load(I64, mem_flags, answer, 0);
                    self.call_extern("drop_cow", &[old], fb);
                    fb.ins().store(mem_flags, new_ptr, answer, 0);
                    fb.ins().store(mem_flags, new_len, answer, 8);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(1),
            },
            "send-broadcast-sync" => match args {
                [Expr::Imm(Immediate::String(name))] => {
                    if let Some((handler, _)) = self.broadcasts.get(&*name.to_lowercase()) {
                        let handler = self.object_module.declare_func_in_func(*handler, fb.func);
                        fb.ins().call(handler, &[]);
                    }
                    Ok(CONTINUE)
                }
                [name] => {
                    let name = self.generate_cow_expr(name, fb)?;
                    let main_broadcast_handler = self.main_broadcast_handler(fb);
                    fb.ins().call(main_broadcast_handler, &<[_; 2]>::from(name));
                    self.call_extern("drop_cow", &[name.0], fb);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(1),
            },
            "wait" => match args {
                [duration] => {
                    let duration = self.generate_double_expr(duration, fb)?;
                    self.call_extern("wait_seconds", &[duration], fb);
                    Ok(CONTINUE)
                }
                _ => wrong_arg_count(1),
            },
            _ => {
                self.generate_custom_proc_call(proc_name, args, span, fb)?;
                Ok(CONTINUE)
            }
        }
    }

    fn generate_custom_proc_call(
        &mut self,
        proc_name: &str,
        args: &'a [Expr],
        span: Span,
        fb: &mut FunctionBuilder,
    ) -> Result<()> {
        let proc = self
            .custom_procs
            .get(proc_name)
            .ok_or_else(|| Error::UnknownProc {
                span,
                proc_name: proc_name.to_owned(),
            })?;
        let func_ref = self.object_module.declare_func_in_func(proc.id, fb.func);

        let param_count = proc.param_names.len();
        if args.len() != param_count {
            return Err(Box::new(Error::CustomProcWrongArgCount {
                span,
                proc_name: proc_name.to_owned(),
                expected: param_count,
                got: args.len(),
            }));
        }

        let args = args
            .iter()
            .map(|arg| self.generate_any_expr(arg, fb))
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .flat_map(<[_; 2]>::from)
            .collect::<Vec<_>>();
        fb.ins().call(func_ref, &args);

        Ok(())
    }
}
