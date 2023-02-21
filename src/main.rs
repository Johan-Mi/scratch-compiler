#![forbid(unsafe_code)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(drain_filter)]
#![feature(let_chains)]

mod asset;
mod ast;
mod codegen;
mod diagnostic;
mod ir;
mod lint;
mod macros;
mod optimize;
mod opts;
mod parser;
mod span;
mod uid;

use crate::{
    codegen::write_program, ir::Program, lint::lint_ast, macros::expand,
    opts::Opts, parser::Input,
};
use codespan::Files;
use gumdrop::Options;
use nom8::input::Located;
use std::{fs, io::Write, process::ExitCode, sync::Mutex};

static FILES: Mutex<Files<String>> = Mutex::new(Files::new());

fn main() -> ExitCode {
    let opts = Opts::parse_args_default_or_exit();
    let input = match fs::read_to_string(&opts.file) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("IO error: {err}");
            return ExitCode::FAILURE;
        }
    };
    let main_file_id = FILES.lock().unwrap().add(&opts.file, input.clone());

    if let Err(err) = parser::program(Input {
        input: Located::new(&input),
        state: main_file_id,
    })
    .and_then(|asts| {
        if opts.lint {
            for ast in &asts {
                lint_ast(ast);
            }
        }
        if let Some(file) = opts.dump_ast.as_deref() {
            let mut file = fs::File::create(file).unwrap();
            for ast in &asts {
                writeln!(file, "{ast:#?}").unwrap();
            }
        }
        let expanded = expand(asts, &opts)?;

        if let Some(file) = &opts.dump_expanded {
            let mut file = fs::File::create(file).unwrap();
            writeln!(file, "{expanded:#?}").unwrap();
        }
        let mut program = Program::from_asts(expanded)?;
        if let Some(file) = &opts.dump_ir {
            let mut file = fs::File::create(file).unwrap();
            writeln!(file, "{program:#?}").unwrap();
        }
        program.optimize();
        if let Some(file) = &opts.dump_optimized {
            let mut file = fs::File::create(file).unwrap();
            writeln!(file, "{program:#?}").unwrap();
        }
        write_program(&program, &opts)
    }) {
        err.emit();
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
