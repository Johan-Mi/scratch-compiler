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
use std::{fs, process::ExitCode};
use winnow::stream::Located;

fn main() -> ExitCode {
    let opts = Opts::parse_args_default_or_exit();
    let input = match fs::read_to_string(&opts.file) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("IO error: {err}");
            return ExitCode::FAILURE;
        }
    };

    let mut files = Files::new();
    let main_file_id = files.add(&opts.file, input.clone());

    if let Err(err) = parser::program(Input {
        input: Located::new(&input),
        state: main_file_id,
    })
    .and_then(|asts| {
        if opts.lint {
            for ast in &asts {
                lint_ast(ast, &files);
            }
        }
        let expanded = expand(asts, &opts, &mut files)?;
        let mut program = Program::from_asts(expanded)?;
        program.optimize();
        write_program(&program, &opts)
    }) {
        err.emit(&files);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
