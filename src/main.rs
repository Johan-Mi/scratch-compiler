#![forbid(unsafe_code)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(extract_if)]
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
mod uid;

use crate::{
    codegen::write_program, ir::Program, lint::lint_ast, macros::expand,
    opts::Opts, parser::Input,
};
use codemap::CodeMap;
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

    let mut code_map = CodeMap::new();
    let main_file =
        code_map.add_file(opts.file.display().to_string(), input.clone());

    if let Err(err) = parser::program(Input {
        input: Located::new(&input),
        state: &main_file,
    })
    .and_then(|asts| {
        if opts.lint {
            for ast in &asts {
                lint_ast(ast, &code_map);
            }
        }
        let expanded = expand(asts, &opts, &mut code_map)?;
        let mut program = Program::from_asts(expanded)?;
        program.optimize();
        write_program(&program, &opts)
    }) {
        err.emit(&code_map);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
