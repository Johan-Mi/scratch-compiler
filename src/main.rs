#![forbid(unsafe_code)]
#![feature(box_patterns)]
#![feature(string_deref_patterns)]
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
    let mut code_map = CodeMap::new();

    match real_main(opts, &mut code_map) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            err.emit(&code_map);
            ExitCode::FAILURE
        }
    }
}

fn real_main(opts: Opts, code_map: &mut CodeMap) -> diagnostic::Result<()> {
    let input = fs::read_to_string(&opts.file).map_err(|err| {
        diagnostic::Error::FailedToReadSourceCode { inner: err }
    })?;

    let main_file =
        code_map.add_file(opts.file.display().to_string(), input.clone());

    let asts = parser::program(Input {
        input: Located::new(&input),
        state: &main_file,
    })?;
    if opts.lint {
        for ast in &asts {
            lint_ast(ast, code_map);
        }
    }
    let expanded = expand(asts, &opts, code_map)?;
    let mut program = Program::from_asts(expanded)?;
    program.optimize();
    write_program(&program, &opts)
}
