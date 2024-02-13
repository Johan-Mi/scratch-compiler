#![forbid(unsafe_code)]
#![feature(box_patterns)]
#![feature(string_deref_patterns)]
#![feature(extract_if)]
#![feature(let_chains)]

mod asset;
mod ast;
mod codegen;
mod diagnostic;
mod formatter;
mod ir;
mod macros;
mod optimize;
mod opts;
mod parser;
mod uid;

use crate::{
    codegen::write_program,
    ir::Program,
    macros::expand,
    opts::{Command, CompileOpts, Opts},
    parser::Input,
};
use codemap::CodeMap;
use gumdrop::Options;
use std::{fs, process::ExitCode};
use winnow::stream::Located;

fn main() -> ExitCode {
    let opts = Opts::parse_args_default_or_exit();
    let mut code_map = CodeMap::new();

    let res = match opts.command {
        Some(Command::Compile(opts)) => real_main(opts, &mut code_map),
        Some(Command::Format(_)) => formatter::format_stdin_to_stdout()
            .map_err(|err| {
                Box::new(diagnostic::Error::FailedToReadSourceCode {
                    inner: err,
                })
            }),
        None => {
            eprintln!("error: no command provided");
            return ExitCode::FAILURE;
        }
    };
    match res {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            err.emit(&code_map);
            ExitCode::FAILURE
        }
    }
}

fn real_main(
    opts: CompileOpts,
    code_map: &mut CodeMap,
) -> diagnostic::Result<()> {
    let input = fs::read_to_string(&opts.file).map_err(|err| {
        diagnostic::Error::FailedToReadSourceCode { inner: err }
    })?;

    let main_file =
        code_map.add_file(opts.file.display().to_string(), input.clone());

    let asts = parser::program(Input {
        input: Located::new(&input),
        state: &main_file,
    })?;
    let expanded = expand(asts, &opts, code_map)?;
    let mut program = Program::from_asts(expanded)?;
    program.optimize();
    write_program(&program, &opts)
}
