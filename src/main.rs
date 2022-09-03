#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(drain_filter)]

mod asset;
mod ast;
mod diagnostic;
mod ir;
mod lint;
mod macros;
mod optimize;
mod opts;
mod parser;
mod ser;
mod span;
mod uid;

use crate::{
    ir::Program, lint::lint_ast, macros::expand, opts::Opts,
    parser::input::Input, ser::write_sb3_file,
};
use codespan::Files;
use gumdrop::Options;
use std::{fs, io::Write, path::Path, sync::Mutex};

static FILES: Mutex<Files<String>> = Mutex::new(Files::new());

fn main() {
    let opts = Opts::parse_args_default_or_exit();
    let input = match fs::read_to_string(&opts.file) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("IO error: {err}");
            return;
        }
    };
    let main_file_id = FILES.lock().unwrap().add(&opts.file, input.clone());

    let asts = match parser::program(Input::new(&input, main_file_id)) {
        Ok((_, asts)) => asts,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };
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

    if let Err(err) = expand(asts, &opts).and_then(|expanded| {
        if let Some(file) = opts.dump_expanded {
            let mut file = fs::File::create(file).unwrap();
            writeln!(file, "{expanded:#?}").unwrap();
        }
        let mut program = Program::from_asts(expanded)?;
        if let Some(file) = opts.dump_ir {
            let mut file = fs::File::create(file).unwrap();
            writeln!(file, "{program:#?}").unwrap();
        }
        program.optimize();
        if let Some(file) = opts.dump_optimized {
            let mut file = fs::File::create(file).unwrap();
            writeln!(file, "{program:#?}").unwrap();
        }
        write_sb3_file(&program, Path::new("project.sb3"))
    }) {
        err.emit();
    }
}
