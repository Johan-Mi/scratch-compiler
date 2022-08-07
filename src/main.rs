#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(once_cell)]

mod asset;
mod ast;
mod error;
mod ir;
mod macros;
mod optimize;
mod parser;
mod ser;
mod span;
mod uid;

use crate::{
    ir::Program, macros::expand, parser::input::Input, ser::write_sb3_file,
};
use codespan::Files;
use gumdrop::Options;
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Mutex,
};

lazy_static::lazy_static! {
    static ref FILES: Mutex<Files<String>> = Mutex::default();
}

#[derive(Options)]
/// Compiles Lisp code into Scratch projects.
struct Opts {
    /// Displays this help message
    help: bool,

    /// The source file to compile
    #[options(free, required)]
    file: PathBuf,
}

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

    if let Err(err) = expand(asts).and_then(|expanded| {
        let mut program = Program::from_asts(expanded)?;
        program.optimize();
        write_sb3_file(&program, Path::new("project.sb3"))
    }) {
        err.emit();
    }
}
