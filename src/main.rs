#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]

mod asset;
mod ast;
mod ir;
mod macros;
mod optimize;
mod parser;
mod ser;
mod uid;

use crate::{ir::Program, macros::expand, ser::write_sb3_file};
use std::{fs, path::Path};

fn main() {
    let mut args = std::env::args().skip(1);
    let source_path = args.next();
    let source_path = source_path.as_deref().unwrap_or("program.scratch");
    let input = match fs::read_to_string(source_path) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("IO error: {err}");
            return;
        }
    };

    let asts = match parser::program(&input) {
        Ok((_, asts)) => asts,
        Err(err) => {
            eprintln!("{err}");
            return;
        }
    };

    let expanded = expand(asts);
    let mut program = Program::from_asts(expanded);
    program.optimize();
    write_sb3_file(&program, Path::new("project.sb3"));
}
