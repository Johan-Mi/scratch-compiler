#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]

mod asset;
mod ast;
mod ir;
mod macros;
mod optimize;
mod parser;
mod rewrite;
mod ser;
mod uid;

use crate::{ir::Program, macros::expand, ser::write_sb3_file};
use std::{fs, path::Path};

fn main() {
    // TODO: Error handling
    let input = fs::read_to_string("program.scratch").unwrap();
    let parsed = parser::program(&input);
    match parsed {
        Ok((_, ast)) => {
            let expanded = expand(ast);
            let mut program = Program::from_asts(expanded);
            program.optimize();
            write_sb3_file(&program, Path::new("project.sb3"));
        }
        Err(err) => eprintln!("{err}"),
    }
}
