#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![feature(once_cell)]

mod asset;
mod ast;
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
use std::{fs, path::Path, sync::Mutex};

lazy_static::lazy_static! {
    static ref FILES: Mutex<Files<String>> = Mutex::default();
}

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
    let main_file_id = FILES.lock().unwrap().add(source_path, input.clone());

    let asts = match parser::program(Input::new(&input, main_file_id)) {
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
