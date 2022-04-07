#![feature(box_patterns)]

mod ast;
mod ir;
mod macros;
mod parser;

use crate::{ir::Program, macros::expand};

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let parsed = parser::program(&input);
        match parsed {
            Ok((_, ast)) => {
                let expanded = expand(ast);
                let program = Program::from_asts(expanded);
                println!("{program:#?}");
            }
            Err(err) => eprintln!("{err}"),
        }
    }
}
