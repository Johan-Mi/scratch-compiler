#![feature(box_patterns)]

mod ast;
mod macros;
mod parser;

use crate::macros::expand;

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let parsed = parser::program(&input);
        match parsed {
            Ok((_, ast)) => {
                let expanded = expand(ast);
                println!("{expanded:#?}");
            }
            Err(err) => eprintln!("{err}"),
        }
    }
}
