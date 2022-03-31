mod ast;
mod parser;

fn main() {
    loop {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let parsed = parser::program(&input);
        match parsed {
            Ok((_, ast)) => println!("{ast:#?}"),
            Err(err) => eprintln!("{err}"),
        }
    }
}
