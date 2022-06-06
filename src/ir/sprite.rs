use crate::{
    ast::{all_symbols, Ast},
    ir::proc::Procedure,
};
use fancy_match::fancy_match;
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug)]
pub struct Sprite {
    pub costumes: HashMap<String, PathBuf>,
    pub variables: HashSet<String>,
    pub lists: HashSet<String>,
    pub procedures: HashMap<String, Procedure>,
}

impl Sprite {
    pub fn from_ast(ast: Ast) -> (String, Self) {
        // TODO: Error handling
        let mut tail = #[fancy_match]
        match ast {
            Ast::Node(box Ast::Sym("sprite"), tail) => tail.into_iter(),
            _ => todo!("invalid sprite definition:\n{ast:#?}"),
        };

        let name = match tail.next() {
            Some(Ast::String(name)) => name,
            _ => todo!(),
        };

        let mut costumes = HashMap::new();
        let mut variables = HashSet::new();
        let mut lists = HashSet::new();
        let mut procedures = HashMap::new();

        for decl in tail {
            match decl {
                Ast::Node(box Ast::Sym(sym), tail) => match &*sym {
                    "variables" => variables.extend(all_symbols(tail)),
                    "lists" => lists.extend(all_symbols(tail)),
                    "costumes" => parse_costume_decl(&mut costumes, tail),
                    "proc" => {
                        let (name, proc) = Procedure::from_asts(tail);
                        procedures.insert(name, proc);
                    }
                    _ => todo!("invalid item in sprite `{name}`: `{sym}`"),
                },
                _ => todo!(),
            }
        }

        (
            name,
            Self {
                costumes,
                variables,
                lists,
                procedures,
            },
        )
    }

    pub fn merge(&mut self, other: Self) {
        let Self {
            costumes,
            variables,
            lists,
            procedures,
        } = other;
        self.costumes.extend(costumes);
        self.variables.extend(variables);
        self.lists.extend(lists);
        self.procedures.extend(procedures);
    }

    pub fn optimize(&mut self) {
        for proc in self.procedures.values_mut() {
            proc.optimize();
        }
    }
}

fn parse_costume_decl(costumes: &mut HashMap<String, PathBuf>, args: Vec<Ast>) {
    // TODO: Error handling
    let mut args = args.into_iter();
    while let Some(name) = args.next() {
        let name = match name {
            Ast::String(name) => name,
            _ => todo!(),
        };
        let path = args.next().unwrap();
        let path = match path {
            Ast::String(path) => path,
            _ => todo!(),
        };
        costumes.insert(name, path.into());
    }
}
