use crate::{
    ast::{all_symbols, Ast},
    ir::proc::Procedure,
};
use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug)]
pub(crate) struct Sprite {
    costumes: HashMap<String, PathBuf>,
    variables: HashSet<String>,
    lists: HashSet<String>,
    procedures: HashMap<String, Procedure>,
}

impl Sprite {
    pub fn from_ast(ast: Ast) -> (String, Self) {
        // TODO: Error handling
        let mut tail = match ast {
            Ast::Node(box Ast::Sym(sym), tail) if sym == "sprite" => {
                tail.into_iter()
            }
            _ => todo!(),
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
                    "costumes" => todo!(),
                    "proc" => {
                        let (name, proc) = Procedure::from_asts(tail);
                        procedures.insert(name, proc);
                    }
                    _ => todo!(),
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
