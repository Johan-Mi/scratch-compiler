use crate::{
    ast::{all_symbols, Ast},
    diagnostic::{Error, Result},
    ir::proc::Procedure,
};
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    path::PathBuf,
};

#[derive(Debug)]
pub struct Sprite {
    pub costumes: HashMap<String, PathBuf>,
    pub variables: HashSet<String>,
    pub lists: HashSet<String>,
    pub procedures: HashMap<String, Vec<Procedure>>,
}

impl Sprite {
    pub fn from_ast(ast: Ast) -> Result<(String, Self)> {
        let (mut tail, span) = match ast {
            Ast::Node(box Ast::Sym("sprite", ..), tail, span) => Ok((tail.into_iter(), span)),
            _ => Err(Error::InvalidTopLevelItem { span: ast.span() }),
        }?;

        let name = match tail.next() {
            Some(Ast::String(name, ..)) => Ok(name),
            Some(Ast::Sym(_, sym_span)) => Err(Error::SpriteMissingName {
                span,
                candidate_symbol: Some(sym_span),
            }),
            _ => Err(Error::SpriteMissingName {
                span,
                candidate_symbol: None,
            }),
        }?;

        let mut costumes = HashMap::new();
        let mut variables = HashSet::new();
        let mut lists = HashSet::new();
        let mut procedures = HashMap::new();

        for decl in tail {
            let span = decl.span();
            match decl {
                Ast::Node(box Ast::Sym(sym, ..), tail, ..) => match &*sym {
                    // TODO: Error handling
                    "variables" => variables.extend(all_symbols(tail).unwrap()),
                    "lists" => lists.extend(all_symbols(tail).unwrap()),
                    "costumes" => parse_costume_decl(&mut costumes, tail),
                    "proc" => {
                        let (name, proc) = Procedure::from_asts(tail)?;
                        procedures
                            .entry(name)
                            .or_insert_with(|| Vec::with_capacity(1))
                            .push(proc);
                    }
                    _ => return Err(Box::new(Error::InvalidItemInSprite { span })),
                },
                _ => return Err(Box::new(Error::InvalidItemInSprite { span })),
            }
        }

        Ok((
            name,
            Self {
                costumes,
                variables,
                lists,
                procedures,
            },
        ))
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
        for (name, procs) in procedures {
            match self.procedures.entry(name) {
                Entry::Occupied(mut occupied) => {
                    occupied.get_mut().extend(procs);
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(procs);
                }
            }
        }
    }

    pub fn optimize(&mut self) {
        for proc in self.procedures.values_mut().flatten() {
            proc.optimize();
        }
    }
}

fn parse_costume_decl(costumes: &mut HashMap<String, PathBuf>, args: Vec<Ast>) {
    // TODO: Error handling
    let mut args = args.into_iter();
    while let Some(name) = args.next() {
        let Ast::String(name, ..) = name else {
            todo!();
        };
        let path = args.next().unwrap();
        let Ast::String(path, ..) = path else {
            todo!();
        };
        costumes.insert(name, path.into());
    }
}
