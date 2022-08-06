use crate::{
    ast::{all_symbols, Ast},
    error::{Error, Result},
    ir::proc::Procedure,
};
use fancy_match::fancy_match;
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
        // TODO: Error handling
        let (mut tail, span) = #[fancy_match]
        match ast {
            Ast::Node(box Ast::Sym("sprite", ..), tail, span) => {
                (tail.into_iter(), span)
            }
            _ => todo!("invalid sprite definition:\n{ast:#?}"),
        };

        let name = match tail.next() {
            Some(Ast::String(name, ..)) => Ok(name),
            Some(Ast::Sym(_, sym_span)) => {
                Err(Box::new(Error::SpriteMissingName {
                    span,
                    candidate_symbol: Some(sym_span),
                }))
            }
            _ => Err(Box::new(Error::SpriteMissingName {
                span,
                candidate_symbol: None,
            })),
        }?;

        let mut costumes = HashMap::new();
        let mut variables = HashSet::new();
        let mut lists = HashSet::new();
        let mut procedures = HashMap::new();

        for decl in tail {
            let span = decl.span();
            match decl {
                Ast::Node(box Ast::Sym(sym, ..), tail, ..) => match &*sym {
                    "variables" => variables.extend(all_symbols(tail)),
                    "lists" => lists.extend(all_symbols(tail)),
                    "costumes" => parse_costume_decl(&mut costumes, tail),
                    "proc" => {
                        let (name, proc) = Procedure::from_asts(tail)?;
                        procedures
                            .entry(name)
                            .or_insert_with(|| Vec::with_capacity(1))
                            .push(proc);
                    }
                    _ => {
                        return Err(Box::new(Error::InvalidItemInSprite {
                            span,
                        }))
                    }
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
        let name = match name {
            Ast::String(name, ..) => name,
            _ => todo!(),
        };
        let path = args.next().unwrap();
        let path = match path {
            Ast::String(path, ..) => path,
            _ => todo!(),
        };
        costumes.insert(name, path.into());
    }
}
