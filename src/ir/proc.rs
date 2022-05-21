use crate::{
    ast::{all_symbols, Ast},
    ir::expr::Expr,
    optimize::statement::optimize_stmt,
    rewrite::{Clean, Rewrite, TreeWalk},
};
use fancy_match::fancy_match;
use std::{collections::HashSet, mem};

#[derive(Debug)]
pub struct Procedure {
    pub params: Vec<Expr>,
    pub body: Statement,
    pub variables: HashSet<String>,
    pub lists: HashSet<String>,
}

impl Procedure {
    pub fn from_asts(args: Vec<Ast>) -> (String, Self) {
        // TODO: Error handling
        let mut args = args.into_iter();
        let signature = args.next().unwrap();
        let (name, params) = parse_signature(signature);
        let mut body = Vec::new();
        let mut variables = HashSet::new();
        let mut lists = HashSet::new();

        for stmt_or_decl in args {
            #[fancy_match]
            match stmt_or_decl {
                Ast::Node(box Ast::Sym("variables"), var_decls) => {
                    variables.extend(all_symbols(var_decls));
                }
                Ast::Node(box Ast::Sym("lists"), list_decls) => {
                    lists.extend(all_symbols(list_decls));
                }
                _ => body.push(Statement::from_ast(stmt_or_decl)),
            }
        }

        (
            name,
            Self {
                params,
                body: Statement::Do(body),
                variables,
                lists,
            },
        )
    }

    pub fn optimize(&mut self) {
        self.body.optimize();
    }
}

fn parse_signature(ast: Ast) -> (String, Vec<Expr>) {
    // TODO: Error handling
    match ast {
        Ast::Node(box Ast::Sym(name), params) => {
            let params = params.into_iter().map(Expr::from_ast).collect();
            (name, params)
        }
        _ => todo!(),
    }
}

#[derive(Debug)]
pub enum Statement {
    ProcCall {
        proc_name: String,
        args: Vec<Expr>,
    },
    Do(Vec<Statement>),
    IfElse {
        condition: Expr,
        if_true: Box<Statement>,
        if_false: Box<Statement>,
    },
    Repeat {
        times: Expr,
        body: Box<Statement>,
    },
    Forever(Box<Statement>),
    Until {
        condition: Expr,
        body: Box<Statement>,
    },
    While {
        condition: Expr,
        body: Box<Statement>,
    },
    For {
        counter: String,
        times: Expr,
        body: Box<Statement>,
    },
}

impl Statement {
    fn from_ast(ast: Ast) -> Self {
        // TODO: Error handling
        match ast {
            Ast::Node(box Ast::Sym(sym), tail) => match &*sym {
                "do" => Self::Do(
                    tail.into_iter().map(Self::from_ast).collect(),
                ),
                "if" => todo!(),
                "repeat" => {
                    let mut tail = tail.into_iter();
                    let times = tail.next().unwrap();
                    Self::Repeat {
                        times: Expr::from_ast(times),
                        body: Box::new(Self::Do(
                            tail.map(Self::from_ast).collect(),
                        )),
                    }
                }
                "forever" => Self::Forever(Box::new(Self::Do(
                    tail.into_iter().map(Self::from_ast).collect(),
                ))),
                "until" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Self::Until {
                        condition: Expr::from_ast(condition),
                        body: Box::new(Self::Do(
                            tail.map(Self::from_ast).collect(),
                        )),
                    }
                }
                "while" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Self::While {
                        condition: Expr::from_ast(condition),
                        body: Box::new(Self::Do(
                            tail.map(Self::from_ast).collect(),
                        )),
                    }
                }
                "for" => {
                    let mut tail = tail.into_iter();
                    let counter = tail.next().unwrap();
                    let counter = match counter {
                        Ast::Sym(sym) => sym,
                        _ => todo!(),
                    };
                    let times = tail.next().unwrap();
                    Self::For {
                        counter,
                        times: Expr::from_ast(times),
                        body: Box::new(Self::Do(
                            tail.map(Self::from_ast).collect(),
                        )),
                    }
                }
                "when" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Self::IfElse {
                        condition: Expr::from_ast(condition),
                        if_true: Box::new(Self::Do(
                            tail.map(Self::from_ast).collect(),
                        )),
                        if_false: Box::new(Self::Do(Vec::new())),
                    }
                }
                "unless" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Self::IfElse {
                        condition: Expr::from_ast(condition),
                        if_true: Box::new(Self::Do(Vec::new())),
                        if_false: Box::new(Self::Do(
                            tail.map(Self::from_ast).collect(),
                        )),
                    }
                }
                _ => Self::ProcCall {
                    proc_name: sym,
                    args: tail.into_iter().map(Expr::from_ast).collect(),
                },
            },
            _ => todo!(),
        }
    }

    fn optimize(&mut self) {
        let placeholder = Self::Do(Vec::new());
        let this = mem::replace(self, placeholder);
        *self = optimize_stmt(this).into_inner()
    }
}

impl TreeWalk<Rewrite<Self>> for Statement {
    fn each_branch(
        self,
        mut f: impl FnMut(Self) -> Rewrite<Self>,
    ) -> Rewrite<Self> {
        match self {
            Self::ProcCall { .. } => Clean(self),
            Self::Do(body) => body
                .into_iter()
                .map(f)
                .collect::<Rewrite<_>>()
                .map(Self::Do),
            Self::IfElse {
                condition,
                if_true,
                if_false,
            } => f(*if_true).bind(|if_true| {
                f(*if_false).map(|if_false| Self::IfElse {
                    condition,
                    if_true: Box::new(if_true),
                    if_false: Box::new(if_false),
                })
            }),
            Self::Repeat { times, body } => f(*body).map(|body| Self::Repeat {
                times,
                body: Box::new(body),
            }),
            Self::Forever(body) => {
                f(*body).map(|body| Self::Forever(Box::new(body)))
            }
            Self::Until { condition, body } => {
                f(*body).map(|body| Self::Until {
                    condition,
                    body: Box::new(body),
                })
            }
            Self::While { condition, body } => {
                f(*body).map(|body| Self::While {
                    condition,
                    body: Box::new(body),
                })
            }
            Self::For {
                counter,
                times,
                body,
            } => f(*body).map(|body| Self::For {
                counter,
                times,
                body: Box::new(body),
            }),
        }
    }
}
