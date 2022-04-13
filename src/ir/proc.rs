use crate::{
    ast::{all_symbols, Ast},
    ir::expr::Expr,
};
use std::collections::HashSet;

#[derive(Debug)]
pub(crate) struct Procedure {
    params: Vec<Expr>,
    body: Statement,
    variables: HashSet<String>,
    lists: HashSet<String>,
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
            match stmt_or_decl {
                Ast::Node(box Ast::Sym(sym), var_decls)
                    if sym == "variables" =>
                {
                    variables.extend(all_symbols(var_decls));
                }
                Ast::Node(box Ast::Sym(sym), list_decls) if sym == "lists" => {
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
pub(crate) enum Statement {
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
        counter: Expr,
        times: Expr,
        body: Box<Statement>,
    },
}

impl Statement {
    fn from_ast(ast: Ast) -> Statement {
        // TODO: Error handling
        match ast {
            Ast::Node(box Ast::Sym(sym), tail) => match &*sym {
                "do" => Statement::Do(
                    tail.into_iter().map(Statement::from_ast).collect(),
                ),
                "if" => todo!(),
                "repeat" => {
                    let mut tail = tail.into_iter();
                    let times = tail.next().unwrap();
                    Statement::Repeat {
                        times: Expr::from_ast(times),
                        body: Box::new(Statement::Do(
                            tail.map(Statement::from_ast).collect(),
                        )),
                    }
                }
                "forever" => Statement::Forever(Box::new(Statement::Do(
                    tail.into_iter().map(Statement::from_ast).collect(),
                ))),
                "until" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Statement::Until {
                        condition: Expr::from_ast(condition),
                        body: Box::new(Statement::Do(
                            tail.map(Statement::from_ast).collect(),
                        )),
                    }
                }
                "while" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Statement::While {
                        condition: Expr::from_ast(condition),
                        body: Box::new(Statement::Do(
                            tail.map(Statement::from_ast).collect(),
                        )),
                    }
                }
                "for" => {
                    let mut tail = tail.into_iter();
                    let counter = tail.next().unwrap();
                    let times = tail.next().unwrap();
                    Statement::For {
                        counter: Expr::from_ast(counter),
                        times: Expr::from_ast(times),
                        body: Box::new(Statement::Do(
                            tail.map(Statement::from_ast).collect(),
                        )),
                    }
                }
                "when" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Statement::IfElse {
                        condition: Expr::from_ast(condition),
                        if_true: Box::new(Statement::Do(
                            tail.map(Statement::from_ast).collect(),
                        )),
                        if_false: Box::new(Statement::Do(Vec::new())),
                    }
                }
                "unless" => {
                    let mut tail = tail.into_iter();
                    let condition = tail.next().unwrap();
                    Statement::IfElse {
                        condition: Expr::from_ast(condition),
                        if_true: Box::new(Statement::Do(Vec::new())),
                        if_false: Box::new(Statement::Do(
                            tail.map(Statement::from_ast).collect(),
                        )),
                    }
                }
                _ => Statement::ProcCall {
                    proc_name: sym,
                    args: tail.into_iter().map(Expr::from_ast).collect(),
                },
            },
            _ => todo!(),
        }
    }

    fn optimize(&mut self) {
        // TODO
    }
}
