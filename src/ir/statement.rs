use crate::{ast::Ast, ir::expr::Expr, optimize::statement::optimize_stmt};
use std::mem;
use trexp::{Clean, Rewrite, TreeWalk};

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
    pub fn from_ast(ast: Ast) -> Self {
        // TODO: Error handling
        match ast {
            Ast::Node(box Ast::Sym(sym, ..), tail, ..) => {
                let mut tail = tail.into_iter();
                match &*sym {
                    "do" => Self::Do(tail.map(Self::from_ast).collect()),
                    "if" => {
                        let condition = tail.next().unwrap();
                        let if_true = tail.next().unwrap();
                        let if_false = tail.next().unwrap();
                        assert!(tail.next().is_none());
                        Self::IfElse {
                            condition: Expr::from_ast(condition),
                            if_true: Box::new(Self::from_ast(if_true)),
                            if_false: Box::new(Self::from_ast(if_false)),
                        }
                    }
                    "repeat" => {
                        let times = tail.next().unwrap();
                        Self::Repeat {
                            times: Expr::from_ast(times),
                            body: Box::new(Self::Do(
                                tail.map(Self::from_ast).collect(),
                            )),
                        }
                    }
                    "forever" => Self::Forever(Box::new(Self::Do(
                        tail.map(Self::from_ast).collect(),
                    ))),
                    "until" => {
                        let condition = tail.next().unwrap();
                        Self::Until {
                            condition: Expr::from_ast(condition),
                            body: Box::new(Self::Do(
                                tail.map(Self::from_ast).collect(),
                            )),
                        }
                    }
                    "while" => {
                        let condition = tail.next().unwrap();
                        Self::While {
                            condition: Expr::from_ast(condition),
                            body: Box::new(Self::Do(
                                tail.map(Self::from_ast).collect(),
                            )),
                        }
                    }
                    "for" => {
                        let counter = tail.next().unwrap();
                        let counter = match counter {
                            Ast::Sym(sym, ..) => sym,
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
                        let condition = tail.next().unwrap();
                        Self::IfElse {
                            condition: Expr::from_ast(condition),
                            if_true: Box::new(Self::Do(Vec::new())),
                            if_false: Box::new(Self::Do(
                                tail.map(Self::from_ast).collect(),
                            )),
                        }
                    }
                    "cond" => {
                        let mut cases = Vec::with_capacity(tail.len() / 2);
                        let mut else_branch = Self::Do(Vec::new());
                        while let Some(ast) = tail.next() {
                            match tail.next() {
                                Some(body) => cases.push((
                                    Expr::from_ast(ast),
                                    Self::from_ast(body),
                                )),
                                None => else_branch = Self::from_ast(ast),
                            }
                        }
                        cases.into_iter().rfold(
                            else_branch,
                            |acc, (condition, then)| Self::IfElse {
                                condition,
                                if_true: Box::new(then),
                                if_false: Box::new(acc),
                            },
                        )
                    }
                    _ => Self::ProcCall {
                        proc_name: sym,
                        args: tail.map(Expr::from_ast).collect(),
                    },
                }
            }
            _ => todo!(),
        }
    }

    pub fn optimize(&mut self) {
        let placeholder = Self::Do(Vec::new());
        let this = mem::replace(self, placeholder);
        *self = optimize_stmt(this).into_inner();
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
