use crate::{
    ast::Ast, diagnostic::Result, ir::expr::Expr,
    optimize::statement::optimize_stmt, span::Span,
};

#[derive(Debug)]
pub enum Statement {
    ProcCall {
        proc_name: String,
        proc_span: Span,
        args: Vec<Expr>,
    },
    Do(Vec<Self>),
    IfElse {
        condition: Expr,
        then: Box<Self>,
        else_: Box<Self>,
        span: Span,
    },
    Repeat {
        times: Expr,
        body: Box<Self>,
    },
    Forever(Box<Self>),
    Until {
        condition: Expr,
        body: Box<Self>,
    },
    While {
        condition: Expr,
        body: Box<Self>,
    },
    For {
        counter: (String, Span),
        times: Expr,
        body: Box<Self>,
    },
}

impl Default for Statement {
    fn default() -> Self {
        Self::Do(Vec::new())
    }
}

impl Statement {
    pub fn from_ast(ast: Ast) -> Result<Self> {
        // TODO: Error handling
        let full_span = ast.span();
        let Ast::Node(box Ast::Sym(sym, sym_span), tail, ..) = ast else {
            todo!();
        };
        let mut tail = tail.into_iter();
        Ok(match &*sym {
            "do" => Self::Do(tail.map(Self::from_ast).collect::<Result<_>>()?),
            "if" => {
                let condition = tail.next().unwrap();
                let then = tail.next().unwrap();
                let else_ = tail.next().unwrap();
                assert!(tail.next().is_none());
                Self::IfElse {
                    condition: Expr::from_ast(condition)?,
                    then: Box::new(Self::from_ast(then)?),
                    else_: Box::new(Self::from_ast(else_)?),
                    span: full_span,
                }
            }
            "repeat" => {
                let times = tail.next().unwrap();
                Self::Repeat {
                    times: Expr::from_ast(times)?,
                    body: Box::new(Self::Do(
                        tail.map(Self::from_ast).collect::<Result<_>>()?,
                    )),
                }
            }
            "forever" => Self::Forever(Box::new(Self::Do(
                tail.map(Self::from_ast).collect::<Result<_>>()?,
            ))),
            "until" => {
                let condition = tail.next().unwrap();
                Self::Until {
                    condition: Expr::from_ast(condition)?,
                    body: Box::new(Self::Do(
                        tail.map(Self::from_ast).collect::<Result<_>>()?,
                    )),
                }
            }
            "while" => {
                let condition = tail.next().unwrap();
                Self::While {
                    condition: Expr::from_ast(condition)?,
                    body: Box::new(Self::Do(
                        tail.map(Self::from_ast).collect::<Result<_>>()?,
                    )),
                }
            }
            "for" => {
                let counter = tail.next().unwrap();
                let counter = match counter {
                    Ast::Sym(sym, span) => (sym, span),
                    _ => todo!(),
                };
                let times = tail.next().unwrap();
                Self::For {
                    counter,
                    times: Expr::from_ast(times)?,
                    body: Box::new(Self::Do(
                        tail.map(Self::from_ast).collect::<Result<_>>()?,
                    )),
                }
            }
            "when" => {
                let condition = tail.next().unwrap();
                Self::IfElse {
                    condition: Expr::from_ast(condition)?,
                    then: Box::new(Self::Do(
                        tail.map(Self::from_ast).collect::<Result<_>>()?,
                    )),
                    else_: Box::new(Self::Do(Vec::new())),
                    span: full_span,
                }
            }
            "unless" => {
                let condition = tail.next().unwrap();
                Self::IfElse {
                    condition: Expr::from_ast(condition)?,
                    then: Box::new(Self::Do(Vec::new())),
                    else_: Box::new(Self::Do(
                        tail.map(Self::from_ast).collect::<Result<_>>()?,
                    )),
                    span: full_span,
                }
            }
            "cond" => {
                let mut cases = Vec::with_capacity(tail.len() / 2);
                let mut else_branch = Self::Do(Vec::new());
                while let Some(ast) = tail.next() {
                    match tail.next() {
                        Some(body) => cases.push((
                            Expr::from_ast(ast)?,
                            Self::from_ast(body)?,
                        )),
                        None => else_branch = Self::from_ast(ast)?,
                    }
                }
                cases.into_iter().rfold(
                    else_branch,
                    |acc, (condition, then)| Self::IfElse {
                        condition,
                        then: Box::new(then),
                        else_: Box::new(acc),
                        span: full_span,
                    },
                )
            }
            _ => Self::ProcCall {
                proc_name: sym,
                proc_span: sym_span,
                args: tail.map(Expr::from_ast).collect::<Result<_>>()?,
            },
        })
    }

    pub fn optimize(&mut self) {
        optimize_stmt(self);
    }

    pub fn traverse_postorder_mut(&mut self, f: &mut impl FnMut(&mut Self)) {
        match self {
            Self::ProcCall {
                proc_name: _,
                proc_span: _,
                args: _,
            } => {}
            Self::Do(stmts) => {
                for stmt in stmts {
                    stmt.traverse_postorder_mut(f);
                }
            }
            Self::IfElse {
                condition: _,
                then,
                else_,
                ..
            } => {
                then.traverse_postorder_mut(f);
                else_.traverse_postorder_mut(f);
            }
            Self::Repeat { times: _, body }
            | Self::Forever(body)
            | Self::Until { condition: _, body }
            | Self::While { condition: _, body }
            | Self::For {
                counter: _,
                times: _,
                body,
            } => body.traverse_postorder_mut(f),
        }
        f(self);
    }

    pub fn is_nop(&self) -> bool {
        matches!(self, Self::Do(stmts) if stmts.is_empty())
    }
}
