use crate::span::Span;
use trexp::{Clean, Rewrite, TreeWalk};

#[derive(Debug, Clone)]
pub enum Ast {
    Num(f64, Span),
    String(String, Span),
    Sym(String, Span),
    Node(Box<Ast>, Vec<Ast>, Span),
    Unquote(Box<Ast>, Span),
}

impl Ast {
    pub fn is_the_function_call(&self, func_name: &str) -> bool {
        matches!(self, Self::Node(box Ast::Sym(sym, ..), ..) if sym == func_name)
    }

    pub const fn span(&self) -> Span {
        match *self {
            Ast::Num(_, span)
            | Ast::String(_, span)
            | Ast::Sym(_, span)
            | Ast::Node(_, _, span)
            | Ast::Unquote(_, span) => span,
        }
    }
}

impl TreeWalk<Self> for Ast {
    fn each_branch(self, mut f: impl FnMut(Self) -> Self) -> Self {
        match self {
            Ast::Num(..) | Ast::String(..) | Ast::Sym(..) => self,
            Ast::Node(mut head, tail, span) => {
                *head = f(*head);
                Self::Node(head, tail.into_iter().map(f).collect(), span)
            }
            Ast::Unquote(mut unquoted, span) => {
                *unquoted = f(*unquoted);
                Self::Unquote(unquoted, span)
            }
        }
    }
}

impl<E> TreeWalk<Result<Self, E>> for Ast {
    fn each_branch(
        self,
        mut f: impl FnMut(Self) -> Result<Self, E>,
    ) -> Result<Self, E> {
        match self {
            Ast::Num(..) | Ast::String(..) | Ast::Sym(..) => Ok(self),
            Ast::Node(mut head, tail, span) => {
                *head = f(*head)?;
                let tail = tail.into_iter().map(f).collect::<Result<_, _>>()?;
                Ok(Self::Node(head, tail, span))
            }
            Ast::Unquote(mut unquoted, span) => {
                *unquoted = f(*unquoted)?;
                Ok(Self::Unquote(unquoted, span))
            }
        }
    }
}

impl<E> TreeWalk<Result<Rewrite<Self>, E>> for Ast {
    fn each_branch(
        self,
        mut f: impl FnMut(Self) -> Result<Rewrite<Self>, E>,
    ) -> Result<Rewrite<Self>, E> {
        match self {
            Ast::Num(..) | Ast::String(..) | Ast::Sym(..) => Ok(Clean(self)),
            Ast::Node(head, tail, span) => f(*head)?.try_bind(|head| {
                Ok(tail
                    .into_iter()
                    .map(f)
                    .collect::<Result<Rewrite<_>, _>>()?
                    .map(|tail| Self::Node(Box::new(head), tail, span)))
            }),
            Ast::Unquote(unquoted, span) => Ok(f(*unquoted)?
                .map(|unquoted| Self::Unquote(Box::new(unquoted), span))),
        }
    }
}

impl TreeWalk<Rewrite<Self>> for Ast {
    fn each_branch(
        self,
        mut f: impl FnMut(Self) -> Rewrite<Self>,
    ) -> Rewrite<Self> {
        match self {
            Ast::Num(..) | Ast::String(..) | Ast::Sym(..) => Clean(self),
            Ast::Node(head, tail, span) => f(*head).bind(|head| {
                tail.into_iter()
                    .map(f)
                    .collect::<Rewrite<_>>()
                    .map(|tail| Self::Node(Box::new(head), tail, span))
            }),
            Ast::Unquote(unquoted, span) => f(*unquoted)
                .map(|unquoted| Self::Unquote(Box::new(unquoted), span)),
        }
    }
}

pub fn all_symbols(asts: Vec<Ast>) -> Vec<String> {
    asts.into_iter()
        .map(|ast| match ast {
            Ast::Sym(sym, ..) => sym,
            // TODO: Error handling
            _ => todo!(),
        })
        .collect()
}
