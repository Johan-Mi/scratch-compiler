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
        matches!(self, Self::Node(box Self::Sym(sym, ..), ..) if sym == func_name)
    }

    pub const fn span(&self) -> Span {
        match *self {
            Self::Num(_, span)
            | Self::String(_, span)
            | Self::Sym(_, span)
            | Self::Node(_, _, span)
            | Self::Unquote(_, span) => span,
        }
    }
}

impl TreeWalk<Self> for Ast {
    fn each_branch(self, mut f: impl FnMut(Self) -> Self) -> Self {
        match self {
            Self::Num(..) | Self::String(..) | Self::Sym(..) => self,
            Self::Node(mut head, tail, span) => {
                *head = f(*head);
                Self::Node(head, tail.into_iter().map(f).collect(), span)
            }
            Self::Unquote(mut unquoted, span) => {
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
            Self::Num(..) | Self::String(..) | Self::Sym(..) => Ok(self),
            Self::Node(mut head, tail, span) => {
                *head = f(*head)?;
                let tail = tail.into_iter().map(f).collect::<Result<_, _>>()?;
                Ok(Self::Node(head, tail, span))
            }
            Self::Unquote(mut unquoted, span) => {
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
            Self::Num(..) | Self::String(..) | Self::Sym(..) => Ok(Clean(self)),
            Self::Node(head, tail, span) => f(*head)?.try_bind(|head| {
                Ok(tail
                    .into_iter()
                    .map(f)
                    .collect::<Result<Rewrite<_>, _>>()?
                    .map(|tail| Self::Node(Box::new(head), tail, span)))
            }),
            Self::Unquote(unquoted, span) => Ok(f(*unquoted)?
                .map(|unquoted| Self::Unquote(Box::new(unquoted), span))),
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
