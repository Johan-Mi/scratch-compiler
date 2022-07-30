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
