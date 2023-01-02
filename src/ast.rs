use crate::span::Span;
use std::ops::ControlFlow;

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

    pub fn traverse_postorder_mut<B>(
        &mut self,
        f: &mut impl FnMut(&mut Self) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match self {
            Self::Num(_, _) => {}
            Self::String(_, _) => {}
            Self::Sym(_, _) => {}
            Self::Node(head, tail, _) => {
                head.traverse_postorder_mut(f)?;
                for branch in tail {
                    branch.traverse_postorder_mut(f)?;
                }
            }
            Self::Unquote(unquoted, _) => unquoted.traverse_postorder_mut(f)?,
        }
        f(self)
    }
}

pub fn all_symbols(asts: Vec<Ast>) -> Result<Vec<String>, Ast> {
    asts.into_iter()
        .map(|ast| match ast {
            Ast::Sym(sym, ..) => Ok(sym),
            _ => Err(ast),
        })
        .collect()
}
