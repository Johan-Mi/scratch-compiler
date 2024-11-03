use codemap::Span;

#[derive(Debug, Clone)]
pub enum Ast {
    Num(f64, Span),
    Bool(bool, Span),
    String(String, Span),
    Sym(String, Span),
    Node(Box<Self>, Vec<Self>, Span),
    Unquote(Box<Self>, Span),
}

impl Ast {
    pub fn is_the_function_call(&self, func_name: &str) -> bool {
        matches!(self, Self::Node(box Self::Sym(sym, ..), ..) if sym == func_name)
    }

    pub const fn span(&self) -> Span {
        match *self {
            Self::Num(_, span)
            | Self::Bool(_, span)
            | Self::String(_, span)
            | Self::Sym(_, span)
            | Self::Node(_, _, span)
            | Self::Unquote(_, span) => span,
        }
    }

    pub fn traverse_postorder_mut<E>(
        &mut self,
        f: &mut impl FnMut(&mut Self) -> Result<(), E>,
    ) -> Result<(), E> {
        match self {
            Self::Num(..) | Self::Bool(..) | Self::String(..) | Self::Sym(..) => {}
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
