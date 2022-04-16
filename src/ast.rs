use crate::rewrite::TreeWalk;

#[derive(Debug, Clone)]
pub(crate) enum Ast {
    Num(f64),
    String(String),
    Sym(String),
    Node(Box<Ast>, Vec<Ast>),
    Unquote(Box<Ast>),
}

impl TreeWalk<Ast> for Ast {
    fn each_branch(self, mut f: impl FnMut(Self) -> Self) -> Self {
        match self {
            Ast::Num(_) | Ast::String(_) | Ast::Sym(_) => self,
            Ast::Node(mut head, tail) => {
                *head = f(*head);
                Ast::Node(head, tail.into_iter().map(f).collect())
            }
            Ast::Unquote(mut unquoted) => {
                *unquoted = f(*unquoted);
                Ast::Unquote(unquoted)
            }
        }
    }
}

pub(crate) fn all_symbols(asts: Vec<Ast>) -> Vec<String> {
    asts.into_iter()
        .map(|ast| match ast {
            Ast::Sym(sym) => sym,
            // TODO: Error handling
            _ => todo!(),
        })
        .collect()
}
