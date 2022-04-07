#[derive(Debug, Clone)]
pub(crate) enum Ast {
    Num(f64),
    String(String),
    Sym(String),
    Node(Box<Ast>, Vec<Ast>),
    Unquote(Box<Ast>),
}

impl Ast {
    pub fn each_subtree(self, mut f: impl FnMut(Ast) -> Ast) -> Ast {
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

impl Default for Ast {
    fn default() -> Self {
        Self::Num(0.0)
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
