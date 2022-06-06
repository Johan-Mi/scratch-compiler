use trexp::{Clean, Rewrite, TreeWalk};

#[derive(Debug, Clone)]
pub enum Ast {
    Num(f64),
    String(String),
    Sym(String),
    Node(Box<Ast>, Vec<Ast>),
    Unquote(Box<Ast>),
}

impl Ast {
    pub fn is_the_function_call(&self, func_name: &str) -> bool {
        matches!(self, Self::Node(box Ast::Sym(sym), _) if sym == func_name)
    }
}

impl TreeWalk<Self> for Ast {
    fn each_branch(self, mut f: impl FnMut(Self) -> Self) -> Self {
        match self {
            Ast::Num(_) | Ast::String(_) | Ast::Sym(_) => self,
            Ast::Node(mut head, tail) => {
                *head = f(*head);
                Self::Node(head, tail.into_iter().map(f).collect())
            }
            Ast::Unquote(mut unquoted) => {
                *unquoted = f(*unquoted);
                Self::Unquote(unquoted)
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
            Ast::Num(_) | Ast::String(_) | Ast::Sym(_) => Clean(self),
            Ast::Node(head, tail) => f(*head).bind(|head| {
                tail.into_iter()
                    .map(f)
                    .collect::<Rewrite<_>>()
                    .map(|tail| Self::Node(Box::new(head), tail))
            }),
            Ast::Unquote(unquoted) => {
                f(*unquoted).map(|unquoted| Self::Unquote(Box::new(unquoted)))
            }
        }
    }
}

pub fn all_symbols(asts: Vec<Ast>) -> Vec<String> {
    asts.into_iter()
        .map(|ast| match ast {
            Ast::Sym(sym) => sym,
            // TODO: Error handling
            _ => todo!(),
        })
        .collect()
}
