#[derive(Debug, Clone)]
pub(crate) enum Ast {
    Num(f64),
    String(String),
    Sym(String),
    Node(Box<Ast>, Vec<Ast>),
    Unquote(Box<Ast>),
}

impl Default for Ast {
    fn default() -> Self {
        Self::Num(0.0)
    }
}
