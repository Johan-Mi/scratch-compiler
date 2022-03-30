#[derive(Debug)]
pub(crate) enum Ast {
    Num(f64),
    String(String),
    Sym(String),
    Node(Box<Ast>, Vec<Ast>),
    Unquote(Box<Ast>),
}
