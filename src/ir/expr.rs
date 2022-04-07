use crate::ast::Ast;

#[derive(Debug)]
pub(crate) enum Expr {
    Lit(Value),
    Sym(String),
    FuncCall(String, Vec<Expr>),
}

impl Expr {
    pub fn from_ast(ast: Ast) -> Self {
        // TODO: Error handling
        match ast {
            Ast::Num(num) => Self::Lit(Value::Num(num)),
            Ast::String(s) => Self::Lit(Value::String(s)),
            Ast::Sym(sym) => Self::Sym(sym),
            Ast::Node(box Ast::Sym(func_name), args) => Self::FuncCall(
                func_name,
                args.into_iter().map(Self::from_ast).collect(),
            ),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Value {
    Num(f64),
    String(String),
    Bool(bool),
}
