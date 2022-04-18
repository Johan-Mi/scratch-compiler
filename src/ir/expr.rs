use crate::{
    ast::Ast,
    rewrite::{Clean, Rewrite, TreeWalk},
};

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

impl TreeWalk<Rewrite<Self>> for Expr {
    fn each_branch(
        self,
        f: impl FnMut(Self) -> Rewrite<Self>,
    ) -> Rewrite<Self> {
        match self {
            Expr::Lit(_) | Expr::Sym(_) => Clean(self),
            Expr::FuncCall(func_name, args) => args
                .into_iter()
                .map(f)
                .collect::<Rewrite<_>>()
                .map(|new_args| Expr::FuncCall(func_name, new_args)),
        }
    }
}

#[derive(Debug)]
pub(crate) enum Value {
    Num(f64),
    String(String),
    Bool(bool),
}
