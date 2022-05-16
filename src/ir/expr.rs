use std::{borrow::Cow, fmt};

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

impl Value {
    pub(crate) fn to_cow_str(&self) -> Cow<str> {
        match self {
            Value::Num(num) => Cow::Owned(number_to_string(*num)),
            Value::String(s) => Cow::Borrowed(s),
            Value::Bool(true) => Cow::Borrowed("true"),
            Value::Bool(false) => Cow::Borrowed("false"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_cow_str().fmt(f)
    }
}

fn number_to_string(num: f64) -> String {
    // FIXME: Rust does not format floats the same way as JavaScript.
    if num == f64::INFINITY {
        "Infinity".to_owned()
    } else if num == f64::NEG_INFINITY {
        "-Infinity".to_owned()
    } else {
        num.to_string()
    }
}
