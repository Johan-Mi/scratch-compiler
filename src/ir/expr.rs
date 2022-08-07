use crate::{
    ast::Ast,
    error::{Error, Result},
    span::Span,
};
use lazy_static::lazy_static;
use sb3_stuff::Value;
use smol_str::SmolStr;
use std::collections::HashSet;
use trexp::{Clean, Rewrite, TreeWalk};

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Value),
    Sym(SmolStr, Span),
    FuncCall(&'static str, Span, Vec<Expr>),
}

impl Expr {
    pub fn from_ast(ast: Ast) -> Result<Self> {
        // TODO: Error handling
        Ok(match ast {
            Ast::Num(num, ..) => Self::Lit(Value::Num(num)),
            Ast::String(s, ..) => Self::Lit(Value::String(s.into())),
            Ast::Sym(sym, span) => Self::Sym(sym.into(), span),
            Ast::Node(box Ast::Sym(func_name, span), args, ..) => {
                let func_name =
                    KNOWN_FUNC_NAMES.get(&*func_name).ok_or_else(|| {
                        Box::new(Error::UnknownFunction { span, func_name })
                    })?;
                Self::FuncCall(
                    func_name,
                    span,
                    args.into_iter()
                        .map(Self::from_ast)
                        .collect::<Result<_>>()?,
                )
            }
            _ => todo!(),
        })
    }

    #[must_use]
    pub const fn as_lit(&self) -> Option<&Value> {
        match self {
            Self::Lit(v) => Some(v),
            _ => None,
        }
    }
}

impl TreeWalk<Rewrite<Self>> for Expr {
    fn each_branch(
        self,
        f: impl FnMut(Self) -> Rewrite<Self>,
    ) -> Rewrite<Self> {
        match self {
            Expr::Lit(_) | Expr::Sym(..) => Clean(self),
            Expr::FuncCall(func_name, func_span, args) => {
                args.into_iter().map(f).collect::<Rewrite<_>>().map(
                    |new_args| Self::FuncCall(func_name, func_span, new_args),
                )
            }
        }
    }
}

lazy_static! {
    static ref KNOWN_FUNC_NAMES: HashSet<&'static str> = HashSet::from([
        "+",
        "-",
        "*",
        "/",
        "!!",
        "++",
        "and",
        "or",
        "not",
        "=",
        "<",
        ">",
        "length",
        "str-length",
        "char-at",
        "mod",
        "abs",
        "floor",
        "ceil",
        "sqrt",
        "ln",
        "log",
        "e^",
        "ten^",
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "pressing-key",
        "to-num",
    ]);
}
