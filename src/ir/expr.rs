use crate::{ast::Ast, span::Span};
use sb3_stuff::Value;
use smol_str::SmolStr;
use trexp::{Clean, Rewrite, TreeWalk};

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Value),
    Sym(SmolStr, Span),
    FuncCall(String, Span, Vec<Expr>),
}

impl Expr {
    pub fn from_ast(ast: Ast) -> Self {
        // TODO: Error handling
        match ast {
            Ast::Num(num, ..) => Self::Lit(Value::Num(num)),
            Ast::String(s, ..) => Self::Lit(Value::String(s.into())),
            Ast::Sym(sym, span) => Self::Sym(sym.into(), span),
            Ast::Node(box Ast::Sym(func_name, span), args, ..) => {
                Self::FuncCall(
                    func_name,
                    span,
                    args.into_iter().map(Self::from_ast).collect(),
                )
            }
            _ => todo!(),
        }
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
