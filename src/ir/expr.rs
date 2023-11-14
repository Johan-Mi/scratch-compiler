use crate::{
    ast::Ast,
    diagnostic::{Error, Result},
};
use codemap::Span;
use ecow::EcoString;
use sb3_stuff::Value;

#[derive(Debug, Clone)]
pub enum Expr {
    Imm(Value),
    Sym(EcoString, Span),
    FuncCall(&'static str, Span, Vec<Self>),
    AddSub(Vec<Self>, Vec<Self>),
    MulDiv(Vec<Self>, Vec<Self>),
}

impl Default for Expr {
    fn default() -> Self {
        Self::Imm(Value::Num(0.0))
    }
}

macro_rules! known_func_name {
    ($s:expr, $($lit:literal),* $(,)?) => {
        match $s {
            $($lit => Some($lit),)*
            _ => None,
        }
    }
}

impl Expr {
    pub fn from_ast(ast: Ast) -> Result<Self> {
        Ok(match ast {
            Ast::Num(num, ..) => Self::Imm(Value::Num(num)),
            Ast::Bool(b, ..) => Self::Imm(Value::Bool(b)),
            Ast::String(s, ..) => Self::Imm(Value::String(s.into())),
            Ast::Sym(sym, span) => Self::Sym(sym.into(), span),
            Ast::Node(box Ast::Sym(func_name, span), args, ..) => {
                match &*func_name {
                    "+" => {
                        let positives = args
                            .into_iter()
                            .map(Self::from_ast)
                            .collect::<Result<_>>()?;
                        Self::AddSub(positives, Vec::new())
                    }
                    "-" => {
                        let mut terms = args.into_iter().map(Self::from_ast);
                        let positive_or_negated = terms.next().unwrap()?;
                        let terms = terms.collect::<Result<Vec<_>>>()?;
                        if terms.is_empty() {
                            Self::AddSub(Vec::new(), vec![positive_or_negated])
                        } else {
                            Self::AddSub(vec![positive_or_negated], terms)
                        }
                    }
                    "*" => {
                        let numerators = args
                            .into_iter()
                            .map(Self::from_ast)
                            .collect::<Result<_>>()?;
                        Self::MulDiv(numerators, Vec::new())
                    }
                    "/" => {
                        let mut terms = args.into_iter().map(Self::from_ast);
                        let numerator_or_inverted = terms.next().unwrap()?;
                        let terms = terms.collect::<Result<Vec<_>>>()?;
                        if terms.is_empty() {
                            Self::MulDiv(
                                Vec::new(),
                                vec![numerator_or_inverted],
                            )
                        } else {
                            Self::MulDiv(vec![numerator_or_inverted], terms)
                        }
                    }
                    _ => {
                        let func_name =
                            known_func_name! { &*func_name,
                                "*", "/", "!!", "++", "and", "or", "not", "=", "<", ">", "length",
                                "str-length", "char-at", "mod", "abs", "floor", "ceil", "sqrt", "ln", "log",
                                "e^", "ten^", "sin", "cos", "tan", "asin", "acos", "atan", "pressing-key",
                                "to-num", "random",
                            }.ok_or(
                                Error::UnknownFunction { span, func_name },
                            )?;
                        Self::FuncCall(
                            func_name,
                            span,
                            args.into_iter()
                                .map(Self::from_ast)
                                .collect::<Result<_>>()?,
                        )
                    }
                }
            }
            Ast::Node(not_a_symbol, ..) => {
                return Err(Box::new(Error::FunctionNameMustBeSymbol {
                    span: not_a_symbol.span(),
                }))
            }
            Ast::Unquote(_, span) => {
                return Err(Box::new(Error::UnquoteOutsideOfMacro { span }))
            }
        })
    }

    /// Returns `true` if the expr is [`Imm`].
    ///
    /// [`Imm`]: Expr::Imm
    #[must_use]
    pub const fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(..))
    }

    pub fn traverse_postorder_mut(&mut self, f: &mut impl FnMut(&mut Self)) {
        match self {
            Self::Imm(_) | Self::Sym(_, _) => {}
            Self::FuncCall(_, _, args) => {
                for expr in args {
                    expr.traverse_postorder_mut(f);
                }
            }
            Self::AddSub(a, b) | Self::MulDiv(a, b) => {
                for expr in a.iter_mut().chain(b) {
                    expr.traverse_postorder_mut(f);
                }
            }
        }
        f(self);
    }
}
