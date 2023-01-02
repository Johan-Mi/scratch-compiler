use crate::{
    ast::Ast,
    diagnostic::{Error, Result},
    span::Span,
};
use sb3_stuff::Value;
use smol_str::SmolStr;
use std::ops::ControlFlow;

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Value),
    Sym(SmolStr, Span),
    FuncCall(&'static str, Span, Vec<Expr>),
    AddSub(Vec<Expr>, Vec<Expr>),
    MulDiv(Vec<Expr>, Vec<Expr>),
}

impl Default for Expr {
    fn default() -> Self {
        Self::Lit(Value::Num(0.0))
    }
}

impl Expr {
    pub fn from_ast(ast: Ast) -> Result<Self> {
        Ok(match ast {
            Ast::Num(num, ..) => Self::Lit(Value::Num(num)),
            Ast::String(s, ..) => Self::Lit(Value::String(s.into())),
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
                            KNOWN_FUNC_NAMES.get_key(&*func_name).ok_or(
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

    /// Returns `true` if the expr is [`Lit`].
    ///
    /// [`Lit`]: Expr::Lit
    #[must_use]
    pub const fn is_lit(&self) -> bool {
        matches!(self, Self::Lit(..))
    }

    pub fn traverse_postorder_mut<B>(
        &mut self,
        f: &mut impl FnMut(&mut Self) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        match self {
            Self::Lit(_) | Self::Sym(_, _) => {}
            Self::FuncCall(_, _, args) => {
                for expr in args {
                    expr.traverse_postorder_mut(f)?;
                }
            }
            Self::AddSub(a, b) | Self::MulDiv(a, b) => {
                for expr in a.iter_mut().chain(b) {
                    expr.traverse_postorder_mut(f)?;
                }
            }
        }
        f(self)
    }
}

static KNOWN_FUNC_NAMES: phf::Set<&'static str> = phf::phf_set! {
    "*", "/", "!!", "++", "and", "or", "not", "=", "<", ">", "length",
    "str-length", "char-at", "mod", "abs", "floor", "ceil", "sqrt", "ln", "log",
    "e^", "ten^", "sin", "cos", "tan", "asin", "acos", "atan", "pressing-key",
    "to-num", "random",
};
