use crate::{
    ast::Ast,
    diagnostic::{Error, Result},
    span::Span,
};
use sb3_stuff::Value;
use smol_str::SmolStr;
use trexp::{Clean, Rewrite, TreeWalk};

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Value),
    Sym(SmolStr, Span),
    FuncCall(&'static str, Span, Vec<Expr>),
    AddSub(Vec<Expr>, Vec<Expr>),
    MulDiv(Vec<Expr>, Vec<Expr>),
}

impl Expr {
    pub fn from_ast(ast: Ast) -> Result<Self> {
        // TODO: Error handling
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
                        let func_name = KNOWN_FUNC_NAMES
                            .get_key(&*func_name)
                            .ok_or_else(|| {
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
                }
            }
            _ => todo!(),
        })
    }

    /// Returns `true` if the expr is [`Lit`].
    ///
    /// [`Lit`]: Expr::Lit
    #[must_use]
    pub const fn is_lit(&self) -> bool {
        matches!(self, Self::Lit(..))
    }
}

impl TreeWalk<Rewrite<Self>> for Expr {
    fn each_branch(
        self,
        mut f: impl FnMut(Self) -> Rewrite<Self>,
    ) -> Rewrite<Self> {
        match self {
            Self::Lit(_) | Self::Sym(..) => Clean(self),
            Self::FuncCall(func_name, func_span, args) => {
                args.into_iter().map(f).collect::<Rewrite<_>>().map(
                    |new_args| Self::FuncCall(func_name, func_span, new_args),
                )
            }
            Self::AddSub(positives, negatives) => positives
                .into_iter()
                .map(&mut f)
                .collect::<Rewrite<_>>()
                .bind(|positives| {
                    negatives
                        .into_iter()
                        .map(f)
                        .collect::<Rewrite<_>>()
                        .map(|negatives| Self::AddSub(positives, negatives))
                }),
            Self::MulDiv(numerators, denominators) => numerators
                .into_iter()
                .map(&mut f)
                .collect::<Rewrite<_>>()
                .bind(|numerators| {
                    denominators.into_iter().map(f).collect::<Rewrite<_>>().map(
                        |denominators| Self::MulDiv(numerators, denominators),
                    )
                }),
        }
    }
}

static KNOWN_FUNC_NAMES: phf::Set<&'static str> = phf::phf_set! {
    "*", "/", "!!", "++", "and", "or", "not", "=", "<", ">", "length",
    "str-length", "char-at", "mod", "abs", "floor", "ceil", "sqrt", "ln", "log",
    "e^", "ten^", "sin", "cos", "tan", "asin", "acos", "atan", "pressing-key",
    "to-num", "random",
};
