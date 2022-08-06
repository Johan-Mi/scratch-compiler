use crate::ir::expr::Expr::{self, *};
use fancy_match::fancy_match;
use sb3_stuff::Value;
use trexp::{Bind, Clean, Rewrite, TreeWalk};
use Rewrite::Dirty;

pub fn optimize_expr(expr: Expr) -> Rewrite<Expr> {
    Rewrite::repeat(expr, |e| {
        e.bottom_up(|e| {
            EXPR_OPTIMIZATIONS.iter().fold(Clean(e), Bind::bind_mut)
        })
    })
}

const EXPR_OPTIMIZATIONS: &[fn(Expr) -> Rewrite<Expr>] =
    &[const_minus, mul_identities, add_identity];

/// Constant folding for `-`.
fn const_minus(expr: Expr) -> Rewrite<Expr> {
    #[fancy_match]
    match &expr {
        FuncCall("-", _, args) => match &args[..] {
            [Lit(negation)] => Dirty(Lit(Value::Num(-negation.to_num()))),
            [Lit(lhs), rest @ ..] => {
                match rest
                    .iter()
                    .map(|expr| expr.as_lit().map(Value::to_num))
                    .sum::<Option<f64>>()
                {
                    Some(sum) => Dirty(Lit(Value::Num(lhs.to_num() - sum))),
                    _ => Clean(expr),
                }
            }
            _ => Clean(expr),
        },
        _ => Clean(expr),
    }
}

/// Multiplication by 0 or 1.
fn mul_identities(expr: Expr) -> Rewrite<Expr> {
    #[fancy_match]
    match &expr {
        FuncCall("*", span, args) => match &args[..] {
            [Lit(Value::Num(num)), ..] if *num == 0.0 => {
                Dirty(Lit(Value::Num(0.0)))
            }
            [Lit(Value::Num(num)), rest @ ..] if *num == 1.0 => {
                Dirty(FuncCall("*".to_owned(), *span, rest.to_vec()))
            }
            _ => Clean(expr),
        },
        _ => Clean(expr),
    }
}

/// Addition with 0.
fn add_identity(expr: Expr) -> Rewrite<Expr> {
    #[fancy_match]
    match &expr {
        FuncCall("+", span, args) => match &args[..] {
            [Lit(Value::Num(num)), rest @ ..] if *num == 0.0 => {
                Dirty(FuncCall("+".to_owned(), *span, rest.to_vec()))
            }
            _ => Clean(expr),
        },
        _ => Clean(expr),
    }
}
