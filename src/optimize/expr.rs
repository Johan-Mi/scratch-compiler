use crate::ir::expr::Expr::{self, *};
use sb3_stuff::Value;
use std::mem;
use trexp::{Bind, Clean, Rewrite, TreeWalk};
use Rewrite::Dirty;

pub fn optimize_expr(expr: Expr) -> Rewrite<Expr> {
    Rewrite::repeat(expr, |e| {
        e.bottom_up(|e| {
            EXPR_OPTIMIZATIONS.iter().fold(Clean(e), Bind::bind_mut)
        })
    })
}

const EXPR_OPTIMIZATIONS: &[fn(Expr) -> Rewrite<Expr>] = &[
    const_minus,
    mul_identities,
    add_identity,
    trigonometry,
    double_minus,
];

/// Constant folding for `-`.
fn const_minus(expr: Expr) -> Rewrite<Expr> {
    match expr {
        FuncCall("-", _, ref args) => match &args[..] {
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
    match expr {
        FuncCall("*", span, ref args) => match &args[..] {
            [Lit(Value::Num(num)), ..] if *num == 0.0 => {
                Dirty(Lit(Value::Num(0.0)))
            }
            [Lit(Value::Num(num)), rest @ ..] if *num == 1.0 => {
                Dirty(FuncCall("*", span, rest.to_vec()))
            }
            _ => Clean(expr),
        },
        _ => Clean(expr),
    }
}

/// Addition with 0.
fn add_identity(expr: Expr) -> Rewrite<Expr> {
    match expr {
        FuncCall("+", span, ref args) => match &args[..] {
            [Lit(Value::Num(num)), rest @ ..] if *num == 0.0 => {
                Dirty(FuncCall("+", span, rest.to_vec()))
            }
            _ => Clean(expr),
        },
        _ => Clean(expr),
    }
}

/// Trigonometric identities
///
/// - `(sin (- n))` => `(- (sin n))`
/// - `(cos (- n))` => `(cos n)`
fn trigonometry(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall(sym_sin @ "sin", _, args) = &mut expr
      && let [FuncCall(sym_minus @ "-", _, args)] = &mut args[..]
      && args.len() == 1
    {
        mem::swap(sym_sin, sym_minus);
        Dirty(expr)
    } else if let FuncCall("cos", _, cos_args) = &mut expr
      && let [FuncCall("-", _, args)] = &mut cos_args[..]
      && args.len() == 1
    {
        *cos_args = mem::take(args);
        Dirty(expr)
    } else {
        Clean(expr)
    }
}

/// Double negation just converts the argument to a number.
fn double_minus(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall(outer_sym @ "-", _, outer_args) = &mut expr
      && let [FuncCall("-", _, args)] = &mut outer_args[..]
      && args.len() == 1
    {
        *outer_sym = "to-num";
        *outer_args = mem::take(args);
        Dirty(expr)
    } else {
        Clean(expr)
    }
}
