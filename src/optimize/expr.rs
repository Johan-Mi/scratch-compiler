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
    const_plus,
    const_minus,
    mul_identities,
    add_identity,
    trigonometry,
    zero_minus,
    double_minus,
    times_negation,
    add_subtraction,
    add_negation,
    redundant_to_num,
];

/// Constant folding for `+`
fn const_plus(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("+", _, ref mut args) = expr
      && let Some(knowns) = drain_at_least_n_lits(2, args)
    {
        let sum = knowns.map(|lit| lit.to_num()).sum();
        args.push(Expr::Lit(Value::Num(sum)));
        Dirty(expr)
    } else {
        Clean(expr)
    }
}

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
fn add_identity(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("+", _, args) = &mut expr
      && let Some(index) = args.iter().position(|arg| {
             matches!(arg, Lit(Value::Num(num)) if *num == 0.0)
         })
    {
        args.swap_remove(index);
        Dirty(expr)
    } else {
        Clean(expr)
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

/// Subtracting something from zero is equivalent to negation.
fn zero_minus(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("-", _, args) = &mut expr
      && matches!(&args[..], [Lit(Value::Num(num)), _] if *num == 0.0)
    {
        args.swap_remove(0);
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

/// Floats negation in a multiplication outward.
fn times_negation(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("*", span, args) = &mut expr
      && args.iter_mut().any(|e|
        if let FuncCall("-", _, args) = e && args.len() == 1 {
            *e = args.pop().unwrap();
            true
        } else {
            false
        }
    )
    {
        Dirty(FuncCall("-", *span, vec![expr]))
    } else {
        Clean(expr)
    }
}

/// Floats subtraction in addition outward.
fn add_subtraction(mut expr: Expr) -> Rewrite<Expr> {
    let mut subtracted = Vec::new();
    if let FuncCall("+", span, ref mut terms) = expr {
        for term in terms {
            if let FuncCall("-", _, args) = term
              && args.len() >= 2 {
                let positive = args.swap_remove(0);
                subtracted.append(args);
                *term = positive;
            }
        }
        if subtracted.is_empty() {
            Clean(expr)
        } else {
            subtracted.insert(0, expr);
            Dirty(FuncCall("-", span, subtracted))
        }
    } else {
        Clean(expr)
    }
}

/// Floats negation in addition outward.
fn add_negation(mut expr: Expr) -> Rewrite<Expr> {
    let mut subtracted = Vec::new();
    if let FuncCall("+", span, ref mut terms) = expr {
        let mut i = 0;
        while i < terms.len() {
            if let FuncCall("-", _, args) = &mut terms[i]
              && args.len() == 1 {
                subtracted.push(args.pop().unwrap());
                terms.swap_remove(i);
            } else {
                i += 1;
            }
        }
        if subtracted.is_empty() {
            Clean(expr)
        } else {
            subtracted.insert(0, expr);
            Dirty(FuncCall("-", span, subtracted))
        }
    } else {
        Clean(expr)
    }
}

/// (Sometimes) removes `to-num` if the argument is already a number.
fn redundant_to_num(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("to-num", _, ref mut args) = expr
      && let [arg] = &args[..]
      && is_guaranteed_number(arg)
    {
        Dirty(args.pop().unwrap())
    } else {
        Clean(expr)
    }
}

fn is_guaranteed_number(expr: &Expr) -> bool {
    matches!(
        expr,
        FuncCall(
            "length"
                | "str-length"
                | "mod"
                | "abs"
                | "floor"
                | "ceil"
                | "sqrt"
                | "ln"
                | "log"
                | "e^"
                | "ten^"
                | "sin"
                | "cos"
                | "tan"
                | "asin"
                | "acos"
                | "atan"
                | "to-num",
            _,
            _
        )
    )
}

fn drain_at_least_n_lits(
    n: usize,
    exprs: &'_ mut Vec<Expr>,
) -> Option<impl Iterator<Item = Value> + '_> {
    let mut found_lits = 0;
    for expr in &*exprs {
        if expr.is_lit() {
            found_lits += 1;
            if found_lits == n {
                break;
            }
        }
    }
    if found_lits < n {
        None
    } else {
        Some(
            exprs
                .drain_filter(|expr| expr.is_lit())
                .map(|expr| match expr {
                    Lit(lit) => lit,
                    _ => unreachable!(),
                }),
        )
    }
}
