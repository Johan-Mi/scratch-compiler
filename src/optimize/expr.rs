use crate::ir::expr::Expr::{self, *};
use sb3_stuff::Value;
use std::mem;
use trexp::{Bind, Clean, Dirty, Rewrite, TreeWalk};

pub fn optimize_expr(expr: Expr) -> Rewrite<Expr> {
    Rewrite::repeat(expr, |e| {
        e.bottom_up(|e| {
            EXPR_OPTIMIZATIONS.iter().fold(Clean(e), Bind::bind_mut)
        })
    })
}

const EXPR_OPTIMIZATIONS: &[fn(Expr) -> Rewrite<Expr>] = &[
    const_add_sub,
    const_mul_div,
    add_sub_zero,
    mul_div_one,
    trigonometry,
    flatten_add_sub,
    flatten_mul_div,
    distribute_mul_into_sum,
    redundant_to_num,
    const_mathops,
];

/// Constant folding for addition and subtraction.
fn const_add_sub(mut expr: Expr) -> Rewrite<Expr> {
    if let AddSub(positives, negatives) = &mut expr
      && positives.iter().chain(&*negatives).filter(|term| term.is_lit()).take(2).count() == 2
    {
        let positive_sum: f64 = drain_lits(positives).map(|term| term.to_num()).sum();
        let negative_sum: f64 = drain_lits(negatives).map(|term| term.to_num()).sum();
        let sum = positive_sum - negative_sum;
        positives.push(Lit(Value::Num(sum)));
        Dirty(expr)
    } else {
        Clean(expr)
    }
}

/// Constant folding for multiplication and division.
fn const_mul_div(mut expr: Expr) -> Rewrite<Expr> {
    if let MulDiv(numerators, denominators) = &mut expr
      && numerators.iter().chain(&*denominators).filter(|term| term.is_lit()).take(2).count() == 2
    {
        let numerator: f64 = drain_lits(numerators).map(|term| term.to_num()).sum();
        let denominator: f64 = drain_lits(denominators).map(|term| term.to_num()).sum();
        let product = numerator / denominator;
        numerators.push(Lit(Value::Num(product)));
        Dirty(expr)
    } else {
        Clean(expr)
    }
}

/// Multiplication and division by 1.
fn mul_div_one(mut expr: Expr) -> Rewrite<Expr> {
    if let MulDiv(numerators, denominators) = &mut expr {
        for terms in &mut [numerators, denominators] {
            if let Some(index) = terms.iter().position(
                |arg| matches!(arg, Lit(Value::Num(num)) if *num == 1.0),
            ) {
                terms.swap_remove(index);
                return Dirty(expr);
            }
        }
    }
    Clean(expr)
}

/// Addition and subtraction with 0.
fn add_sub_zero(mut expr: Expr) -> Rewrite<Expr> {
    if let AddSub(positives, negatives) = &mut expr {
        for terms in &mut [positives, negatives] {
            if let Some(index) = terms.iter().position(
                |arg| matches!(arg, Lit(Value::Num(num)) if *num == 0.0),
            ) {
                terms.swap_remove(index);
                return Dirty(expr);
            }
        }
    }
    Clean(expr)
}

/// Trigonometric identities
///
/// - `(sin (- n))` => `(- (sin n))`
/// - `(cos (- n))` => `(cos n)`
fn trigonometry(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("sin", span, ref mut args) = expr
      && let [AddSub(positives, negatives)] = &mut args[..]
      && positives.is_empty()
      && negatives.len() == 1
    {
        Dirty(AddSub(Vec::new(), vec![FuncCall("sin", span, mem::take(negatives))]))
    } else if let FuncCall("cos", _, args) = &mut expr
      && let [AddSub(positives, negatives)] = &mut args[..]
      && positives.is_empty()
      && negatives.len() == 1
    {
        *args = mem::take(negatives);
        Dirty(expr)
    } else {
        Clean(expr)
    }
}

/// Flattens nested addition and subtraction.
fn flatten_add_sub(mut expr: Expr) -> Rewrite<Expr> {
    if let AddSub(positives, negatives) = &mut expr {
        if positives.len() == 1 && negatives.is_empty() {
            Dirty(positives.pop().unwrap())
        } else if positives.iter().any(|term| matches!(term, AddSub(..))) {
            let (flat_positives, flat_negatives): (
                Vec<Vec<Expr>>,
                Vec<Vec<Expr>>,
            ) = positives
                .drain_filter(|term| matches!(term, AddSub(..)))
                .map(|term| match term {
                    AddSub(flat_positives, flat_negatives) => {
                        (flat_positives, flat_negatives)
                    }
                    _ => unreachable!(),
                })
                .unzip();
            positives.extend(flat_positives.into_iter().flatten());
            negatives.extend(flat_negatives.into_iter().flatten());
            Dirty(expr)
        } else if negatives.iter().any(|term| matches!(term, AddSub(..))) {
            let (flat_negatives, flat_positives): (
                Vec<Vec<Expr>>,
                Vec<Vec<Expr>>,
            ) = negatives
                .drain_filter(|term| matches!(term, AddSub(..)))
                .map(|term| match term {
                    AddSub(flat_negatives, flat_positives) => {
                        (flat_negatives, flat_positives)
                    }
                    _ => unreachable!(),
                })
                .unzip();
            positives.extend(flat_positives.into_iter().flatten());
            negatives.extend(flat_negatives.into_iter().flatten());
            Dirty(expr)
        } else {
            Clean(expr)
        }
    } else {
        Clean(expr)
    }
}

/// Flattens nested multiplication and division.
fn flatten_mul_div(mut expr: Expr) -> Rewrite<Expr> {
    if let MulDiv(numerators, denominators) = &mut expr {
        if numerators.len() == 1 && denominators.is_empty() {
            Dirty(numerators.pop().unwrap())
        } else if numerators.iter().any(|term| matches!(term, MulDiv(..))) {
            let (flat_numerators, flat_denominators): (
                Vec<Vec<Expr>>,
                Vec<Vec<Expr>>,
            ) = numerators
                .drain_filter(|term| matches!(term, MulDiv(..)))
                .map(|term| match term {
                    MulDiv(flat_positives, flat_negatives) => {
                        (flat_positives, flat_negatives)
                    }
                    _ => unreachable!(),
                })
                .unzip();
            numerators.extend(flat_numerators.into_iter().flatten());
            denominators.extend(flat_denominators.into_iter().flatten());
            Dirty(expr)
        } else if denominators.iter().any(|term| matches!(term, MulDiv(..))) {
            let (flat_denominators, flat_numerators): (
                Vec<Vec<Expr>>,
                Vec<Vec<Expr>>,
            ) = denominators
                .drain_filter(|term| matches!(term, MulDiv(..)))
                .map(|term| match term {
                    MulDiv(flat_negatives, flat_positives) => {
                        (flat_negatives, flat_positives)
                    }
                    _ => unreachable!(),
                })
                .unzip();
            numerators.extend(flat_numerators.into_iter().flatten());
            denominators.extend(flat_denominators.into_iter().flatten());
            Dirty(expr)
        } else {
            Clean(expr)
        }
    } else {
        Clean(expr)
    }
}

/// Distributes multiplication by a constant into sums containing at least one
/// other constant.
fn distribute_mul_into_sum(mut expr: Expr) -> Rewrite<Expr> {
    let contains_a_lit =
        |v: &[Expr]| v.iter().filter(|arg| arg.is_lit()).take(1).count() == 1;

    if let MulDiv(args, _) = &mut expr
      && let Some(sum_index) = args.iter().position(|arg| {
             matches!(arg, AddSub(positives, negatives) if contains_a_lit(positives) && negatives.is_empty())
         })
      && contains_a_lit(args)
    {
        let mut sum = args.swap_remove(sum_index);
        let factor = drain_lits(args).next().unwrap();
        let terms = match &mut sum {
            AddSub(positives, _) => positives,
            _ => unreachable!(),
        };
        let known_term = drain_lits(terms).next().unwrap();
        args.push(
            AddSub(vec![
                MulDiv(vec![
                    Expr::Lit(factor.clone()), Expr::Lit(known_term),
                ], Vec::new()),
                MulDiv(vec![Expr::Lit(factor), sum], Vec::new()),
            ], Vec::new()),
        );
        Dirty(expr)
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

/// Constant folding for math operations.
fn const_mathops(expr: Expr) -> Rewrite<Expr> {
    if let FuncCall(op, _, args) = &expr
      && let [Expr::Lit(arg)] = &args[..]
    {
        let n = arg.to_num();
        Dirty(Expr::Lit(Value::Num(match *op {
            "abs" => n.abs(),
            "floor" => n.floor(),
            "ceil" => n.ceil(),
            "sqrt" => n.sqrt(),
            "ln" => n.ln(),
            "log" => n.log10(),
            "e^" => n.exp(),
            "ten^" => 10.0f64.powf(n),
            "sin" => n.to_radians().sin(),
            "cos" => n.to_radians().cos(),
            "tan" => n.to_radians().tan(),
            "asin" => n.asin().to_degrees(),
            "acos" => n.acos().to_degrees(),
            "atan" => n.atan().to_degrees(),
            _ => return Clean(expr),
        })))
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

fn drain_lits(exprs: &mut Vec<Expr>) -> impl Iterator<Item = Value> + '_ {
    exprs
        .drain_filter(|expr| expr.is_lit())
        .map(|expr| match expr {
            Lit(lit) => lit,
            _ => unreachable!(),
        })
}
