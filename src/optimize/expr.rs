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
    const_times,
    mul_identities,
    add_sub_zero,
    trigonometry,
    flatten_add_sub,
    times_or_divided_negation,
    single_mul,
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

/// Constant folding for `*`
fn const_times(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("*", _, ref mut args) = expr
      && let Some(knowns) = drain_at_least_n_lits(2, args)
    {
        let sum = knowns.map(|lit| lit.to_num()).product();
        args.push(Expr::Lit(Value::Num(sum)));
        Dirty(expr)
    } else {
        Clean(expr)
    }
}

/// Multiplication by 0 or 1.
fn mul_identities(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("*", _, args) = &mut expr {
        if args
            .iter()
            .any(|arg| matches!(arg, Lit(Value::Num(num)) if *num == 0.0))
        {
            Dirty(Lit(Value::Num(0.0)))
        } else if let Some(index) = args
            .iter()
            .position(|arg| matches!(arg, Lit(Value::Num(num)) if *num == 1.0))
        {
            args.swap_remove(index);
            Dirty(expr)
        } else {
            Clean(expr)
        }
    } else {
        Clean(expr)
    }
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
        } else if positives.iter().any(|term| matches!(term, AddSub(..))) {
            let (flat_negatives, flat_positives): (
                Vec<Vec<Expr>>,
                Vec<Vec<Expr>>,
            ) = positives
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

/// Floats negation in a multiplication or division outward.
fn times_or_divided_negation(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("*" | "/", _, args) = &mut expr
      && args.iter_mut().any(|factor|
        if let AddSub(positives, negatives) = factor && positives.is_empty() && negatives.is_empty() {
            *factor = negatives.pop().unwrap();
            true
        } else {
            false
        }
    )
    {
        Dirty(AddSub(Vec::new(), vec![expr]))
    } else {
        Clean(expr)
    }
}

/// Replaces multipliction with a single argument with just that argument.
fn single_mul(mut expr: Expr) -> Rewrite<Expr> {
    if let FuncCall("*", _, args) = &mut expr
      && args.len() == 1
    {
        Dirty(args.pop().unwrap())
    } else {
        Clean(expr)
    }
}

/// Distributes multiplication by a constant into sums containing at least one
/// other constant.
fn distribute_mul_into_sum(mut expr: Expr) -> Rewrite<Expr> {
    let contains_a_lit =
        |v: &[Expr]| v.iter().filter(|arg| arg.is_lit()).take(1).count() == 1;

    if let FuncCall("*", span, ref mut args) = expr
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
                FuncCall("*", span, vec![
                    Expr::Lit(factor.clone()), Expr::Lit(known_term),
                ]),
                FuncCall("*", span, vec![Expr::Lit(factor), sum]),
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

fn drain_at_least_n_lits(
    n: usize,
    exprs: &'_ mut Vec<Expr>,
) -> Option<impl Iterator<Item = Value> + '_> {
    let found_enough_lits =
        exprs.iter().filter(|expr| expr.is_lit()).take(n).count() == n;
    found_enough_lits.then(|| drain_lits(exprs))
}
