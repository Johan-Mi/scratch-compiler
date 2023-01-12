use crate::ir::expr::Expr::{self, *};
use sb3_stuff::Value;
use std::mem;

pub fn optimize_expr(expr: &mut Expr) -> bool {
    let mut dirty = false;
    while {
        let mut this_step_dirty = false;
        expr.traverse_postorder_mut(&mut |e| {
            for f in EXPR_OPTIMIZATIONS {
                this_step_dirty |= f(e);
            }
        });
        this_step_dirty
    } {
        dirty = true;
    }
    dirty
}

const EXPR_OPTIMIZATIONS: &[fn(&mut Expr) -> bool] = &[
    const_add_sub,
    const_mul_div,
    add_sub_zero,
    mul_zero,
    mul_div_one,
    trigonometry,
    flatten_add_sub,
    flatten_mul_div,
    mul_div_negation,
    distribute_mul_into_sum,
    redundant_to_num,
    const_mathops,
    empty_call,
];

/// Constant folding for addition and subtraction.
fn const_add_sub(expr: &mut Expr) -> bool {
    if let AddSub(positives, negatives) = expr
      && positives.iter().chain(&*negatives).filter(|term| term.is_lit()).take(2).count() == 2
    {
        let positive_sum: f64 = drain_lits(positives).map(|term| term.to_num()).sum();
        let negative_sum: f64 = drain_lits(negatives).map(|term| term.to_num()).sum();
        let sum = positive_sum - negative_sum;
        positives.push(Lit(Value::Num(sum)));
        true
    } else {
        false
    }
}

/// Constant folding for multiplication and division.
fn const_mul_div(expr: &mut Expr) -> bool {
    if let MulDiv(numerators, denominators) = expr
      && numerators.iter().chain(&*denominators).filter(|term| term.is_lit()).take(2).count() == 2
    {
        let numerator: f64 = drain_lits(numerators).map(|term| term.to_num()).product();
        let denominator: f64 = drain_lits(denominators).map(|term| term.to_num()).product();
        let product = numerator / denominator;
        numerators.push(Lit(Value::Num(product)));
        true
    } else {
        false
    }
}

/// Multiplication by 0.
fn mul_zero(expr: &mut Expr) -> bool {
    if let MulDiv(numerators, _) = expr
      && numerators.iter().any(
             |arg| matches!(arg, Lit(Value::Num(num)) if *num == 0.0),
         )
    {
        *expr = Expr::Lit(Value::Num(0.0));
        true
    } else {
        false
    }
}

/// Multiplication and division by 1.
fn mul_div_one(expr: &mut Expr) -> bool {
    if let MulDiv(numerators, denominators) = expr {
        for terms in &mut [numerators, denominators] {
            if let Some(index) = terms.iter().position(
                |arg| matches!(arg, Lit(Value::Num(num)) if *num == 1.0),
            ) {
                terms.swap_remove(index);
                return true;
            }
        }
    }
    false
}

/// Addition and subtraction with 0.
fn add_sub_zero(expr: &mut Expr) -> bool {
    if let AddSub(positives, negatives) = expr {
        for terms in &mut [positives, negatives] {
            if let Some(index) = terms.iter().position(
                |arg| matches!(arg, Lit(Value::Num(num)) if *num == 0.0),
            ) {
                terms.swap_remove(index);
                return true;
            }
        }
    }
    false
}

/// Trigonometric identities
///
/// - `(sin (- n))` => `(- (sin n))`
/// - `(cos (- n))` => `(cos n)`
fn trigonometry(expr: &mut Expr) -> bool {
    if let FuncCall("sin", span, args) = expr
      && let [AddSub(positives, negatives)] = &mut args[..]
      && positives.is_empty()
      && negatives.len() == 1
    {
        *expr = AddSub(Vec::new(), vec![FuncCall("sin", *span, mem::take(negatives))]);
        true
    } else if let FuncCall("cos", _, args) = expr
      && let [AddSub(positives, negatives)] = &mut args[..]
      && positives.is_empty()
      && negatives.len() == 1
    {
        *args = mem::take(negatives);
        true
    } else {
        false
    }
}

/// Flattens nested addition and subtraction.
fn flatten_add_sub(expr: &mut Expr) -> bool {
    let AddSub(positives, negatives) = expr else { return false; };
    if positives.len() <= 1 && negatives.is_empty() {
        *expr = positives.pop().unwrap_or(Expr::Lit(Value::Num(0.0)));
        true
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
        true
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
        true
    } else {
        false
    }
}

/// Flattens nested multiplication and division.
fn flatten_mul_div(expr: &mut Expr) -> bool {
    let MulDiv(numerators, denominators) = expr else { return false };
    if numerators.len() <= 1 && denominators.is_empty() {
        *expr = numerators.pop().unwrap_or(Expr::Lit(Value::Num(1.0)));
        true
    } else if numerators.iter().any(|term| matches!(term, MulDiv(..))) {
        let (flat_numerators, flat_denominators): (
            Vec<Vec<Expr>>,
            Vec<Vec<Expr>>,
        ) = numerators
            .drain_filter(|term| matches!(term, MulDiv(..)))
            .map(|term| match term {
                MulDiv(flat_numerators, flat_denominators) => {
                    (flat_numerators, flat_denominators)
                }
                _ => unreachable!(),
            })
            .unzip();
        numerators.extend(flat_numerators.into_iter().flatten());
        denominators.extend(flat_denominators.into_iter().flatten());
        true
    } else if denominators.iter().any(|term| matches!(term, MulDiv(..))) {
        let (flat_denominators, flat_numerators): (
            Vec<Vec<Expr>>,
            Vec<Vec<Expr>>,
        ) = denominators
            .drain_filter(|term| matches!(term, MulDiv(..)))
            .map(|term| match term {
                MulDiv(flat_denominators, flat_numerators) => {
                    (flat_denominators, flat_numerators)
                }
                _ => unreachable!(),
            })
            .unzip();
        numerators.extend(flat_numerators.into_iter().flatten());
        denominators.extend(flat_denominators.into_iter().flatten());
        true
    } else {
        false
    }
}

/// Floats negation in a multiplication or division outward.
fn mul_div_negation(expr: &mut Expr) -> bool {
    if let MulDiv(numerators, denominators) = expr
      && [numerators, denominators].into_iter().flatten().any(|factor|
        if let AddSub(positives, negatives) = factor && positives.is_empty() {
            mem::swap(positives, negatives);
            true
        } else {
            false
        }
    )
    {
        *expr = AddSub(Vec::new(), vec![mem::take(expr)]);
        true
    } else {
        false
    }
}

/// Distributes multiplication by a constant into sums containing at least one
/// other constant.
fn distribute_mul_into_sum(expr: &mut Expr) -> bool {
    let contains_a_lit =
        |v: &[Expr]| v.iter().filter(|arg| arg.is_lit()).take(1).count() == 1;

    if let MulDiv(args, _) = expr
      && let Some(sum_index) = args.iter().position(|arg| {
             matches!(arg, AddSub(positives, negatives) if contains_a_lit(positives) && negatives.is_empty())
         })
      && contains_a_lit(args)
    {
        let mut sum = args.swap_remove(sum_index);
        let factor = drain_lits(args).next().unwrap();
        let AddSub(terms, _) = &mut sum else {
            unreachable!();
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
        true
    } else {
        false
    }
}

/// (Sometimes) removes `to-num` if the argument is already a number.
fn redundant_to_num(expr: &mut Expr) -> bool {
    if let FuncCall("to-num", _, ref mut args) = expr
      && let [arg] = &args[..]
      && is_guaranteed_number(arg)
    {
        *expr = args.pop().unwrap();
        true
    } else {
        false
    }
}

/// Constant folding for math operations.
fn const_mathops(expr: &mut Expr) -> bool {
    if let FuncCall(op, _, args) = expr
      && let [Expr::Lit(arg)] = &args[..]
    {
        let n = arg.to_num();
        *expr = 
        Expr::Lit(Value::Num(match *op {
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
            _ => return false,
        }));
        true
    } else {
        false
    }
}

/// Some functions return known constants when applied to zero arguments.
fn empty_call(expr: &mut Expr) -> bool {
    let Expr::FuncCall(func_name, _, args) = expr else { return false; };
    if !args.is_empty() {
        return false;
    }
    *expr = Expr::Lit(match *func_name {
        "++" => Value::String("".into()),
        "and" => Value::Bool(true),
        "or" => Value::Bool(false),
        _ => return false,
    });
    true
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
