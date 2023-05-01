use crate::{
    ir::{
        expr::Expr::{self, Imm},
        statement::Statement::{self, *},
    },
    optimize::expr::optimize_expr,
};
use std::mem;

pub fn optimize_stmt(stmt: &mut Statement) {
    while {
        let mut this_step_dirty = false;
        stmt.traverse_postorder_mut(&mut |s| {
            for f in STMT_OPTIMIZATIONS {
                this_step_dirty |= f(s);
            }
        });
        this_step_dirty
    } {}
}

const STMT_OPTIMIZATIONS: &[fn(&mut Statement) -> bool] = &[
    optimize_stmt_exprs,
    flatten_do,
    const_conditions,
    nested_ifs,
];

/// Optimizes all expressions contained in a statement.
fn optimize_stmt_exprs(stmt: &mut Statement) -> bool {
    match stmt {
        Do(_) | Forever(_) => false,
        ProcCall { args, .. } => args.iter_mut().any(optimize_expr),
        IfElse {
            condition: expr, ..
        }
        | Repeat { times: expr, .. }
        | Until {
            condition: expr, ..
        }
        | While {
            condition: expr, ..
        }
        | For { times: expr, .. } => optimize_expr(expr),
    }
}

/// Flattens nested `do` blocks.
fn flatten_do(stmt: &mut Statement) -> bool {
    match stmt {
        Do(ref mut stmts) if stmts.len() == 1 => {
            *stmt = stmts.pop().unwrap();
            true
        }
        Do(stmts) if stmts.iter().any(|stmt| matches!(stmt, Do(_))) => {
            *stmts = mem::take(stmts)
                .into_iter()
                .flat_map(|stmt| match stmt {
                    Do(nested) => nested,
                    _ => vec![stmt],
                })
                .collect();
            true
        }
        _ => false,
    }
}

/// Removes constant conditions from if statements and loops.
fn const_conditions(stmt: &mut Statement) -> bool {
    match stmt {
        IfElse {
            condition: Imm(condition),
            if_true,
            if_false,
            ..
        } => {
            *stmt = if condition.to_bool() {
                mem::take(if_true)
            } else {
                mem::take(if_false)
            };
            true
        }
        Until {
            condition: Imm(condition),
            body,
        } => {
            *stmt = if condition.to_bool() {
                Do(Vec::new())
            } else {
                Forever(mem::take(body))
            };
            true
        }
        While {
            condition: Imm(condition),
            body,
        } => {
            *stmt = if condition.to_bool() {
                Forever(mem::take(body))
            } else {
                Do(Vec::new())
            };
            true
        }
        _ => false,
    }
}

/// Turns two nested `if`s into a single `if` with the conjunction of both
/// conditions.
fn nested_ifs(stmt: &mut Statement) -> bool {
    if let Statement::IfElse {
        condition: outer_condition,
        if_true:
            box Statement::IfElse {
                condition: inner_condition,
                if_true: inner_if_true,
                if_false: inner_if_false,
                ..
            },
        if_false: outer_if_false,
        span,
    } = stmt
      && outer_if_false.is_nop()
      && inner_if_false.is_nop()
    {
        *stmt = Statement::IfElse {
            condition: Expr::FuncCall(
                "and",
                *span,
                vec![mem::take(outer_condition), mem::take(inner_condition)],
            ),
            if_true: mem::take(inner_if_true),
            if_false: Box::default(),
            span: *span,
        };
        true
    } else {
        false
    }
}
