use crate::{
    ir::{
        expr::Expr::Lit,
        statement::Statement::{self, *},
    },
    optimize::expr::optimize_expr,
};
use std::mem;

pub fn optimize_stmt(stmt: &mut Statement) -> bool {
    let mut dirty = false;
    while {
        let mut this_step_dirty = false;
        stmt.traverse_postorder_mut(&mut |s| {
            for f in STMT_OPTIMIZATIONS {
                this_step_dirty |= f(s);
            }
        });
        this_step_dirty
    } {
        dirty = true;
    }
    dirty
}

const STMT_OPTIMIZATIONS: &[fn(&mut Statement) -> bool] =
    &[optimize_stmt_exprs, flatten_do, const_conditions];

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
            condition: Lit(condition),
            if_true,
            if_false,
        } => {
            *stmt = if condition.to_bool() {
                mem::take(if_true)
            } else {
                mem::take(if_false)
            };
            true
        }
        Until {
            condition: Lit(condition),
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
            condition: Lit(condition),
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
