use crate::{
    ir::{
        expr::Expr::Lit,
        statement::Statement::{self, *},
    },
    optimize::expr::optimize_expr,
};
use trexp::{Bind, Clean, Dirty, Rewrite, TreeWalk};

pub fn optimize_stmt(stmt: Statement) -> Rewrite<Statement> {
    Rewrite::repeat(stmt, |s| {
        s.bottom_up(|s| {
            STMT_OPTIMIZATIONS.iter().fold(Clean(s), Bind::bind_mut)
        })
    })
}

const STMT_OPTIMIZATIONS: &[fn(Statement) -> Rewrite<Statement>] =
    &[optimize_stmt_exprs, flatten_do, const_conditions];

/// Optimizes all expressions contained in a statement.
fn optimize_stmt_exprs(stmt: Statement) -> Rewrite<Statement> {
    match stmt {
        Do(_) | Forever(_) => Clean(stmt),
        ProcCall { proc_name, args } => args
            .into_iter()
            .map(optimize_expr)
            .collect::<Rewrite<_>>()
            .map(|args| ProcCall { proc_name, args }),
        IfElse {
            condition,
            if_true,
            if_false,
        } => optimize_expr(condition).map(|condition| IfElse {
            condition,
            if_true,
            if_false,
        }),
        Repeat { times, body } => {
            optimize_expr(times).map(|times| Repeat { times, body })
        }
        Until { condition, body } => {
            optimize_expr(condition).map(|condition| Until { condition, body })
        }
        While { condition, body } => {
            optimize_expr(condition).map(|condition| While { condition, body })
        }
        For {
            counter,
            times,
            body,
        } => optimize_expr(times).map(|times| For {
            counter,
            times,
            body,
        }),
    }
}

/// Flattens nested `do` blocks.
fn flatten_do(stmt: Statement) -> Rewrite<Statement> {
    match stmt {
        Do(mut stmts) if stmts.len() == 1 => Dirty(stmts.pop().unwrap()),
        Do(stmts) if stmts.iter().any(|stmt| matches!(stmt, Do(_))) => {
            Dirty(Do(stmts
                .into_iter()
                .flat_map(|stmt| match stmt {
                    Do(nested) => nested,
                    _ => vec![stmt],
                })
                .collect()))
        }
        _ => Clean(stmt),
    }
}

/// Removes constant conditions from if statements and loops.
fn const_conditions(stmt: Statement) -> Rewrite<Statement> {
    match stmt {
        IfElse {
            condition: Lit(condition),
            if_true,
            if_false,
        } => Dirty(if condition.to_bool() {
            *if_true
        } else {
            *if_false
        }),
        Until {
            condition: Lit(condition),
            body,
        } => Dirty(if condition.to_bool() {
            Do(Vec::new())
        } else {
            Forever(body)
        }),
        While {
            condition: Lit(condition),
            body,
        } => Dirty(if condition.to_bool() {
            Forever(body)
        } else {
            Do(Vec::new())
        }),
        _ => Clean(stmt),
    }
}
