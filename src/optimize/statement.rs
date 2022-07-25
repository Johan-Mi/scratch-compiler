use crate::{
    ir::proc::Statement::{self, *},
    optimize::expr::optimize_expr,
};
use trexp::{Bind, Clean, Rewrite, TreeWalk};

pub fn optimize_stmt(stmt: Statement) -> Rewrite<Statement> {
    Rewrite::repeat(stmt, |s| {
        s.bottom_up(|s| {
            STMT_OPTIMIZATIONS.iter().fold(Clean(s), Bind::bind_mut)
        })
    })
}

const STMT_OPTIMIZATIONS: &[fn(Statement) -> Rewrite<Statement>] =
    &[optimize_stmt_exprs];

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
