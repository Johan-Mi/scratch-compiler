use crate::{
    ir::expr::Expr,
    rewrite::{Clean, Rewrite},
};

pub fn optimize_expr(expr: Expr) -> Rewrite<Expr> {
    // TODO
    Clean(expr)
}
