use crate::{
    ir::expr::Expr,
    rewrite::{Clean, Rewrite},
};

pub(crate) fn optimize_expr(expr: Expr) -> Rewrite<Expr> {
    // TODO
    Clean(expr)
}
