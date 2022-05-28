use crate::ir::expr::Expr;
use trexp::{Clean, Rewrite};

pub fn optimize_expr(expr: Expr) -> Rewrite<Expr> {
    // TODO
    Clean(expr)
}
