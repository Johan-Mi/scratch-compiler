use crate::ir::expr::Expr;
use sb3_stuff::Value;

pub enum Typ<'a> {
    Double,
    Bool,
    StaticStr(&'a str),
    OwnedString,
    Any,
}

pub fn expr_type(expr: &Expr) -> Typ {
    match expr {
        Expr::Lit(Value::String(s)) => Typ::StaticStr(s),
        Expr::Lit(Value::Bool(_)) => Typ::Bool,
        Expr::Lit(Value::Num(_)) | Expr::AddSub(..) | Expr::MulDiv(..) => {
            Typ::Double
        }
        Expr::Sym(..) => {
            // NOTE: This will need to be changed when we add support for converting
            // lists to strings or start perform static type analysis on variables.
            Typ::Any
        }
        Expr::FuncCall(func_name, _, _args) => match *func_name {
            "!!" => Typ::Any,
            "not" | "and" | "or" | "<" | "=" | ">" => Typ::Bool,
            "++" | "char-at" => Typ::OwnedString,
            "length" | "str-length" | "mod" | "abs" | "floor" | "ceil"
            | "sqrt" | "ln" | "log" | "e^" | "ten^" | "sin" | "cos" | "tan"
            | "asin" | "acos" | "atan" | "to-num" | "random" => Typ::Double,
            _ => todo!(),
        },
    }
}
