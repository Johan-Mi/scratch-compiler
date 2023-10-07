use crate::ir::expr::Expr;
use cranelift::prelude::Value;
use sb3_stuff::Value as Immediate;

pub enum Typ<'a> {
    Double,
    Bool,
    StaticStr(&'a str),
    OwnedString,
    Any,
}

pub fn expr_type(expr: &Expr) -> Typ {
    match expr {
        Expr::Imm(Immediate::String(s)) => Typ::StaticStr(s),
        Expr::Imm(Immediate::Bool(_)) => Typ::Bool,
        Expr::Imm(Immediate::Num(_)) | Expr::AddSub(..) | Expr::MulDiv(..) => {
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

pub enum MixedSizeValue {
    Single(Value),
    Pair([Value; 2]),
}

impl From<Value> for MixedSizeValue {
    fn from(value: Value) -> Self {
        Self::Single(value)
    }
}

impl From<(Value, Value)> for MixedSizeValue {
    fn from(values: (Value, Value)) -> Self {
        Self::Pair(values.into())
    }
}

impl MixedSizeValue {
    pub const fn single(self) -> Value {
        match self {
            Self::Single(v) => v,
            Self::Pair(_) => panic!(),
        }
    }

    pub const fn pair(self) -> (Value, Value) {
        match self {
            Self::Single(_) => panic!(),
            Self::Pair([v0, v1]) => (v0, v1),
        }
    }

    pub const fn as_slice(&self) -> &[Value] {
        match self {
            Self::Single(v) => std::slice::from_ref(v),
            Self::Pair(vs) => vs,
        }
    }
}
