pub enum Typ<'a> {
    Double,
    Bool,
    StaticStr(StrKnowledge<'a>),
    OwnedString,
    Any,
}

pub enum StrKnowledge<'a> {
    Exact(&'a str),
}
