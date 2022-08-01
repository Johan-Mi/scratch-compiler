use crate::span::Span;

pub type Result<T> = std::result::Result<T, Box<Error>>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(
        "{span}: function macro `{macro_name}` expected {expected} arguments\
        but got {got}"
    )]
    FunctionMacroWrongArgCount {
        span: Span,
        macro_name: String,
        expected: usize,
        got: usize,
    },
    #[error("{span}: invalid arguments for `include`")]
    InvalidArgsForInclude { span: Span },
    #[error("{span}: invalid macro signature")]
    InvalidMacroSignature { span: Span },
    #[error("{span}: macro definition is missing a body")]
    MacroDefinitionMissingBody { span: Span },
    #[error("{span}: macro definition is missing a signature")]
    MacroDefinitionMissingSignature { span: Span },
    #[error("{span}: unknown metavariable: `{var_name}`")]
    UnknownMetavariable { span: Span, var_name: String },
}
