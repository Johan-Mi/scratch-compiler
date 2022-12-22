use super::{emit_all, plural, primary, secondary, Diagnostic};
use crate::span::Span;
use smol_str::SmolStr;
use std::io;

#[derive(Debug)]
pub enum Error {
    BuiltinProcWrongArgCount {
        span: Span,
        proc_name: String,
        expected: usize,
        got: usize,
    },
    CouldNotCreateSb3File {
        inner: io::Error,
    },
    CouldNotCreateProjectJson {
        inner: zip::result::ZipError,
    },
    CouldNotFinishZip {
        inner: zip::result::ZipError,
    },
    CustomProcWrongArgCount {
        span: Span,
        proc_name: String,
        expected: usize,
        got: usize,
    },
    FunctionMacroMatchFailed {
        pattern: Span,
        provided: Span,
        macro_name: String,
    },
    FunctionMacroWrongArgCount {
        span: Span,
        macro_name: String,
        expected: usize,
        got: usize,
    },
    FunctionNameMustBeSymbol {
        span: Span,
    },
    FunctionWrongArgCount {
        span: Span,
        func_name: &'static str,
        expected: usize,
        got: usize,
    },
    InvalidArgsForInclude {
        span: Span,
    },
    InvalidItemInSprite {
        span: Span,
    },
    InvalidMacroParameter {
        span: Span,
    },
    InvalidMacroSignature {
        span: Span,
    },
    MacroDefinitionMissingBody {
        span: Span,
    },
    MacroDefinitionMissingSignature {
        span: Span,
    },
    ProgramMissingStage,
    SpriteMissingName {
        span: Span,
        candidate_symbol: Option<Span>,
    },
    SymbolMacroInInlinePosition {
        span: Span,
    },
    SymConcatEmptySymbol {
        span: Span,
    },
    UnknownFunction {
        span: Span,
        func_name: String,
    },
    UnknownList {
        span: Span,
        list_name: SmolStr,
    },
    UnknownMetavariable {
        span: Span,
        var_name: String,
    },
    UnknownProc {
        span: Span,
        proc_name: String,
    },
    UnknownVar {
        span: Span,
        var_name: SmolStr,
    },
    UnknownVarOrList {
        span: Span,
        sym_name: SmolStr,
    },
    UnquoteOutsideOfMacro {
        span: Span,
    },
}

impl Error {
    pub fn emit(&self) {
        use Error::*;
        let diagnostics = match self {
            BuiltinProcWrongArgCount {
                span,
                proc_name,
                expected,
                got,
            } => vec![wrong_arg_count(
                "builtin procedure",
                proc_name,
                *expected,
                *got,
                *span,
            )],
            CouldNotCreateSb3File { inner } => {
                vec![just_message("could not create SB3 file")
                    .with_notes(vec![inner.to_string()])]
            }
            CouldNotCreateProjectJson { inner } => {
                vec![just_message("could not create `project.json`")
                    .with_notes(vec![inner.to_string()])]
            }
            CouldNotFinishZip { inner } => {
                vec![just_message("could not finish zip archive")
                    .with_notes(vec![inner.to_string()])]
            }
            CustomProcWrongArgCount {
                span,
                proc_name,
                expected,
                got,
            } => vec![wrong_arg_count(
                "custom procedure",
                proc_name,
                *expected,
                *got,
                *span,
            )],
            FunctionMacroMatchFailed {
                pattern,
                provided,
                macro_name,
            } => vec![just_message(format!(
                "argument to function macro `{macro_name}` does not match the \
                pattern in its definition"
            ))
            .with_labels(vec![
                primary(*provided).with_message("argument provided here"),
                secondary(*pattern).with_message("pattern was defined here"),
            ])],
            FunctionMacroWrongArgCount {
                span,
                macro_name,
                expected,
                got,
            } => vec![wrong_arg_count(
                "function macro",
                macro_name,
                *expected,
                *got,
                *span,
            )],
            FunctionNameMustBeSymbol { span } => {
                vec![with_span("function name must be a symbol", *span)]
            }
            FunctionWrongArgCount {
                span,
                func_name,
                expected,
                got,
            } => vec![wrong_arg_count(
                "function", func_name, *expected, *got, *span,
            )],
            InvalidArgsForInclude { span } => {
                vec![with_span("invalid arguments for `include`", *span)]
            }
            InvalidItemInSprite { span } => {
                vec![with_span("invalid arguments in sprite", *span)]
            }
            InvalidMacroParameter { span } => {
                vec![with_span("invalid macro parameter", *span).with_notes(
                    vec![
                        "expected symbol or node consisiting of a symbol and \
                        more parameters"
                            .to_owned(),
                    ],
                )]
            }
            InvalidMacroSignature { span } => {
                vec![with_span("invalid macro signature", *span)]
            }
            MacroDefinitionMissingBody { span } => {
                vec![with_span("macro definition is missing a body", *span)]
            }
            MacroDefinitionMissingSignature { span } => {
                vec![with_span(
                    "macro definition is missing a signature",
                    *span,
                )]
            }
            ProgramMissingStage => {
                vec![just_message("program is missing a stage")]
            }
            SpriteMissingName {
                span,
                candidate_symbol,
            } => {
                let mut diagnostic =
                    with_span("sprite is missing a name", *span);
                if let Some(candidate_symbol) = candidate_symbol {
                    diagnostic.labels.push(
                        secondary(*candidate_symbol)
                            .with_message("expected string but got a symbol"),
                    );
                    vec![
                        diagnostic,
                        Diagnostic::help().with_message(
                            "If this symbol is meant to be the name, try \
                            wrapping it in double quotes",
                        ),
                    ]
                } else {
                    vec![diagnostic]
                }
            }
            SymbolMacroInInlinePosition { span } => vec![with_span(
                "symbol macro cannot be used in inline position",
                *span,
            )],
            SymConcatEmptySymbol { span } => vec![
                with_span("`sym-concat!` cannot create an empty symbol", *span),
                Diagnostic::help().with_message(
                    "At least one symbol must be provided as an argument",
                ),
            ],
            UnknownFunction { span, func_name } => {
                vec![with_span(
                    format!("unknown function: `{func_name}`"),
                    *span,
                )]
            }
            UnknownList { span, list_name } => {
                vec![with_span(format!("unknown list: `{list_name}`"), *span)]
            }
            UnknownMetavariable { span, var_name } => {
                vec![with_span(
                    format!("unknown metavariable: `{var_name}`"),
                    *span,
                )]
            }
            UnknownProc { span, proc_name } => {
                vec![with_span(
                    format!("unknown procedure: `{proc_name}`"),
                    *span,
                )]
            }
            UnknownVar { span, var_name } => {
                vec![with_span(
                    format!("unknown variable: `{var_name}`"),
                    *span,
                )]
            }
            UnknownVarOrList { span, sym_name } => vec![with_span(
                format!("unknown variable or list: `{sym_name}`"),
                *span,
            )],
            UnquoteOutsideOfMacro { span } => vec![with_span(
                "unquote can only be used in macro definitions",
                *span,
            )],
        };

        emit_all(&diagnostics);
    }
}

fn just_message(message: impl ToString) -> Diagnostic {
    Diagnostic::error().with_message(message)
}

fn with_span(message: impl ToString, span: Span) -> Diagnostic {
    just_message(message).with_labels(vec![primary(span)])
}

fn wrong_arg_count(
    kind: &str,
    name: &str,
    expected: usize,
    got: usize,
    span: Span,
) -> Diagnostic {
    with_span(
        format!(
            "{kind} `{name}` expected {expected} {} but got {got}",
            plural(expected, "argument", "arguments"),
        ),
        span,
    )
}
