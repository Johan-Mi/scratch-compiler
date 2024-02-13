use super::{emit_all, plural, primary, secondary, Diagnostic};
use codemap::{CodeMap, Span};
use codemap_diagnostic::SpanLabel as Label;
use ecow::EcoString;
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
    FailedToReadSourceCode {
        inner: io::Error,
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
    InvalidParameterForCustomProcDef {
        span: Span,
    },
    InvalidTopLevelItem {
        span: Span,
    },
    MacroDefinitionMissingBody {
        span: Span,
    },
    MacroDefinitionMissingSignature {
        span: Span,
    },
    Parse(String),
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
        list_name: EcoString,
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
        var_name: EcoString,
    },
    UnknownVarOrList {
        span: Span,
        sym_name: EcoString,
    },
    UnquoteOutsideOfMacro {
        span: Span,
    },
}

impl Error {
    pub fn emit(&self, code_map: &CodeMap) {
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
            CouldNotCreateSb3File { inner } => vec![
                error("could not create SB3 file", Vec::new()),
                note(inner.to_string()),
            ],
            CouldNotCreateProjectJson { inner } => vec![
                error("could not create `project.json`", Vec::new()),
                note(inner.to_string()),
            ],
            CouldNotFinishZip { inner } => vec![
                error("could not finish zip archive", Vec::new()),
                note(inner.to_string()),
            ],
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
            FailedToReadSourceCode { inner } => vec![
                error("failed to read source code", Vec::new()),
                note(inner.to_string()),
            ],
            FunctionMacroMatchFailed {
                pattern,
                provided,
                macro_name,
            } => vec![error(
                format!(
                    "argument to function macro `{macro_name}` does not match \
                    the pattern in its definition"
                ),
                vec![
                    primary(*provided, "argument provided here".to_owned()),
                    secondary(*pattern, "pattern was defined here".to_owned()),
                ],
            )],
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
            FunctionNameMustBeSymbol { span } => vec![error(
                "function name must be a symbol",
                vec![primary(*span, None)],
            )],
            FunctionWrongArgCount {
                span,
                func_name,
                expected,
                got,
            } => vec![wrong_arg_count(
                "function", func_name, *expected, *got, *span,
            )],
            InvalidArgsForInclude { span } => vec![error(
                "invalid arguments for `include`",
                vec![primary(*span, None)],
            )],
            InvalidItemInSprite { span } => vec![error(
                "invalid item in sprite",
                vec![primary(*span, None)],
            )],
            InvalidMacroParameter { span } => vec![error(
                "invalid macro parameter",
                vec![primary(
                    *span,
                    "expected symbol or node consisiting of a symbol and \
                        more parameters"
                        .to_owned(),
                )],
            )],
            InvalidMacroSignature { span } => vec![error(
                "invalid macro signature",
                vec![primary(*span, None)],
            )],
            InvalidParameterForCustomProcDef { span } => vec![error(
                "invalid parameter for custom procedure definition",
                vec![primary(*span, "expected symbol".to_owned())],
            )],
            InvalidTopLevelItem { span } => vec![error(
                "invalid top-level item",
                vec![primary(
                    *span,
                    "expected macro or sprite definition".to_owned(),
                )],
            )],
            MacroDefinitionMissingBody { span } => vec![error(
                "macro definition is missing a body",
                vec![primary(*span, None)],
            )],
            MacroDefinitionMissingSignature { span } => vec![error(
                "macro definition is missing a signature",
                vec![primary(*span, None)],
            )],
            Parse(parse_error) => {
                vec![error("syntax error", Vec::new()), note(parse_error)]
            }
            ProgramMissingStage => {
                vec![error("program is missing a stage", Vec::new())]
            }
            SpriteMissingName {
                span,
                candidate_symbol,
            } => {
                let mut diagnostic = error(
                    "sprite is missing a name",
                    vec![primary(*span, None)],
                );
                if let Some(candidate_symbol) = candidate_symbol {
                    diagnostic.spans.push(secondary(
                        *candidate_symbol,
                        "expected string but got a symbol".to_owned(),
                    ));
                    vec![
                        diagnostic,
                        help(
                            "If this symbol is meant to be the name, try \
                            wrapping it in double quotes",
                        ),
                    ]
                } else {
                    vec![diagnostic]
                }
            }
            SymbolMacroInInlinePosition { span } => vec![error(
                "symbol macro cannot be used in inline position",
                vec![primary(*span, None)],
            )],
            SymConcatEmptySymbol { span } => vec![
                error(
                    "`sym-concat!` cannot create an empty symbol",
                    vec![primary(*span, None)],
                ),
                note("at least one symbol must be provided as an argument"),
            ],
            UnknownFunction { span, func_name } => vec![error(
                format!("unknown function: `{func_name}`"),
                vec![primary(*span, None)],
            )],
            UnknownList { span, list_name } => vec![error(
                format!("unknown list: `{list_name}`"),
                vec![primary(*span, None)],
            )],
            UnknownMetavariable { span, var_name } => vec![error(
                format!("unknown metavariable: `{var_name}`"),
                vec![primary(*span, None)],
            )],
            UnknownProc { span, proc_name } => vec![error(
                format!("unknown procedure: `{proc_name}`"),
                vec![primary(*span, None)],
            )],
            UnknownVar { span, var_name } => vec![error(
                format!("unknown variable: `{var_name}`"),
                vec![primary(*span, None)],
            )],
            UnknownVarOrList { span, sym_name } => vec![error(
                format!("unknown variable or list: `{sym_name}`"),
                vec![primary(*span, None)],
            )],
            UnquoteOutsideOfMacro { span } => vec![error(
                "unquote can only be used in macro definitions",
                vec![primary(*span, None)],
            )],
        };

        emit_all(&diagnostics, code_map);
    }
}

fn wrong_arg_count(
    kind: &str,
    name: &str,
    expected: usize,
    got: usize,
    span: Span,
) -> Diagnostic {
    error(
        format!(
            "{kind} `{name}` expected {expected} {} but got {got}",
            plural(expected, "argument", "arguments"),
        ),
        vec![primary(span, None)],
    )
}

fn error(message: impl Into<String>, labels: Vec<Label>) -> Diagnostic {
    Diagnostic {
        level: codemap_diagnostic::Level::Error,
        message: message.into(),
        code: None,
        spans: labels,
    }
}

fn note(message: impl Into<String>) -> Diagnostic {
    Diagnostic {
        level: codemap_diagnostic::Level::Note,
        message: message.into(),
        code: None,
        spans: Vec::new(),
    }
}

fn help(message: impl Into<String>) -> Diagnostic {
    Diagnostic {
        level: codemap_diagnostic::Level::Help,
        message: message.into(),
        code: None,
        spans: Vec::new(),
    }
}
