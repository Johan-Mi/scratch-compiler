use crate::{span::Span, FILES};
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};
use smol_str::SmolStr;
use std::io;

pub type Result<T> = std::result::Result<T, Box<Error>>;

#[derive(Debug)]
pub enum Error {
    CouldNotCreateSb3File {
        inner: io::Error,
    },
    CouldNotCreateProjectJson {
        inner: zip::result::ZipError,
    },
    CouldNotFinishZip {
        inner: zip::result::ZipError,
    },
    FunctionMacroWrongArgCount {
        span: Span,
        macro_name: String,
        expected: usize,
        got: usize,
    },
    FunctionWrongArgCount {
        span: Span,
        func_name: String,
        expected: usize,
        got: usize,
    },
    InvalidArgsForInclude {
        span: Span,
    },
    InvalidItemInSprite {
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
}

impl Error {
    pub fn emit(&self) {
        use Error::*;
        let diagnostics = match self {
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
            FunctionMacroWrongArgCount {
                span,
                macro_name,
                expected,
                got,
            } => vec![with_span(
                format!(
                    "function macro `{macro_name}` expected {expected} {} but \
                    got {got}",
                    plural(*expected, "argument", "arguments"),
                ),
                *span,
            )],
            FunctionWrongArgCount {
                span,
                func_name,
                expected,
                got,
            } => vec![with_span(
                format!(
                    "function `{func_name}` expected {expected} {} but \
                    got {got}",
                    plural(*expected, "argument", "arguments"),
                ),
                *span,
            )],
            InvalidArgsForInclude { span } => {
                vec![with_span("invalid arguments for `include`", *span)]
            }
            InvalidItemInSprite { span } => {
                vec![with_span("invalid arguments in sprite", *span)]
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
        };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        let files = &*FILES.lock().unwrap();

        for diagnostic in &diagnostics {
            term::emit(&mut writer.lock(), &config, files, diagnostic).unwrap();
        }
    }
}

fn plural<'a>(count: usize, one: &'a str, many: &'a str) -> &'a str {
    if count == 1 {
        one
    } else {
        many
    }
}

type Diagnostic = codespan_reporting::diagnostic::Diagnostic<codespan::FileId>;
type Label = codespan_reporting::diagnostic::Label<codespan::FileId>;

fn just_message(message: impl Into<String>) -> Diagnostic {
    Diagnostic::error().with_message(message)
}

fn with_span(message: impl Into<String>, span: Span) -> Diagnostic {
    just_message(message)
        .with_labels(vec![Label::primary(span.file, span.position)])
}

fn secondary(span: Span) -> Label {
    Label::secondary(span.file, span.position)
}
