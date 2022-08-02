use crate::{span::Span, FILES};
use codespan_reporting::{
    diagnostic::Label,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use smol_str::SmolStr;

pub type Result<T> = std::result::Result<T, Box<Error>>;

#[derive(Debug)]
pub enum Error {
    FunctionMacroWrongArgCount {
        span: Span,
        macro_name: String,
        expected: usize,
        got: usize,
    },
    InvalidArgsForInclude {
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
        let diagnostic = match self {
            FunctionMacroWrongArgCount {
                span,
                macro_name,
                expected,
                got,
            } => with_span(
                format!(
                    "function macro `{macro_name}` expected {expected} {} but \
                    got {got}",
                    plural(*expected, "argument", "arguments"),
                ),
                *span,
            ),
            InvalidArgsForInclude { span } => {
                with_span("invalid arguments for `include`", *span)
            }
            InvalidMacroSignature { span } => {
                with_span("invalid macro signature", *span)
            }
            MacroDefinitionMissingBody { span } => {
                with_span("macro definition is missing a body", *span)
            }
            MacroDefinitionMissingSignature { span } => {
                with_span("macro definition is missing a signature", *span)
            }
            ProgramMissingStage => just_message("program is missing a stage"),
            UnknownFunction { span, func_name } => {
                with_span(format!("unknown function: `{func_name}`"), *span)
            }
            UnknownList { span, list_name } => {
                with_span(format!("unknown list: `{list_name}`"), *span)
            }
            UnknownMetavariable { span, var_name } => {
                with_span(format!("unknown metavariable: `{var_name}`"), *span)
            }
            UnknownProc { span, proc_name } => {
                with_span(format!("unknown procedure: `{proc_name}`"), *span)
            }
            UnknownVar { span, var_name } => {
                with_span(format!("unknown variable: `{var_name}`"), *span)
            }
            UnknownVarOrList { span, sym_name } => with_span(
                format!("unknown variable or list: `{sym_name}`"),
                *span,
            ),
        };

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        let files = &*FILES.lock().unwrap();

        term::emit(&mut writer.lock(), &config, files, &diagnostic).unwrap();
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

fn just_message(message: impl Into<String>) -> Diagnostic {
    Diagnostic::error().with_message(message)
}

fn with_span(message: impl Into<String>, span: Span) -> Diagnostic {
    just_message(message)
        .with_labels(vec![Label::primary(span.file, span.position)])
}
