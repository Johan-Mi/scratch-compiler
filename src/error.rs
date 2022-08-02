use crate::{span::Span, FILES};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
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
    UnknownList {
        span: Span,
        list_name: SmolStr,
    },
    UnknownMetavariable {
        span: Span,
        var_name: String,
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
        let (primary_span, message) = match self {
            FunctionMacroWrongArgCount {
                span,
                macro_name,
                expected,
                got,
            } => (
                span,
                format!(
                    "function macro `{macro_name}` expected {expected} {} but\
                    got {got}",
                    plural(*expected, "argument", "arguments"),
                ),
            ),
            InvalidArgsForInclude { span } => {
                (span, String::from("invalid arguments for `include`"))
            }
            InvalidMacroSignature { span } => {
                (span, String::from("invalid macro signature"))
            }
            MacroDefinitionMissingBody { span } => {
                (span, String::from("macro definition is missing a body"))
            }
            MacroDefinitionMissingSignature { span } => (
                span,
                String::from("macro definition is missing a signature"),
            ),
            UnknownList { span, list_name } => {
                (span, format!("unknown list: `{list_name}`"))
            }
            UnknownMetavariable { span, var_name } => {
                (span, format!("unknown metavariable: `{var_name}`"))
            }
            UnknownVar { span, var_name } => {
                (span, format!("unknown variable: `{var_name}`"))
            }
            UnknownVarOrList { span, sym_name } => {
                (span, format!("unknown variable or list: `{sym_name}`"))
            }
        };

        let diagnostic =
            Diagnostic::error().with_message(message).with_labels(vec![
                Label::primary(primary_span.file, primary_span.position),
            ]);

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
