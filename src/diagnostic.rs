mod error;
pub use error::Error;

use codemap::{CodeMap, Span};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, SpanLabel as Label, SpanStyle};

pub type Result<T> = std::result::Result<T, Box<Error>>;

const fn plural<'a>(count: usize, one: &'a str, many: &'a str) -> &'a str {
    if count == 1 {
        one
    } else {
        many
    }
}

fn primary(span: Span, label: impl Into<Option<String>>) -> Label {
    Label {
        span,
        label: label.into(),
        style: SpanStyle::Primary,
    }
}

fn secondary(span: Span, label: impl Into<Option<String>>) -> Label {
    Label {
        span,
        label: label.into(),
        style: SpanStyle::Secondary,
    }
}

fn emit_all(diagnostics: &[Diagnostic], code_map: &CodeMap) {
    let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(code_map));
    emitter.emit(diagnostics);
}
