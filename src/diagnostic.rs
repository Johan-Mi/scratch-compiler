mod error;
pub use error::Error;
mod warning;
pub use warning::Warning;

use crate::{span::Span, FILES};
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
};

pub type Result<T> = std::result::Result<T, Box<Error>>;

type Diagnostic = codespan_reporting::diagnostic::Diagnostic<codespan::FileId>;
type Label = codespan_reporting::diagnostic::Label<codespan::FileId>;

const fn plural<'a>(count: usize, one: &'a str, many: &'a str) -> &'a str {
    if count == 1 {
        one
    } else {
        many
    }
}

fn primary(span: Span) -> Label {
    Label::primary(span.file, span.position)
}

fn secondary(span: Span) -> Label {
    Label::secondary(span.file, span.position)
}

fn emit_all(diagnostics: &[Diagnostic]) {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();
    let files = &*FILES.lock().unwrap();

    for diagnostic in diagnostics {
        term::emit(&mut writer.lock(), &config, files, diagnostic).unwrap();
    }
}
