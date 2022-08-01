use crate::FILES;
use std::{fmt, path::Path};

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub position: codespan::Span,
    pub file: codespan::FileId,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let files = FILES.lock().unwrap();
        let location =
            files.location(self.file, self.position.start()).unwrap();
        write!(
            f,
            "{}:{}:{}",
            Path::new(files.name(self.file)).display(),
            location.line.number(),
            location.column.number(),
        )
    }
}
