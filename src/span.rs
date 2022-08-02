#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub position: codespan::Span,
    pub file: codespan::FileId,
}
