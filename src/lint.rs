use crate::{ast::Ast, error::Warning, span::Span, FILES};
use codespan::ByteOffset;

pub fn lint_ast(ast: &Ast) {
    match ast {
        Ast::Node(head, tail, span) => {
            paren_too_far_left(*span);
            lint_ast(head);
            for ast in tail {
                lint_ast(ast);
            }
        }
        Ast::Unquote(unquoted, _) => lint_ast(unquoted),
        _ => {}
    }
}

fn paren_too_far_left(span: Span) {
    let file = span.file;
    let left = span.position.start();
    let right = span.position.end() - ByteOffset(1);
    let (left_column, right_column) = {
        let files = &*FILES.lock().unwrap();
        (
            files.location(file, left).unwrap().column,
            files.location(file, right).unwrap().column,
        )
    };
    if right_column < left_column {
        Warning::ParenTooFarLeft {
            left: Span {
                position: codespan::Span::new(left, left + ByteOffset(1)),
                file,
            },
            right: Span {
                position: codespan::Span::new(right, right + ByteOffset(1)),
                file,
            },
        }
        .emit();
    }
}
