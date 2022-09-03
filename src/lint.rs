use crate::{ast::Ast, diagnostic::Warning, span::Span, FILES};
use codespan::{ByteOffset, Files, Location};

pub fn lint_ast(ast: &Ast) {
    match ast {
        Ast::Node(head, tail, span) => {
            paren_too_far_left(*span);
            inconsistent_indentation(tail, *span);
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

fn inconsistent_indentation(tail: &[Ast], span: Span) {
    let files = FILES.lock().unwrap();
    let mut already_handled_line = start_location(&files, span).line;
    let mut prev_column = None;
    let mut good = None;
    for ast in tail {
        let subspan = ast.span();
        let Location { line, column } = start_location(&files, subspan);
        if line != already_handled_line {
            if let Some(prev_column) = prev_column {
                if column != prev_column {
                    drop(files);
                    Warning::InconsistentIndentation {
                        node: span,
                        good: good.unwrap(),
                        offender: subspan,
                    }
                    .emit();
                    return;
                }
            } else {
                prev_column = Some(column);
                good = Some(subspan);
            }
        }
        already_handled_line = line;
    }
}

fn start_location(files: &Files<String>, span: Span) -> Location {
    files.location(span.file, span.position.start()).unwrap()
}
