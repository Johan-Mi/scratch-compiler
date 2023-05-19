use crate::{ast::Ast, diagnostic::Warning};
use codemap::{CodeMap, Span};

pub fn lint_ast(ast: &Ast, code_map: &CodeMap) {
    match ast {
        Ast::Node(head, tail, span) => {
            paren_too_far_left(*span, code_map);
            inconsistent_indentation(tail, *span, code_map);
            lint_ast(head, code_map);
            for ast in tail {
                lint_ast(ast, code_map);
            }
        }
        Ast::Unquote(unquoted, _) => lint_ast(unquoted, code_map),
        _ => {}
    }
}

fn paren_too_far_left(span: Span, code_map: &CodeMap) {
    let left = span.low();
    let right = left + (span.high() - left - 1);
    let left_column = code_map.look_up_pos(left).position.column;
    let right_column = code_map.look_up_pos(right).position.column;
    if right_column < left_column {
        Warning::ParenTooFarLeft {
            left: span.subspan(0, 1),
            right: span.subspan(span.len() - 1, span.len()),
        }
        .emit(code_map);
    }
}

fn inconsistent_indentation(tail: &[Ast], span: Span, code_map: &CodeMap) {
    let mut already_handled_line =
        code_map.look_up_pos(span.low()).position.line;
    let mut prev_column = None;
    let mut good = None;
    for ast in tail {
        let subspan = ast.span();
        let loc = code_map.look_up_pos(subspan.low()).position;
        if loc.line != already_handled_line {
            if let Some(prev_column) = prev_column {
                if loc.column != prev_column {
                    Warning::InconsistentIndentation {
                        node: span,
                        good: good.unwrap(),
                        offender: subspan,
                    }
                    .emit(code_map);
                    return;
                }
            } else {
                prev_column = Some(loc.column);
                good = Some(subspan);
            }
        }
        already_handled_line = loc.line;
    }
}
