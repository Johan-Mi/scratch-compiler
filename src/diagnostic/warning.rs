use super::{emit_all, primary, secondary, Diagnostic};
use codemap::{CodeMap, Span};
use codemap_diagnostic::SpanLabel as Label;

pub enum Warning {
    ParenTooFarLeft {
        left: Span,
        right: Span,
    },
    InconsistentIndentation {
        node: Span,
        good: Span,
        offender: Span,
    },
}

impl Warning {
    pub fn emit(&self, code_map: &CodeMap) {
        use Warning::*;
        let diagnostic = match self {
            ParenTooFarLeft { left, right } => warning(
                "misleading formatting",
                vec![
                    primary(
                        *right,
                        "this parenthesis is further to the left than its match"
                        .to_owned(),
                    ),
                    secondary(*left, "match is here".to_owned()),
                ],
            ),
            InconsistentIndentation {
                node,
                good,
                offender,
            } => warning("inconsistent indentation", vec![
                primary(*node, "nodes spanning multiple lines should have the same level of indentation for all non-initial lines".to_owned()),
                secondary(*good, "if this item is indented correctly...".to_owned()),
                secondary(*offender, "...then this is not".to_owned()),
            ]),
        };

        emit_all(&[diagnostic], code_map);
    }
}

fn warning(message: impl Into<String>, labels: Vec<Label>) -> Diagnostic {
    Diagnostic {
        level: codemap_diagnostic::Level::Warning,
        message: message.into(),
        code: None,
        spans: labels,
    }
}
