use codespan::Files;

use super::{emit_all, primary, secondary, Diagnostic};
use crate::span::Span;

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
    pub fn emit(&self, files: &Files<String>) {
        use Warning::*;
        let diagnostics = match self {
            ParenTooFarLeft { left, right } => vec![Diagnostic::warning()
                .with_message("misleading formatting")
                .with_labels(vec![primary(*right).with_message(
                    "this parenthesis is further to the left than its match",
                ), secondary(*left).with_message("match is here")])],
            InconsistentIndentation {
                node,
                good,
                offender,
            } => {
                vec![Diagnostic::warning()
                    .with_message("inconsistent indentation")
                    .with_labels(vec![
                    primary(*node).with_message(
                        "nodes spanning multiple lines should have the same \
                        level of indentation for all non-initial lines"
                    ),
                    secondary(*good).with_message(
                        "if this item is indented correctly..."
                    ),
                    secondary(*offender).with_message(
                        "...then this is not"
                    ),
                ])]
            }
        };

        emit_all(&diagnostics, files);
    }
}
