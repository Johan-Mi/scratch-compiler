use crate::ast::Ast;
use std::{collections::HashMap, mem::take};

pub(crate) fn expand(program: Vec<Ast>) -> Vec<Ast> {
    let mut ctx = MacroContext {
        symbols: HashMap::new(),
    };
    program
        .into_iter()
        .filter_map(|ast| ctx.transform_top_level(ast))
        .collect()
}

struct MacroContext {
    symbols: HashMap<String, Ast>,
}

impl MacroContext {
    fn define(&mut self, mut args: Vec<Ast>) {
        match &mut args[..] {
            [Ast::Sym(macro_name), body] => {
                self.symbols.insert(take(macro_name), take(body));
            }
            // TODO: Error handling
            _ => todo!(),
        }
    }

    fn transform_shallow(&self, ast: Ast) -> Ast {
        if let Ast::Sym(sym) = &ast {
            if let Some(body) = self.symbols.get(sym) {
                return body.clone();
            }
        }

        ast
    }

    fn transform_deep(&self, ast: Ast) -> Ast {
        let ast = match ast {
            Ast::Node(mut head, tail) => {
                *head = self.transform_deep(*head);
                Ast::Node(
                    head,
                    tail.into_iter().map(|t| self.transform_deep(t)).collect(),
                )
            }
            Ast::Unquote(unquoted) => *unquoted,
            Ast::Num(_) | Ast::String(_) | Ast::Sym(_) => ast,
        };

        self.transform_shallow(ast)
    }

    fn transform_top_level(&mut self, ast: Ast) -> Option<Ast> {
        let ast = self.transform_deep(ast);

        match ast {
            Ast::Node(box Ast::Sym(sym), args) if sym == "macro" => {
                self.define(args);
                None
            }
            _ => Some(ast),
        }
    }
}
