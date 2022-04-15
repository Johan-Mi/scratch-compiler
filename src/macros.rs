use crate::{
    ast::{all_symbols, Ast},
    rewrite::TreeWalk,
};
use std::{collections::HashMap, mem::take};

pub(crate) fn expand(program: Vec<Ast>) -> Vec<Ast> {
    let mut ctx = MacroContext {
        symbols: HashMap::new(),
        functions: HashMap::new(),
    };
    program
        .into_iter()
        .filter_map(|ast| ctx.transform_top_level(ast))
        .collect()
}

struct MacroContext {
    symbols: HashMap<String, Ast>,
    functions: HashMap<String, FunctionMacro>,
}

impl MacroContext {
    fn define(&mut self, mut args: Vec<Ast>) {
        match &mut args[..] {
            [Ast::Sym(macro_name), body] => {
                self.symbols.insert(take(macro_name), take(body));
            }
            [Ast::Node(box Ast::Sym(macro_name), params), body] => {
                let params = all_symbols(take(params));
                self.functions.insert(
                    take(macro_name),
                    FunctionMacro {
                        params,
                        body: take(body),
                    },
                );
            }
            // TODO: Error handling
            _ => todo!(),
        }
    }

    fn transform_shallow(&self, ast: Ast) -> Ast {
        match &ast {
            Ast::Sym(sym) => {
                if let Some(body) = self.symbols.get(sym) {
                    body.clone()
                } else {
                    ast
                }
            }
            Ast::Node(box Ast::Sym(sym), args) => {
                if let Some(func_macro) = self.functions.get(sym) {
                    let params = &func_macro.params;
                    // TODO: Error handling
                    assert_eq!(args.len(), params.len());
                    let bindings = params
                        .iter()
                        .map(String::as_str)
                        .zip(args.iter())
                        .collect();
                    interpolate(func_macro.body.clone(), &bindings)
                } else {
                    ast
                }
            }
            _ => ast,
        }
    }

    fn transform_deep(&self, ast: Ast) -> Ast {
        ast.bottom_up(|tree| self.transform_shallow(tree))
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

fn interpolate(body: Ast, bindings: &HashMap<&str, &Ast>) -> Ast {
    match body {
        Ast::Unquote(box Ast::Sym(sym)) => {
            // TODO: Error handling
            bindings.get(&*sym).copied().unwrap().clone()
        }
        Ast::Unquote(unquoted) => *unquoted,
        _ => body.each_branch(|tree| interpolate(tree, bindings)),
    }
}

struct FunctionMacro {
    params: Vec<String>,
    body: Ast,
}
