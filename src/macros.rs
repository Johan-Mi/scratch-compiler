use crate::{
    ast::{all_symbols, Ast},
    rewrite::TreeWalk,
};
use fancy_match::fancy_match;
use std::collections::HashMap;

pub fn expand(program: Vec<Ast>) -> Vec<Ast> {
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
    fn define(&mut self, args: Vec<Ast>) {
        // TODO: Error handling
        let mut args = args.into_iter();
        let signature = args.next().unwrap();
        match signature {
            Ast::Sym(macro_name) => {
                let body = args.next().unwrap();
                assert!(args.next().is_none());
                self.symbols.insert(macro_name, body);
            }
            Ast::Node(box Ast::Sym(macro_name), params) => {
                let params = all_symbols(params);
                let body = args.next().unwrap();
                assert!(args.next().is_none());
                self.functions
                    .insert(macro_name, FunctionMacro { params, body });
            }
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
                    let bindings =
                        params.iter().map(String::as_str).zip(args).collect();
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

        #[fancy_match]
        match ast {
            Ast::Node(box Ast::Sym("macro"), args) => {
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
