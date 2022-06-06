use crate::{
    ast::{all_symbols, Ast},
    parser::program,
};
use fancy_match::fancy_match;
use std::{collections::HashMap, fs};
use trexp::{Clean, Dirty, Rewrite, TreeWalk};

pub fn expand(program: Vec<Ast>) -> Vec<Ast> {
    let mut ctx = MacroContext::default();
    for ast in program {
        ctx.transform_top_level(ast);
    }
    ctx.asts
}

#[derive(Default)]
struct MacroContext {
    asts: Vec<Ast>,
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
            _ => todo!("invalid macro signature:\n{signature:#?}"),
        }
    }

    fn transform_shallow(&self, ast: Ast) -> Rewrite<Ast> {
        #[fancy_match]
        match &ast {
            Ast::Node(box Ast::Sym("str-concat!"), args) => {
                let strs = args
                    .iter()
                    .map(|arg| match arg {
                        Ast::String(s) => Some(&**s),
                        _ => None,
                    })
                    .collect::<Option<_>>();
                strs.map_or(Clean(ast), |concatenated| {
                    Dirty(Ast::String(concatenated))
                })
            }
            Ast::Node(box Ast::Sym("sym-concat!"), args) => {
                assert!(
                    !args.is_empty(),
                    "`sym-concat!` cannot create an empty symbol"
                );
                let syms = args
                    .iter()
                    .map(|arg| match arg {
                        Ast::Sym(sym) => Some(&**sym),
                        _ => None,
                    })
                    .collect::<Option<_>>();
                syms.map_or(Clean(ast), |concatenated| {
                    Dirty(Ast::Sym(concatenated))
                })
            }
            Ast::Sym(sym) => {
                if let Some(body) = self.symbols.get(sym) {
                    Dirty(body.clone())
                } else {
                    Clean(ast)
                }
            }
            Ast::Node(box Ast::Sym(sym), args) => {
                if let Some(func_macro) = self.functions.get(sym) {
                    let params = &func_macro.params;
                    let num_args = args.len();
                    let num_params = params.len();
                    assert_eq!(
                        num_args, num_params,
                        "function macro `{sym}` expected {num_params}\
                        arguments but got {num_args}"
                    );
                    let bindings =
                        params.iter().map(String::as_str).zip(args).collect();
                    Dirty(interpolate(func_macro.body.clone(), &bindings))
                } else {
                    Clean(ast)
                }
            }
            _ => Clean(ast),
        }
    }

    fn transform_deep(&self, ast: Ast) -> Rewrite<Ast> {
        Rewrite::repeat(ast, |ast| {
            ast.bottom_up(|branch| self.transform_shallow(branch))
        })
    }

    fn transform_top_level(&mut self, ast: Ast) {
        let ast = self.transform_deep(ast).into_inner();

        #[fancy_match]
        match ast {
            Ast::Node(box Ast::Sym("macro"), args) => self.define(args),
            Ast::Node(box Ast::Sym("include"), args) => self.include(args),
            _ => self.asts.push(ast),
        }
    }

    fn include(&mut self, args: Vec<Ast>) {
        match &args[..] {
            [Ast::String(path)] => {
                let source = fs::read_to_string(path).unwrap();
                let (_, parsed) = program(&source).unwrap();
                for ast in parsed {
                    self.transform_top_level(ast);
                }
            }
            _ => todo!("invalid arguments for `include`:\n{args:#?}"),
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
