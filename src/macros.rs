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

    fn transform_shallow(&mut self, ast: Ast) -> Rewrite<Ast> {
        [
            |this: &mut Self, ast| Self::use_builtin_macros(this, ast),
            |this: &mut Self, ast| Self::use_user_defined_macros(this, ast),
            Self::use_inline_include,
        ]
        .iter()
        .fold(Clean(ast), |ast, f| ast.bind(|ast| f(self, ast)))
    }

    fn transform_deep(&mut self, ast: Ast) -> Rewrite<Ast> {
        Rewrite::repeat(ast, |ast| {
            ast.bottom_up(|branch| self.transform_shallow(branch))
        })
    }

    fn transform_top_level(&mut self, ast: Ast) {
        let ast = self.transform_deep(ast).into_inner();

        #[fancy_match]
        match ast {
            Ast::Node(box Ast::Sym("macro"), args) => self.define(args),
            Ast::Node(box Ast::Sym("include"), args) => {
                for item in self.include(&args) {
                    self.transform_top_level(item);
                }
            }
            _ => self.asts.push(ast),
        }
    }

    fn include(&mut self, args: &[Ast]) -> Vec<Ast> {
        match args {
            [Ast::String(path)] => {
                let source = fs::read_to_string(path).unwrap();
                program(&source).unwrap().1
            }
            _ => todo!("invalid arguments for `include`:\n{args:#?}"),
        }
    }

    fn use_user_defined_macros(&self, ast: Ast) -> Rewrite<Ast> {
        match &ast {
            Ast::Sym(sym) => self.symbols.get(sym).cloned(),
            Ast::Node(box Ast::Sym(sym), args) => {
                self.functions.get(sym).map(|func_macro| {
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
                    interpolate(func_macro.body.clone(), &bindings)
                })
            }
            _ => None,
        }
        .map_or(Clean(ast), Dirty)
    }

    fn use_builtin_macros(&self, ast: Ast) -> Rewrite<Ast> {
        (|| {
            let (sym, args) = match &ast {
                Ast::Node(box Ast::Sym(sym), args) => (sym, args),
                _ => return None,
            };
            match &**sym {
                "str-concat!" => args
                    .iter()
                    .map(|arg| match arg {
                        Ast::String(s) => Some(&**s),
                        _ => None,
                    })
                    .collect::<Option<_>>()
                    .map(Ast::String),
                "sym-concat!" => {
                    assert!(
                        !args.is_empty(),
                        "`sym-concat!` cannot create an empty symbol"
                    );
                    args.iter()
                        .map(|arg| match arg {
                            Ast::Sym(sym) => Some(&**sym),
                            _ => None,
                        })
                        .collect::<Option<_>>()
                        .map(Ast::Sym)
                }
                "include-str" => match &args[..] {
                    [Ast::String(path)] => {
                        Some(Ast::String(fs::read_to_string(path).unwrap()))
                    }
                    _ => None,
                },
                _ => None,
            }
        })()
        .map_or(Clean(ast), Dirty)
    }

    fn use_inline_include(&mut self, ast: Ast) -> Rewrite<Ast> {
        let (head, tail) = match ast {
            Ast::Node(head, tail) => (head, tail),
            _ => return Clean(ast),
        };

        if !tail.iter().any(|item| item.is_the_function_call("include")) {
            return Clean(Ast::Node(head, tail));
        }

        let tail = tail
            .into_iter()
            .map(|item| {
                #[fancy_match]
                match &item {
                    Ast::Node(box Ast::Sym("include"), args) => {
                        Dirty(self.include(args))
                    }
                    _ => Clean(vec![item]),
                }
            })
            .collect::<Rewrite<Vec<Vec<Ast>>>>();

        tail.map(|tail| Ast::Node(head, tail.into_iter().flatten().collect()))
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
