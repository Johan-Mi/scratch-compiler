use crate::{
    ast::Ast,
    diagnostic::{Error, Result},
    lint::lint_ast,
    parser::{input::Input, program},
    span::Span,
    Opts,
};
use fancy_match::fancy_match;
use std::{collections::HashMap, fs};
use trexp::{Clean, Dirty, Rewrite, TreeWalk};

pub fn expand(program: Vec<Ast>, opts: &Opts) -> Result<Vec<Ast>> {
    let mut ctx = MacroContext {
        opts,
        asts: Vec::new(),
        symbols: HashMap::new(),
        functions: HashMap::new(),
    };
    for ast in program {
        ctx.transform_top_level(ast)?;
    }
    Ok(ctx.asts)
}

enum Macro {
    Symbol(Ast),
    Function(FunctionMacro),
}

impl Macro {
    fn parse(args: Vec<Ast>, span: Span) -> Result<(String, Self)> {
        let mut args = args.into_iter();
        let signature = args.next().ok_or_else(|| {
            Box::new(Error::MacroDefinitionMissingSignature { span })
        })?;
        match signature {
            Ast::Sym(macro_name, ..) => {
                let body = args.next().ok_or_else(|| {
                    Box::new(Error::MacroDefinitionMissingBody { span })
                })?;
                assert!(args.next().is_none());
                Ok((macro_name, Self::Symbol(body)))
            }
            Ast::Node(box Ast::Sym(macro_name, ..), params, ..) => {
                let params = params
                    .into_iter()
                    .map(Parameter::from_ast)
                    .collect::<Result<_>>()?;
                let body = args.next().ok_or_else(|| {
                    Box::new(Error::MacroDefinitionMissingBody { span })
                })?;
                assert!(args.next().is_none());
                Ok((macro_name, Self::Function(FunctionMacro { params, body })))
            }
            invalid_signature => Err(Box::new(Error::InvalidMacroSignature {
                span: invalid_signature.span(),
            })),
        }
    }
}

struct MacroContext<'a> {
    opts: &'a Opts,
    asts: Vec<Ast>,
    symbols: HashMap<String, Ast>,
    functions: HashMap<String, FunctionMacro>,
}

impl MacroContext<'_> {
    fn define(&mut self, args: Vec<Ast>, span: Span) -> Result<()> {
        match Macro::parse(args, span)? {
            (name, Macro::Symbol(body)) => {
                self.symbols.insert(name, body);
            }
            (name, Macro::Function(func)) => {
                self.functions.insert(name, func);
            }
        }
        Ok(())
    }

    fn transform_shallow(&self, ast: Ast) -> Result<Rewrite<Ast>> {
        [
            |_this: &Self, ast| Self::use_builtin_macros(ast),
            Self::use_user_defined_macros,
            Self::use_inline_include,
            Self::use_inline_macros,
        ]
        .iter()
        .try_fold(Clean(ast), |ast, f| ast.try_bind(|ast| f(self, ast)))
    }

    fn transform_deep(&self, ast: Ast) -> Result<Rewrite<Ast>> {
        Rewrite::try_repeat(ast, |ast| {
            ast.bottom_up(|branch| self.transform_shallow(branch))
        })
    }

    fn transform_top_level(&mut self, ast: Ast) -> Result<()> {
        // HACK: Prevents early expansion of macro body, while still allowing
        // macros to define other macros.
        let ast = if ast.is_the_function_call("macro") {
            ast
        } else {
            self.transform_deep(ast)?.into_inner()
        };

        #[fancy_match]
        match ast {
            Ast::Node(box Ast::Sym("macro", ..), args, span) => {
                self.define(args, span)
            }
            Ast::Node(box Ast::Sym("include", ..), args, span) => {
                for item in self.include(&args, span)? {
                    self.transform_top_level(item)?;
                }
                Ok(())
            }
            _ => {
                self.asts.push(ast);
                Ok(())
            }
        }
    }

    fn use_user_defined_macros(&self, ast: Ast) -> Result<Rewrite<Ast>> {
        Ok(match &ast {
            Ast::Sym(sym, ..) => self.symbols.get(sym).cloned(),
            Ast::Node(box Ast::Sym(sym, ..), args, span) => self
                .functions
                .get(sym)
                .map(|func_macro| {
                    let params = &func_macro.params;
                    let num_args = args.len();
                    let num_params = params.len();
                    if num_args != num_params {
                        return Err(Box::new(
                            Error::FunctionMacroWrongArgCount {
                                span: *span,
                                macro_name: sym.clone(),
                                expected: num_params,
                                got: num_args,
                            },
                        ));
                    }
                    let mut bindings = HashMap::new();
                    for (param, arg) in params.iter().zip(args) {
                        param.pattern_match(
                            sym,
                            &self.transform_deep(arg.clone())?.into_inner(),
                            &mut bindings,
                        )?;
                    }
                    interpolate(func_macro.body.clone(), &bindings)
                })
                .transpose()?,
            _ => None,
        }
        .map_or(Clean(ast), Dirty))
    }

    fn use_builtin_macros(ast: Ast) -> Result<Rewrite<Ast>> {
        Ok((|| {
            let (sym, args, span) = match &ast {
                Ast::Node(box Ast::Sym(sym, ..), args, span) => {
                    (sym, args, *span)
                }
                _ => return None,
            };
            match &**sym {
                "str-concat!" => args
                    .iter()
                    .map(|arg| match arg {
                        Ast::String(s, ..) => Some(&**s),
                        _ => None,
                    })
                    .collect::<Option<_>>()
                    .map(|s| Ast::String(s, span)),
                "sym-concat!" => {
                    if args.is_empty() {
                        return Some(Err(Box::new(
                            Error::SymConcatEmptySymbol { span },
                        )));
                    }
                    args.iter()
                        .map(|arg| match arg {
                            Ast::Sym(sym, ..) => Some(&**sym),
                            _ => None,
                        })
                        .collect::<Option<_>>()
                        .map(|sym| Ast::Sym(sym, span))
                }
                "include-str" => match &args[..] {
                    [Ast::String(path, ..)] => Some(Ast::String(
                        fs::read_to_string(path).unwrap(),
                        span,
                    )),
                    _ => None,
                },
                _ => None,
            }
            .map(Ok)
        })()
        .transpose()?
        .map_or(Clean(ast), Dirty))
    }

    fn use_inline_include(&self, ast: Ast) -> Result<Rewrite<Ast>> {
        let Ast::Node(head, tail, span) = ast else {
            return Ok(Clean(ast));
        };

        if !tail.iter().any(|item| item.is_the_function_call("include")) {
            return Ok(Clean(Ast::Node(head, tail, span)));
        }

        let tail = tail
            .into_iter()
            .map(|item| {
                #[fancy_match]
                match &item {
                    Ast::Node(box Ast::Sym("include", ..), args, span) => {
                        self.include(args, *span).map(Dirty)
                    }
                    _ => Ok(Clean(vec![item])),
                }
            })
            .collect::<Result<Rewrite<Vec<Vec<Ast>>>>>()?;

        Ok(tail.map(|tail| {
            Ast::Node(head, tail.into_iter().flatten().collect(), span)
        }))
    }

    fn use_inline_macros(&self, ast: Ast) -> Result<Rewrite<Ast>> {
        #[fancy_match]
        match ast {
            Ast::Node(
                box Ast::Node(
                    box Ast::Sym("macro", _),
                    macro_definition,
                    def_span,
                ),
                args,
                span,
            ) => {
                let (macro_name, Macro::Function(func_macro)) =
                    Macro::parse(macro_definition, def_span)? else {
                        todo!();
                    };
                let num_args = args.len();
                let num_params = func_macro.params.len();
                if num_args != num_params {
                    return Err(Box::new(Error::FunctionMacroWrongArgCount {
                        span,
                        macro_name,
                        expected: num_params,
                        got: num_args,
                    }));
                }
                let mut bindings = HashMap::new();
                for (param, arg) in func_macro.params.iter().zip(args) {
                    param.pattern_match(
                        &macro_name,
                        &self.transform_deep(arg.clone())?.into_inner(),
                        &mut bindings,
                    )?;
                }
                interpolate(func_macro.body.clone(), &bindings).map(Dirty)
            }
            _ => Ok(Clean(ast)),
        }
    }

    fn include(&self, args: &[Ast], span: Span) -> Result<Vec<Ast>> {
        match args {
            [Ast::String(path, ..)] => {
                let source = fs::read_to_string(path).unwrap();
                let file_id =
                    crate::FILES.lock().unwrap().add(path, source.clone());
                let asts = program(Input::new(&source, file_id)).unwrap().1;
                if self.opts.lint {
                    for ast in &asts {
                        lint_ast(ast);
                    }
                }
                Ok(asts)
            }
            _ => Err(Box::new(Error::InvalidArgsForInclude { span })),
        }
    }
}

fn interpolate(body: Ast, bindings: &HashMap<String, Ast>) -> Result<Ast> {
    match body {
        Ast::Unquote(box Ast::Sym(sym, span), ..) => bindings
            .get(&*sym)
            .ok_or_else(|| {
                Box::new(Error::UnknownMetavariable {
                    span,
                    var_name: sym,
                })
            })
            .cloned(),
        Ast::Unquote(unquoted, ..) => Ok(*unquoted),
        _ => body.each_branch(|tree| interpolate(tree, bindings)),
    }
}

struct FunctionMacro {
    params: Vec<Parameter>,
    body: Ast,
}

enum Parameter {
    Var(String),
    Constructor(String, Vec<Parameter>, Span),
}

impl Parameter {
    fn from_ast(ast: Ast) -> Result<Self> {
        match ast {
            Ast::Sym(var, _) => Ok(Self::Var(var)),
            Ast::Node(box Ast::Sym(name, _), subparams, span) => {
                Ok(Self::Constructor(
                    name,
                    subparams
                        .into_iter()
                        .map(Self::from_ast)
                        .collect::<Result<_>>()?,
                    span,
                ))
            }
            _ => todo!(),
        }
    }

    fn pattern_match(
        &self,
        macro_name: &str,
        ast: &Ast,
        bindings: &mut HashMap<String, Ast>,
    ) -> Result<()> {
        match self {
            Self::Var(var) => {
                assert!(bindings.insert(var.clone(), ast.clone()).is_none());
                Ok(())
            }
            Self::Constructor(name, subparams, span) => match ast {
                Ast::Node(box Ast::Sym(sym, _), subtrees, _)
                    if sym == name && subparams.len() == subtrees.len() =>
                {
                    for (p, t) in subparams.iter().zip(subtrees) {
                        p.pattern_match(macro_name, t, bindings)?;
                    }
                    Ok(())
                }
                _ => Err(Box::new(Error::FunctionMacroMatchFailed {
                    pattern: *span,
                    provided: ast.span(),
                    macro_name: macro_name.to_owned(),
                })),
            },
        }
    }
}
