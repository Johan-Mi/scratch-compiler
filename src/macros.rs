use crate::{
    ast::Ast,
    diagnostic::{Error, Result},
    lint::lint_ast,
    parser::{program, Input},
    Opts,
};
use codemap::{CodeMap, Span};
use fancy_match::fancy_match;
use std::{collections::HashMap, fs, mem};
use winnow::stream::Located;

pub fn expand(
    program: Vec<Ast>,
    opts: &Opts,
    code_map: &mut CodeMap,
) -> Result<Vec<Ast>> {
    let mut ctx = MacroContext {
        opts,
        code_map,
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
        let signature = args
            .next()
            .ok_or(Error::MacroDefinitionMissingSignature { span })?;
        match signature {
            Ast::Sym(macro_name, ..) => {
                let body = args
                    .next()
                    .ok_or(Error::MacroDefinitionMissingBody { span })?;
                assert!(args.next().is_none());
                Ok((macro_name, Self::Symbol(body)))
            }
            Ast::Node(box Ast::Sym(macro_name, ..), params, ..) => {
                let params = params
                    .into_iter()
                    .map(Parameter::from_ast)
                    .collect::<Result<_>>()?;
                let body = args
                    .next()
                    .ok_or(Error::MacroDefinitionMissingBody { span })?;
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
    code_map: &'a mut CodeMap,
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

    fn transform_shallow(&mut self, ast: &mut Ast) -> Result<bool> {
        Ok(Self::use_builtin_function_macros(ast)?
            | self.use_builtin_symbol_macros(ast)
            | self.use_user_defined_macros(ast)?
            | self.use_inline_include(ast)?
            | self.use_inline_macros(ast)?)
    }

    fn transform_deep(&mut self, ast: &mut Ast) -> Result<bool> {
        let mut dirty = false;
        while {
            let mut this_step_dirty = false;
            ast.traverse_postorder_mut(&mut |branch| {
                this_step_dirty |= self.transform_shallow(branch)?;
                Ok::<(), Box<Error>>(())
            })?;
            this_step_dirty
        } {
            dirty = true;
        }
        Ok(dirty)
    }

    fn transform_top_level(&mut self, mut ast: Ast) -> Result<()> {
        // HACK: Prevents early expansion of macro body, while still allowing
        // macros to define other macros.
        if !ast.is_the_function_call("macro") {
            self.transform_deep(&mut ast)?;
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
            Ast::Node(box Ast::Sym(sym, ..), mut args, _)
                if sym == "when!" || sym == "unless!" =>
            {
                let Some(Ast::Bool(condition, _)) = args.get(0) else {
                    todo!();
                };
                if *condition ^ (sym == "unless!") {
                    for item in args.drain(1..) {
                        self.transform_top_level(item)?;
                    }
                }
                Ok(())
            }
            _ => {
                self.asts.push(ast);
                Ok(())
            }
        }
    }

    fn use_user_defined_macros(&mut self, ast: &mut Ast) -> Result<bool> {
        Ok(match ast {
            Ast::Sym(sym, ..) => {
                let Some(symbol_macro) = self.symbols.get(sym) else {
                    return Ok(false);
                };
                *ast = symbol_macro.clone();
                true
            }
            Ast::Node(box Ast::Sym(sym, ..), args, span) => {
                let Some(func_macro) = self.functions.get(sym) else {
                    return Ok(false);
                };
                let params = &func_macro.params.clone();
                let num_args = args.len();
                let num_params = params.len();
                if num_args != num_params {
                    return Err(Box::new(Error::FunctionMacroWrongArgCount {
                        span: *span,
                        macro_name: sym.clone(),
                        expected: num_params,
                        got: num_args,
                    }));
                }
                let body = func_macro.body.clone();
                let mut bindings = HashMap::new();
                for (param, mut arg) in params.iter().zip(mem::take(args)) {
                    self.transform_deep(&mut arg)?;
                    param.pattern_match(sym, arg, &mut bindings)?;
                }
                *ast = interpolate(body, &bindings)?;
                true
            }
            _ => false,
        })
    }

    fn use_builtin_symbol_macros(&self, ast: &mut Ast) -> bool {
        let Ast::Sym(sym, span) = ast else {
            return false;
        };
        match &**sym {
            "COMPILER-OPTIONS.TARGET" => {
                *ast = Ast::String(self.opts.target.to_str().to_owned(), *span);
                true
            }
            _ => false,
        }
    }

    fn use_builtin_function_macros(ast: &mut Ast) -> Result<bool> {
        let Ast::Node(box Ast::Sym(sym, ..), args, span) = ast else {
            return Ok(false);
        };
        Ok(match &**sym {
            "str-concat!" => {
                let Some(s) = args
                    .iter()
                    .map(|arg| match arg {
                        Ast::String(s, ..) => Some(&**s),
                        _ => None,
                    })
                    .collect()
                else {
                    return Ok(false);
                };
                *ast = Ast::String(s, *span);
                true
            }
            "sym-concat!" => {
                if args.is_empty() {
                    return Err(Box::new(Error::SymConcatEmptySymbol {
                        span: *span,
                    }));
                }
                let Some(sym) = args
                    .iter()
                    .map(|arg| match arg {
                        Ast::Sym(sym, ..) => Some(&**sym),
                        _ => None,
                    })
                    .collect()
                else {
                    return Ok(false);
                };
                *ast = Ast::Sym(sym, *span);
                true
            }
            "str=!" => match &args[..] {
                [Ast::String(lhs, _), Ast::String(rhs, _)] => {
                    *ast = Ast::Bool(lhs == rhs, *span);
                    true
                }
                _ => false,
            },
            "if!" => match &mut args[..] {
                [Ast::Bool(condition, _), _, _] => {
                    let condition = *condition;
                    let if_false = args.pop().unwrap();
                    let if_true = args.pop().unwrap();
                    *ast = if condition { if_true } else { if_false };
                    true
                }
                _ => false,
            },
            "include-str" => match &args[..] {
                [Ast::String(path, ..)] => {
                    *ast =
                        Ast::String(fs::read_to_string(path).unwrap(), *span);
                    true
                }
                _ => false,
            },
            _ => false,
        })
    }

    fn use_inline_include(&mut self, ast: &mut Ast) -> Result<bool> {
        let Ast::Node(_, tail, _) = ast else {
            return Ok(false);
        };

        if !tail.iter().any(|item| item.is_the_function_call("include")) {
            return Ok(false);
        }

        *tail = mem::take(tail)
            .into_iter()
            .map(|item| {
                #[fancy_match]
                match &item {
                    Ast::Node(box Ast::Sym("include", ..), args, span) => {
                        self.include(args, *span)
                    }
                    _ => Ok(vec![item]),
                }
            })
            .collect::<Result<Vec<Vec<Ast>>>>()?
            .into_iter()
            .flatten()
            .collect();

        Ok(true)
    }

    fn use_inline_macros(&mut self, ast: &mut Ast) -> Result<bool> {
        let (macro_definition, def_span, args, span) = #[fancy_match]
        match ast {
            Ast::Node(
                box Ast::Node(
                    box Ast::Sym("macro", _),
                    macro_definition,
                    def_span,
                ),
                args,
                span,
            ) => (macro_definition, def_span, args, span),
            _ => return Ok(false),
        };
        let (macro_name, Macro::Function(func_macro)) =
            Macro::parse(mem::take(macro_definition), *def_span)?
        else {
            return Err(Box::new(
                Error::SymbolMacroInInlinePosition { span: *span }
            ));
        };

        let num_args = args.len();
        let num_params = func_macro.params.len();
        if num_args != num_params {
            return Err(Box::new(Error::FunctionMacroWrongArgCount {
                span: *span,
                macro_name,
                expected: num_params,
                got: num_args,
            }));
        }

        let mut bindings = HashMap::new();
        for (param, mut arg) in func_macro.params.iter().zip(mem::take(args)) {
            self.transform_deep(&mut arg)?;
            param.pattern_match(&macro_name, arg, &mut bindings)?;
        }
        *ast = interpolate(func_macro.body, &bindings)?;
        Ok(true)
    }

    fn include(&mut self, args: &[Ast], span: Span) -> Result<Vec<Ast>> {
        match args {
            [Ast::String(path, ..)] => {
                let source = fs::read_to_string(path).unwrap();
                let file = self.code_map.add_file(path.clone(), source.clone());
                let asts = program(Input {
                    input: Located::new(&source),
                    state: &file,
                })?;
                if self.opts.lint {
                    for ast in &asts {
                        lint_ast(ast, self.code_map);
                    }
                }
                Ok(asts)
            }
            _ => Err(Box::new(Error::InvalidArgsForInclude { span })),
        }
    }
}

fn interpolate(body: Ast, bindings: &HashMap<&str, Ast>) -> Result<Ast> {
    Ok(match body {
        Ast::Unquote(box Ast::Sym(var_name, span), ..) => bindings
            .get(&*var_name)
            .ok_or(Error::UnknownMetavariable { span, var_name })?
            .clone(),
        Ast::Unquote(unquoted, ..) => *unquoted,
        Ast::Num(..) | Ast::Bool(..) | Ast::String(..) | Ast::Sym(..) => body,
        Ast::Node(mut head, tail, span) => {
            *head = interpolate(*head, bindings)?;
            Ast::Node(
                head,
                tail.into_iter()
                    .map(|branch| interpolate(branch, bindings))
                    .collect::<Result<_>>()?,
                span,
            )
        }
    })
}

struct FunctionMacro {
    params: Vec<Parameter>,
    body: Ast,
}

#[derive(Clone)]
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
            _ => {
                Err(Box::new(Error::InvalidMacroParameter { span: ast.span() }))
            }
        }
    }

    fn pattern_match<'a>(
        &'a self,
        macro_name: &str,
        ast: Ast,
        bindings: &mut HashMap<&'a str, Ast>,
    ) -> Result<()> {
        match self {
            Self::Var(var) => {
                assert!(bindings.insert(var, ast).is_none());
                Ok(())
            }
            Self::Constructor(name, subparams, span) => match ast {
                Ast::Node(box Ast::Sym(sym, _), subtrees, _)
                    if sym == *name && subparams.len() == subtrees.len() =>
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
