use crate::{
    ast::{all_symbols, Ast},
    diagnostic::Result,
    ir::{expr::Expr, statement::Statement},
    uid::Uid,
};
use fancy_match::fancy_match;
use smol_str::SmolStr;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Procedure {
    pub params: Vec<Expr>,
    pub body: Statement,
    pub variables: HashSet<String>,
    pub lists: HashSet<String>,
}

impl Procedure {
    pub fn from_asts(args: Vec<Ast>) -> Result<(String, Self)> {
        // TODO: Error handling
        let mut args = args.into_iter();
        let signature = args.next().unwrap();
        let (name, params) = parse_signature(signature)?;
        let mut body = Vec::new();
        let mut variables = HashSet::new();
        let mut lists = HashSet::new();

        for stmt_or_decl in args {
            #[fancy_match]
            match stmt_or_decl {
                Ast::Node(box Ast::Sym("variables", ..), var_decls, ..) => {
                    variables.extend(all_symbols(var_decls));
                }
                Ast::Node(box Ast::Sym("lists", ..), list_decls, ..) => {
                    lists.extend(all_symbols(list_decls));
                }
                _ => body.push(Statement::from_ast(stmt_or_decl)?),
            }
        }

        Ok((
            name,
            Self {
                params,
                body: Statement::Do(body),
                variables,
                lists,
            },
        ))
    }

    pub fn optimize(&mut self) {
        self.body.optimize();
    }
}

fn parse_signature(ast: Ast) -> Result<(String, Vec<Expr>)> {
    // TODO: Error handling
    match ast {
        Ast::Node(box Ast::Sym(name, ..), params, ..) => {
            let params = params
                .into_iter()
                .map(Expr::from_ast)
                .collect::<Result<_>>()?;
            Ok((name, params))
        }
        _ => todo!(),
    }
}

pub struct CustomProcedure {
    pub params: Vec<(SmolStr, Uid)>,
}
