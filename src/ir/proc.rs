use crate::{
    ast::{all_symbols, Ast},
    diagnostic::Result,
    ir::{expr::Expr, statement::Statement},
    uid::Uid,
};
use codemap::Span;
use ecow::EcoString;
use std::collections::HashSet;

#[derive(Debug)]
pub struct Procedure {
    pub params: Vec<(Expr, Span)>,
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
            match stmt_or_decl {
                Ast::Node(box Ast::Sym("variables", ..), var_decls, ..) => {
                    variables.extend(all_symbols(var_decls).unwrap());
                }
                Ast::Node(box Ast::Sym("lists", ..), list_decls, ..) => {
                    lists.extend(all_symbols(list_decls).unwrap());
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
        crate::optimize::stmt(&mut self.body);
    }
}

fn parse_signature(ast: Ast) -> Result<(String, Vec<(Expr, Span)>)> {
    // TODO: Error handling
    let Ast::Node(box Ast::Sym(name, ..), params, ..) = ast else {
        todo!();
    };
    let params = params
        .into_iter()
        .map(|param| {
            let span = param.span();
            Ok((Expr::from_ast(param)?, span))
        })
        .collect::<Result<_>>()?;
    Ok((name, params))
}

pub struct CustomProcedure {
    pub params: Vec<(EcoString, Uid)>,
}
