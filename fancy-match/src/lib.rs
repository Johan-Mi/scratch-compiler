#![feature(box_patterns)]

use proc_macro::TokenStream;
use quote::{format_ident, ToTokens};
use syn::{
    parse_macro_input, parse_quote,
    visit_mut::{self, VisitMut},
    Expr, ExprLit, ExprMatch, Ident, Lit, Pat, PatLit,
};

#[proc_macro_attribute]
pub fn fancy_match(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut match_expr = parse_macro_input!(item as ExprMatch);

    for arm in match_expr.arms.iter_mut() {
        rewrite_arm(arm);
    }

    match_expr.into_token_stream().into()
}

fn rewrite_arm(arm: &mut syn::Arm) {
    let mut guards = Vec::new();
    let mut visitor = PatVisitor {
        guards: &mut guards,
        id: 0,
    };
    visitor.visit_pat_mut(&mut arm.pat);

    if let Some(merged_guard) = and_all(guards) {
        arm.guard = Some((
            parse_quote! { if },
            Box::new(match &arm.guard {
                Some((_, existing_guard)) => {
                    and_two(&merged_guard, existing_guard)
                }
                None => merged_guard,
            }),
        ))
    }
}

fn and_two(left: &Expr, right: &Expr) -> Expr {
    parse_quote! { #left && #right }
}

fn and_all(conds: Vec<Expr>) -> Option<Expr> {
    conds.into_iter().fold(None, |acc, cond| {
        Some(match acc {
            Some(acc) => and_two(&acc, &cond),
            None => cond,
        })
    })
}

struct PatVisitor<'a> {
    guards: &'a mut Vec<Expr>,
    id: u32,
}

impl<'a> PatVisitor<'a> {
    fn new_ident(&mut self) -> Ident {
        self.id += 1;
        format_ident!("__fancy_match_{}", self.id)
    }
}

impl<'a> VisitMut for PatVisitor<'a> {
    fn visit_pat_mut(&mut self, pat: &mut Pat) {
        let lit_str = match pat {
            Pat::Lit(PatLit {
                expr:
                    box Expr::Lit(ExprLit {
                        lit: lit_str @ Lit::Str(_),
                        ..
                    }),
                ..
            }) => lit_str,
            _ => return visit_mut::visit_pat_mut(self, pat),
        };
        let ident = self.new_ident();
        self.guards.push(parse_quote! { #ident == #lit_str });
        *pat = parse_quote! { #ident };
    }
}
