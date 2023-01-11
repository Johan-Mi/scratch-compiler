pub mod input;

use self::input::{spanned, Input};
use crate::ast::Ast;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while_m_n},
    character::complete::{
        char, digit1, hex_digit1, multispace1, oct_digit1, one_of, satisfy,
    },
    combinator::{
        all_consuming, map_opt, map_res, not, opt, peek, recognize, value,
    },
    error::ParseError,
    multi::{count, many0},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult, Parser,
};
use std::borrow::Cow;

pub fn program(input: Input) -> IResult<Input, Vec<Ast>> {
    all_consuming(preceded(ws, many0(terminated(expr, ws))))(input)
}

fn expr(input: Input) -> IResult<Input, Ast> {
    alt((number, boolean, string, sym, node, unquote))(input)
}

fn number(input: Input) -> IResult<Input, Ast> {
    let hex = based(16, "xX", hex_digit1);
    let binary = based(2, "bB", is_a("01"));
    let octal = based(8, "oO", oct_digit1);

    spanned(terminated(
        alt((hex, binary, octal, double)),
        not(sym_non_first_char),
    ))
    .map(|(span, num)| Ast::Num(num, span))
    .parse(input)
}

fn based<'a>(
    base: u32,
    prefix: &'static str,
    digitp: impl FnMut(Input<'a>) -> IResult<Input<'a>, Input<'a>>,
) -> impl FnMut(Input<'a>) -> IResult<Input<'a>, f64> {
    map_res(
        separated_pair(sign, pair(char('0'), one_of(prefix)), digitp),
        move |(sgn, digits)| {
            let sgn = Cow::Borrowed(sgn.unwrap_or_default());
            let with_sign = sgn + digits.to_str();
            i64::from_str_radix(&with_sign, base).map(|n| n as f64)
        },
    )
}

fn sign(input: Input) -> IResult<Input, Option<&str>> {
    opt(recognize(one_of("+-")).map(Input::to_str)).parse(input)
}

fn hex_digit(input: Input) -> IResult<Input, char> {
    satisfy(|c| c.is_ascii_hexdigit())(input)
}

fn boolean(input: Input) -> IResult<Input, Ast> {
    terminated(
        spanned(alt((value(true, tag("true")), value(false, tag("false"))))),
        not(sym_non_first_char),
    )
    .map(|(span, b)| Ast::Bool(b, span))
    .parse(input)
}

fn string(input: Input) -> IResult<Input, Ast> {
    let normal = is_not("\"\\\n").map(Input::to_str).map(Cow::Borrowed);
    let null = value(
        Cow::Borrowed("\0"),
        terminated(char('0'), not(peek(digit1))),
    );
    let character_escape_sequence = alt((
        value("\"", char('"')),
        value("'", char('\'')),
        value("\\", char('\\')),
        value("\n", char('n')),
        value("\t", char('t')),
        value("\r", char('r')),
        value("\x08", char('b')),
        value("\x0c", char('f')),
        value("\x11", char('v')),
    ))
    .map(Cow::Borrowed);

    let hex_escape_sequence =
        preceded(char('x'), recognize(count(hex_digit, 2)));
    let hex4digits = recognize(count(hex_digit, 4));
    let bracketed_unicode = delimited(
        char('{'),
        take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit()),
        char('}'),
    );
    let unicode_escape_sequence =
        preceded(char('u'), alt((hex4digits, bracketed_unicode)));
    let escape_sequence = preceded(
        char('\\'),
        alt((
            character_escape_sequence,
            null,
            map_opt(
                map_res(
                    alt((hex_escape_sequence, unicode_escape_sequence)),
                    |digits| u32::from_str_radix(digits.to_str(), 16),
                ),
                |c| char::from_u32(c).map(String::from).map(Cow::Owned),
            ),
        )),
    );
    let string_char = alt((normal, escape_sequence));

    spanned(delimited(char('"'), many0(string_char), char('"')))
        .map(|(span, strs)| Ast::String(strs.concat(), span))
        .parse(input)
}

fn sym_first_char(input: Input) -> IResult<Input, char> {
    alt((satisfy(char::is_alphabetic), one_of("!$%&*+-./:<=>?@^_~[]")))(input)
}

fn sym_non_first_char(input: Input) -> IResult<Input, ()> {
    alt((unit(sym_first_char), unit(digit1)))(input)
}

fn sym(input: Input) -> IResult<Input, Ast> {
    spanned(recognize(pair(sym_first_char, many0(sym_non_first_char))))
        .map(|(span, sym)| Ast::Sym(sym.to_str().to_owned(), span))
        .parse(input)
}

fn node(input: Input) -> IResult<Input, Ast> {
    let content = pair(expr, many0(preceded(ws, expr)));
    spanned(delimited(pair(char('('), ws), content, pair(ws, char(')'))))
        .map(|(span, (first, rest))| Ast::Node(Box::new(first), rest, span))
        .parse(input)
}

fn unquote(input: Input) -> IResult<Input, Ast> {
    spanned(preceded(pair(char(','), ws), expr))
        .map(|(span, ast)| Ast::Unquote(Box::new(ast), span))
        .parse(input)
}

fn eol_comment(input: Input) -> IResult<Input, ()> {
    unit(pair(char(';'), opt(is_not("\n\r"))))(input)
}

fn ws(input: Input) -> IResult<Input, ()> {
    unit(many0(alt((unit(multispace1), eol_comment))))(input)
}

pub fn unit<I, O, E: ParseError<I>, F>(
    parser: F,
) -> impl FnMut(I) -> IResult<I, (), E>
where
    F: Parser<I, O, E>,
{
    value((), parser)
}
