use crate::ast::Ast;
use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, take_while_m_n},
    character::complete::{
        char, digit0, digit1, hex_digit1, multispace1, oct_digit1, one_of,
        satisfy,
    },
    combinator::{
        all_consuming, map_opt, map_res, not, opt, peek, recognize, value,
    },
    error::ParseError,
    multi::{count, many0, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult, Parser,
};
use std::borrow::Cow;

pub(crate) fn program(input: &str) -> IResult<&str, Vec<Ast>> {
    all_consuming(delimited(ws, separated_list0(ws, expr), ws))(input)
}

fn expr(input: &str) -> IResult<&str, Ast> {
    alt((number, string, sym, node, unquote))(input)
}

fn number(input: &str) -> IResult<&str, Ast> {
    let exponent = opt(tuple((one_of("eE"), sign, digit1)));

    let decimal = map_res(
        recognize(tuple((
            sign,
            alt((
                unit(preceded(char('.'), digit1)),
                unit(pair(digit1, opt(preceded(char('.'), digit0)))),
            )),
            exponent,
        ))),
        str::parse::<f64>,
    );

    let hex = based(16, "xX", hex_digit1);
    let binary = based(2, "bB", is_a("01"));
    let octal = based(8, "oO", oct_digit1);

    terminated(alt((hex, binary, octal, decimal)), not(sym_non_first_char))
        .map(Ast::Num)
        .parse(input)
}

fn based<'a>(
    base: u32,
    prefix: &'static str,
    digitp: impl FnMut(&'a str) -> IResult<&'a str, &'a str>,
) -> impl FnMut(&'a str) -> IResult<&'a str, f64> {
    map_res(
        separated_pair(sign, pair(char('0'), one_of(prefix)), digitp),
        move |(sgn, digits): (_, &str)| {
            let sgn = Cow::Borrowed(sgn.unwrap_or_default());
            let with_sign = sgn + digits;
            i64::from_str_radix(&with_sign, base).map(|n| n as f64)
        },
    )
}

fn sign(input: &str) -> IResult<&str, Option<&str>> {
    opt(recognize(one_of("+-")))(input)
}

fn string(input: &str) -> IResult<&str, Ast> {
    let normal = is_not("\"\\\n").map(Cow::Borrowed);
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

    let hex_digit = |inp| satisfy(|c| c.is_ascii_hexdigit())(inp);
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
                    |digits| u32::from_str_radix(digits, 16),
                ),
                |c| char::from_u32(c).map(String::from).map(Cow::Owned),
            ),
        )),
    );
    let string_char = alt((normal, escape_sequence));

    delimited(char('"'), many0(string_char), char('"'))
        .map(|strs| Ast::String(strs.concat()))
        .parse(input)
}

fn sym_first_char(input: &str) -> IResult<&str, char> {
    alt((satisfy(char::is_alphabetic), one_of("!$%&*+-./:<=>?@^_~[]")))(input)
}

fn sym_non_first_char(input: &str) -> IResult<&str, ()> {
    alt((unit(sym_first_char), unit(digit1)))(input)
}

fn sym(input: &str) -> IResult<&str, Ast> {
    recognize(pair(sym_first_char, many0(sym_non_first_char)))
        .map(|sym: &str| Ast::Sym(sym.to_owned()))
        .parse(input)
}

fn node(input: &str) -> IResult<&str, Ast> {
    let content = pair(expr, many0(preceded(ws, expr)))
        .map(|(first, rest)| Ast::Node(Box::new(first), rest));

    delimited(pair(char('('), ws), content, pair(ws, char(')')))(input)
}

fn unquote(input: &str) -> IResult<&str, Ast> {
    preceded(pair(char(','), ws), expr)
        .map(|ast| Ast::Unquote(Box::new(ast)))
        .parse(input)
}

fn eol_comment(input: &str) -> IResult<&str, ()> {
    unit(pair(char(';'), is_not("\n\r")))(input)
}

fn ws(input: &str) -> IResult<&str, ()> {
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
