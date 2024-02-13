use crate::ast::Ast;
use codemap::{File, Span};
use std::borrow::Cow;
use winnow::{
    ascii::{digit1, float, hex_digit1, multispace1, oct_digit1},
    combinator::{
        alt, delimited, empty, fail, not, opt, preceded, repeat,
        separated_pair, terminated,
    },
    dispatch,
    error::{ContextError as Error, ParserError},
    token::{any, one_of, take_till, take_while},
    Located, PResult, Parser, Stateful,
};

pub type Input<'a> = Stateful<Located<&'a str>, &'a File>;

pub fn program(input: Input) -> crate::diagnostic::Result<Vec<Ast>> {
    Ok(preceded(ws, repeat(0.., terminated(expr, ws)))
        .parse(input)
        .map_err(|err| crate::diagnostic::Error::Parse(format!("{err:?}")))?)
}

fn expr(input: &mut Input) -> PResult<Ast> {
    alt((number, boolean, string, sym, node, unquote)).parse_next(input)
}

fn number(input: &mut Input) -> PResult<Ast> {
    let hex = based(16, &['x', 'X'], hex_digit1);
    let binary = based(2, &['b', 'B'], take_while(1.., ['0', '1']));
    let octal = based(8, &['o', 'O'], oct_digit1);

    spanned(terminated(
        alt((hex, binary, octal, float)),
        not(sym_non_first_char),
    ))
    .map(|(span, num)| Ast::Num(num, span))
    .parse_next(input)
}

fn based<'a>(
    base: u32,
    prefix: &'static [char],
    digitp: impl Parser<Input<'a>, &'a str, Error>,
) -> impl Parser<Input<'a>, f64, Error> {
    separated_pair(sign, ('0', one_of(prefix)), digitp).try_map(
        move |(sign, digits)| {
            i64::from_str_radix(digits, base)
                .map(|n| n as f64 * if sign == Some('-') { -1.0 } else { 1.0 })
        },
    )
}

fn sign(input: &mut Input) -> PResult<Option<char>> {
    opt(one_of(['+', '-'])).parse_next(input)
}

fn hex_digit(input: &mut Input) -> PResult<char> {
    one_of(|c: char| c.is_ascii_hexdigit()).parse_next(input)
}

fn boolean(input: &mut Input) -> PResult<Ast> {
    terminated(
        spanned(alt((
            Parser::<_, _, Error>::value("true", true),
            Parser::<_, _, Error>::value("false", false),
        ))),
        not(sym_non_first_char),
    )
    .map(|(span, b)| Ast::Bool(b, span))
    .parse_next(input)
}

fn string(input: &mut Input) -> PResult<Ast> {
    let normal = take_till(1.., ['\"', '\\', '\n']).map(Cow::Borrowed);
    let null = terminated('0', not(digit1)).value(Cow::Borrowed("\0"));
    let character_escape_sequence = dispatch! {any;
        '"' => empty.value("\""),
        '\'' => empty.value("'"),
        '\\' => empty.value("\\"),
        'n' => empty.value("\n"),
        't' => empty.value("\t"),
        'r' => empty.value("\r"),
        'b' => empty.value("\x08"),
        'f' => empty.value("\x0c"),
        'v' => empty.value("\x11"),
        _ => fail,
    }
    .map(Cow::Borrowed);

    let hex_escape_sequence =
        preceded('x', repeat::<_, _, (), _, _>(2, hex_digit).recognize());
    let hex4digits = repeat::<_, _, (), _, _>(4, hex_digit).recognize();
    let bracketed_unicode =
        delimited('{', take_while(1..=6, |c: char| c.is_ascii_hexdigit()), '}');
    let unicode_escape_sequence =
        preceded('u', alt((hex4digits, bracketed_unicode)));
    let escape_sequence = preceded(
        '\\',
        alt((
            character_escape_sequence,
            null,
            alt((hex_escape_sequence, unicode_escape_sequence))
                .try_map(|digits| u32::from_str_radix(digits, 16))
                .verify_map(|c| {
                    char::from_u32(c).map(String::from).map(Cow::Owned)
                }),
        )),
    );
    let string_char = alt((normal, escape_sequence));

    spanned(delimited('"', repeat(0.., string_char), '"'))
        .map(|(span, strs): (_, Vec<_>)| Ast::String(strs.concat(), span))
        .parse_next(input)
}

fn sym_first_char(input: &mut Input) -> PResult<char> {
    one_of((
        char::is_alphabetic,
        [
            '!', '$', '%', '&', '*', '+', '-', '.', '/', ':', '<', '=', '>',
            '?', '@', '^', '_', '~', '[', ']',
        ],
    ))
    .parse_next(input)
}

fn sym_non_first_char(input: &mut Input) -> PResult<()> {
    alt((sym_first_char.void(), digit1.void())).parse_next(input)
}

fn sym(input: &mut Input) -> PResult<Ast> {
    spanned(
        (
            sym_first_char,
            repeat::<_, _, (), _, _>(0.., sym_non_first_char),
        )
            .recognize(),
    )
    .map(|(span, sym)| Ast::Sym(sym.to_owned(), span))
    .parse_next(input)
}

fn node(input: &mut Input) -> PResult<Ast> {
    let content = (expr, repeat(0.., preceded(ws, expr)));
    spanned(delimited(('(', ws), content, (ws, ')')))
        .map(|(span, (first, rest))| Ast::Node(Box::new(first), rest, span))
        .parse_next(input)
}

fn unquote(input: &mut Input) -> PResult<Ast> {
    spanned(preceded((',', ws), expr))
        .map(|(span, ast)| Ast::Unquote(Box::new(ast), span))
        .parse_next(input)
}

fn eol_comment(input: &mut Input) -> PResult<()> {
    (';', take_till(0.., '\n')).void().parse_next(input)
}

fn ws(input: &mut Input) -> PResult<()> {
    repeat(0.., alt((multispace1.void(), eol_comment))).parse_next(input)
}

fn spanned<'a, O, E: ParserError<Input<'a>>, F>(
    mut parser: F,
) -> impl Parser<Input<'a>, (Span, O), E>
where
    F: Parser<Input<'a>, O, E>,
{
    move |input: &mut Input<'a>| {
        parser
            .by_ref()
            .with_span()
            .map(|(parsed, range)| {
                (
                    input.state.span.subspan(
                        range.start.try_into().unwrap(),
                        range.end.try_into().unwrap(),
                    ),
                    parsed,
                )
            })
            .parse_next(input)
    }
}
