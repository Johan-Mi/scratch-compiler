use crate::{ast::Ast, span::Span};
use codespan::FileId;
use std::borrow::Cow;
use winnow::{
    branch::alt,
    bytes::{any, one_of, take_till1, take_while1, take_while_m_n},
    character::{digit1, float, hex_digit1, multispace1, oct_digit1},
    combinator::{fail, not, opt, peek, success},
    dispatch,
    error::ParseError,
    multi::{count, many0},
    sequence::{delimited, preceded, separated_pair, terminated},
    stream::{Located, Stateful},
    FinishIResult, IResult, Parser,
};

pub type Input<'a> = Stateful<Located<&'a str>, FileId>;
type Error<'a> = winnow::error::Error<Input<'a>>;

pub fn program(input: Input) -> crate::diagnostic::Result<Vec<Ast>> {
    Ok(preceded(ws, many0(terminated(expr, ws)))(input)
        .finish()
        .map_err(|err| crate::diagnostic::Error::Parse(format!("{err:?}")))?)
}

fn expr(input: Input) -> IResult<Input, Ast> {
    alt((number, boolean, string, sym, node, unquote))(input)
}

fn number(input: Input) -> IResult<Input, Ast> {
    let hex = based(16, "xX", hex_digit1);
    let binary = based(2, "bB", take_while1("01"));
    let octal = based(8, "oO", oct_digit1);

    spanned(terminated(
        alt((hex, binary, octal, float)),
        not(sym_non_first_char),
    ))
    .map(|(span, num)| Ast::Num(num, span))
    .parse_next(input)
}

fn based<'a>(
    base: u32,
    prefix: &'static str,
    digitp: impl Parser<Input<'a>, &'a str, Error<'a>>,
) -> impl Parser<Input<'a>, f64, Error<'a>> {
    separated_pair(sign, ('0', one_of(prefix)), digitp).map_res(
        move |(sgn, digits)| {
            let sgn = Cow::Borrowed(sgn.unwrap_or_default());
            let with_sign = sgn + digits;
            i64::from_str_radix(&with_sign, base).map(|n| n as f64)
        },
    )
}

fn sign(input: Input) -> IResult<Input, Option<&str>> {
    opt(one_of("+-").recognize())(input)
}

fn hex_digit(input: Input) -> IResult<Input, char> {
    one_of(|c: char| c.is_ascii_hexdigit())(input)
}

fn boolean(input: Input) -> IResult<Input, Ast> {
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

fn string(input: Input) -> IResult<Input, Ast> {
    let normal = take_till1("\"\\\n").map(Cow::Borrowed);
    let null = terminated('0', not(peek(digit1))).value(Cow::Borrowed("\0"));
    let character_escape_sequence = dispatch! {any;
        '"' => success("\""),
        '\'' => success("'"),
        '\\' => success("\\"),
        'n' => success("\n"),
        't' => success("\t"),
        'r' => success("\r"),
        'b' => success("\x08"),
        'f' => success("\x0c"),
        'v' => success("\x11"),
        _ => fail,
    }
    .map(Cow::Borrowed);

    let hex_escape_sequence =
        preceded('x', count::<_, _, (), _, _>(hex_digit, 2).recognize());
    let hex4digits = count::<_, _, (), _, _>(hex_digit, 4).recognize();
    let bracketed_unicode = delimited(
        '{',
        take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit()),
        '}',
    );
    let unicode_escape_sequence =
        preceded('u', alt((hex4digits, bracketed_unicode)));
    let escape_sequence = preceded(
        '\\',
        alt((
            character_escape_sequence,
            null,
            alt((hex_escape_sequence, unicode_escape_sequence))
                .map_res(|digits| u32::from_str_radix(digits, 16))
                .verify_map(|c| {
                    char::from_u32(c).map(String::from).map(Cow::Owned)
                }),
        )),
    );
    let string_char = alt((normal, escape_sequence));

    spanned(delimited('"', many0(string_char), '"'))
        .map(|(span, strs): (_, Vec<_>)| Ast::String(strs.concat(), span))
        .parse_next(input)
}

fn sym_first_char(input: Input) -> IResult<Input, char> {
    alt((one_of(char::is_alphabetic), one_of("!$%&*+-./:<=>?@^_~[]")))(input)
}

fn sym_non_first_char(input: Input) -> IResult<Input, ()> {
    alt((sym_first_char.void(), digit1.void()))(input)
}

fn sym(input: Input) -> IResult<Input, Ast> {
    spanned(
        (sym_first_char, many0::<_, _, (), _, _>(sym_non_first_char))
            .recognize(),
    )
    .map(|(span, sym)| Ast::Sym(sym.to_owned(), span))
    .parse_next(input)
}

fn node(input: Input) -> IResult<Input, Ast> {
    let content = (expr, many0(preceded(ws, expr)));
    spanned(delimited(('(', ws), content, (ws, ')')))
        .map(|(span, (first, rest))| Ast::Node(Box::new(first), rest, span))
        .parse_next(input)
}

fn unquote(input: Input) -> IResult<Input, Ast> {
    spanned(preceded((',', ws), expr))
        .map(|(span, ast)| Ast::Unquote(Box::new(ast), span))
        .parse_next(input)
}

fn eol_comment(input: Input) -> IResult<Input, ()> {
    (';', opt(take_till1("\n\r"))).void().parse_next(input)
}

fn ws(input: Input) -> IResult<Input, ()> {
    many0(alt((multispace1.void(), eol_comment))).parse_next(input)
}

fn spanned<'a, O, E: ParseError<Input<'a>>, F>(
    mut parser: F,
) -> impl Parser<Input<'a>, (Span, O), E>
where
    F: Parser<Input<'a>, O, E>,
{
    move |input: Input<'a>| {
        parser
            .by_ref()
            .with_span()
            .map(|(parsed, range)| {
                (
                    Span {
                        position: (range.start as u32..range.end as u32).into(),
                        file: input.state,
                    },
                    parsed,
                )
            })
            .parse_next(input)
    }
}
