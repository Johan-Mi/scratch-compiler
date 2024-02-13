use lispfmt::Token;
use logos::Logos;
use std::fmt;

pub fn format_stdin_to_stdout() -> Result<(), std::io::Error> {
    let source_code = std::io::read_to_string(std::io::stdin())?;
    let mut tokens =
        TokenKind::lexer(&source_code).map(|kind| match kind.unwrap() {
            TokenKind::Atom(atom) => Token::Atom(atom),
            TokenKind::Unquote => Token::PrefixOperator(","),
            TokenKind::LParen => Token::LParen,
            TokenKind::RParen => Token::RParen,
            TokenKind::Comment(comment) => Token::Comment(comment),
            TokenKind::NewLine => Token::NewLine,
        });
    let output = lispfmt::format(&mut tokens, 2).unwrap();
    print!("{output}");
    Ok(())
}

struct Atom<'src>(&'src str);

impl fmt::Display for Atom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl lispfmt::Atom for Atom<'_> {
    fn width(&self) -> usize {
        unicode_width::UnicodeWidthStr::width(self.0)
    }

    fn custom_indentation(&self) -> Option<usize> {
        matches!(
            self.0,
            "macro"
                | "sprite"
                | "proc"
                | "if"
                | "when"
                | "unless"
                | "repeat"
                | "for"
                | "while"
                | "until"
        )
        .then_some(2)
    }
}

#[derive(Logos)]
#[logos(skip r"[ \t]+")]
enum TokenKind<'src> {
    #[regex(r#"[^ \t\n,();"]+"#, |lex| Atom(lex.slice()))]
    #[regex(r#""([^"\\\n]|\\.)*["\\]?"#, |lex| Atom(lex.slice()))]
    Atom(Atom<'src>),
    #[token(",")]
    Unquote,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r";.*\n?", |lex| lex.slice())]
    Comment(&'src str),
    #[token("\n")]
    NewLine,
}
