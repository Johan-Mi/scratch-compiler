use lispfmt::Token;
use logos::Logos;
use std::fmt;

pub fn format_stdin_to_stdout() -> Result<(), std::io::Error> {
    let source_code = std::io::read_to_string(std::io::stdin())?;
    let mut tokens =
        WrappedToken::lexer(&source_code).map(|token| match token.unwrap() {
            WrappedToken::Of(inner) => inner,
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
enum WrappedToken<'src> {
    #[regex(r#"[^ \t\n,();"]+"#, |lex| Token::Atom(Atom(lex.slice())))]
    #[regex(r#""([^"\\\n]|\\.)*["\\]?"#, |lex| Token::Atom(Atom(lex.slice())))]
    #[token(",", |_| Token::PrefixOperator(","))]
    #[token("(", |_| Token::LParen)]
    #[token(")", |_| Token::RParen)]
    #[regex(r";.*\n?", |lex| Token::Comment(lex.slice()))]
    #[token("\n", |_| Token::NewLine)]
    Of(Token<'src, Atom<'src>>),
}
