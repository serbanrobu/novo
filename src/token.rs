use std::{
    fmt::{self, Write},
    num::ParseFloatError,
};

use logos::{Lexer, Logos, Span};
use smol_str::SmolStr;
use thiserror::Error;

#[derive(Default, Debug, Clone, Error, PartialEq, miette::Diagnostic)]
pub enum StringError {
    #[error("unknown character escape: `{char}`")]
    #[diagnostic(code(novo::token::StringError::UnknownCharacterEscape))]
    UnknownCharacterEscape {
        char: char,
        #[label("unknown character escape")]
        span: Span,
    },
    #[error("unexpected end")]
    #[diagnostic(code(novo::token::StringError::UnexpectedEnd))]
    UnexpectedEnd,
    #[default]
    #[error("invalid token")]
    #[diagnostic(code(novo::token::StringError::InvalidToken))]
    InvalidToken,
}

#[derive(Debug, Clone, Error, PartialEq, miette::Diagnostic)]
pub enum LexicalError {
    #[error(transparent)]
    #[diagnostic(code(novo::token::LexicalError::ParseFloatError))]
    ParseFloatError(#[from] ParseFloatError),
    #[error(transparent)]
    #[diagnostic(transparent)]
    StringError(#[from] StringError),
    #[error("unterminated string")]
    #[diagnostic(code(novo::token::LexicalError::UnterminatedString))]
    UnterminatedString {
        #[label]
        span: Span,
    },
    #[error("unterminated comment")]
    #[diagnostic(code(novo::token::LexicalError::UnterminatedComment))]
    UnterminatedComment {
        #[label]
        span: Span,
    },
    #[error("invalid token")]
    #[diagnostic(code(novo::token::LexicalError::InvalidToken))]
    InvalidToken {
        #[label]
        span: Span,
    },
}

impl Default for LexicalError {
    fn default() -> Self {
        Self::InvalidToken {
            span: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Eq, Logos, PartialEq)]
#[logos(skip "#")]
#[logos(skip "[^#]+")]
enum CommentToken {
    #[token("#}")]
    End,
}

#[derive(Clone, Debug, Eq, Logos, PartialEq)]
#[logos(error = StringError)]
enum StringToken<'source> {
    #[token("'")]
    End,
    #[token("\\", escape_callback)]
    Escape(char),
    #[regex(r"[^'\\]+")]
    Slice(&'source str),
}

#[derive(Clone, Debug, Eq, Logos, PartialEq)]
#[logos(error(LexicalError, lexical_error_callback))]
#[logos(skip(r"\{#", comment_callback))]
#[logos(skip r"\{")]
#[logos(skip "[^{]+")]
enum TextToken {
    #[token("{%")]
    End,
}

#[derive(Clone, Debug, Logos, PartialEq)]
#[logos(error(LexicalError, lexical_error_callback))]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip("%}", text_callback))]
#[logos(skip(r"\{#", comment_callback))]
pub enum Token {
    #[token("and")]
    And,
    #[token("|")]
    Bar,
    #[token(",")]
    Comma,
    #[token("else")]
    Else,
    #[token("elseif")]
    Elseif,
    #[token("endif")]
    Endif,
    #[token("=")]
    Eq,
    #[token("==")]
    EqEq,
    #[token(">=")]
    Ge,
    #[token(">")]
    Gt,
    #[regex("[A-Z_a-z][0-9A-Z_a-z]*", ident_callback)]
    Ident(SmolStr),
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("(")]
    LParen,
    #[token("<=")]
    Le,
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    LitBoolean(bool),
    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", lit_number_callback)]
    LitNumber(f64),
    #[token("'", lit_string_callback)]
    LitString(SmolStr),
    #[token("<")]
    Lt,
    #[token("-")]
    Minus,
    #[token("!=")]
    Ne,
    #[token("or")]
    Or,
    #[token("+")]
    Plus,
    #[token(")")]
    RParen,
    #[token("set")]
    Set,
    #[token("/")]
    Slash,
    #[token("*")]
    Star,
    #[token("~")]
    Tilde,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::And => f.write_str("and"),
            Self::Bar => f.write_char('|'),
            Self::Comma => f.write_char(','),
            Self::Else => f.write_str("else"),
            Self::Elseif => f.write_str("elseif"),
            Self::Endif => f.write_str("endif"),
            Self::Eq => f.write_char('='),
            Self::EqEq => f.write_str("=="),
            Self::Ge => f.write_str(">="),
            Self::Gt => f.write_char('>'),
            Self::Ident(i) => f.write_str(i),
            Self::If => f.write_str("if"),
            Self::In => f.write_str("in"),
            Self::LParen => f.write_char('('),
            Self::Le => f.write_str("<="),
            Self::LitBoolean(b) => b.fmt(f),
            Self::LitNumber(n) => n.fmt(f),
            Self::LitString(s) => write!(f, "{:?}", s),
            Self::Lt => f.write_char('<'),
            Self::Minus => f.write_char('-'),
            Self::Ne => f.write_str("!="),
            Self::Or => f.write_str("or"),
            Self::Plus => f.write_char('+'),
            Self::RParen => f.write_char(')'),
            Self::Set => f.write_str("set"),
            Self::Slash => f.write_char('/'),
            Self::Star => f.write_char('*'),
            Self::Tilde => f.write_char('~'),
        }
    }
}

fn lexical_error_callback<'source, T>(lex: &mut Lexer<'source, T>) -> LexicalError
where
    T: Logos<'source>,
{
    LexicalError::InvalidToken { span: lex.span() }
}

fn ident_callback<'source>(lex: &mut Lexer<'source, Token>) -> SmolStr {
    lex.slice().into()
}

fn escape_callback<'source>(
    lex: &mut Lexer<'source, StringToken<'source>>,
) -> Result<char, StringError> {
    let char = lex
        .remainder()
        .chars()
        .next()
        .ok_or(StringError::UnexpectedEnd)?;

    let start = lex.span().end;
    lex.bump(char.len_utf8());

    match char {
        '\'' => Ok('\''),
        'n' => Ok('\n'),
        'r' => Ok('\r'),
        't' => Ok('\t'),
        _ => Err(StringError::UnknownCharacterEscape {
            char,
            span: start..lex.span().end,
        }),
    }
}

fn lit_number_callback<'source>(lex: &mut Lexer<'source, Token>) -> Result<f64, ParseFloatError> {
    lex.slice().parse()
}

fn lit_string_callback<'source>(lex: &mut Lexer<'source, Token>) -> Result<SmolStr, LexicalError> {
    let mut string_lexer = lex.clone().morph::<StringToken<'source>>();
    let mut string = String::new();

    loop {
        let token = string_lexer
            .next()
            .ok_or(LexicalError::UnterminatedString {
                span: lex.span().start..string_lexer.span().end,
            })??;

        match token {
            StringToken::End => break,
            StringToken::Escape(c) => string.push(c),
            StringToken::Slice(s) => string.push_str(s),
        }
    }

    lex.bump(string_lexer.span().end - lex.span().end);
    Ok(string.into())
}

pub fn text_callback<'source>(lex: &mut Lexer<'source, Token>) {
    let mut text_lexer = lex.clone().morph::<TextToken>();

    text_lexer
        .next()
        .transpose()
        .expect("should not fail lexing text");

    *lex = text_lexer.morph();
}

fn comment_callback<'source, T>(lex: &mut Lexer<'source, T>) -> Result<(), LexicalError>
where
    T: Clone + Logos<'source, Source = str>,
    <T as Logos<'source>>::Extras: Clone,
    <T as Logos<'source>>::Extras: From<()>,
    (): From<<T as Logos<'source>>::Extras>,
{
    let mut comment_lexer = lex.clone().morph::<CommentToken>();

    comment_lexer
        .next()
        .ok_or(LexicalError::UnterminatedComment {
            span: lex.span().start..comment_lexer.span().end,
        })?
        .expect("should not fail lexing comment");

    *lex = comment_lexer.morph();
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;

    #[test]
    fn test_keywords_and_symbols() {
        let input = "if else elseif endif and or in set = == != < <= > >= + - * / , | ~ ( )";
        let tokens: Vec<_> = Token::lexer(input).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Token::If),
                Ok(Token::Else),
                Ok(Token::Elseif),
                Ok(Token::Endif),
                Ok(Token::And),
                Ok(Token::Or),
                Ok(Token::In),
                Ok(Token::Set),
                Ok(Token::Eq),
                Ok(Token::EqEq),
                Ok(Token::Ne),
                Ok(Token::Lt),
                Ok(Token::Le),
                Ok(Token::Gt),
                Ok(Token::Ge),
                Ok(Token::Plus),
                Ok(Token::Minus),
                Ok(Token::Star),
                Ok(Token::Slash),
                Ok(Token::Comma),
                Ok(Token::Bar),
                Ok(Token::Tilde),
                Ok(Token::LParen),
                Ok(Token::RParen),
            ]
        );
    }

    #[test]
    fn test_identifier() {
        let input = "foo Bar123 _baz";
        let tokens: Vec<_> = Token::lexer(input).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Token::Ident("foo".into())),
                Ok(Token::Ident("Bar123".into())),
                Ok(Token::Ident("_baz".into())),
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let input = "42 3.15 -12 1e10 -2.5E-3";
        let tokens: Vec<_> = Token::lexer(input).collect();

        assert_eq!(
            tokens,
            vec![
                Ok(Token::LitNumber(42.0)),
                Ok(Token::LitNumber(3.15)),
                Ok(Token::LitNumber(-12.0)),
                Ok(Token::LitNumber(1e10)),
                Ok(Token::LitNumber(-2.5e-3)),
            ]
        );
    }

    #[test]
    fn test_string_literal_simple() {
        let input = "'hello'";
        let tokens: Vec<_> = Token::lexer(input).collect();
        assert_eq!(tokens, vec![Ok(Token::LitString("hello".into()))]);
    }

    #[test]
    fn test_string_with_escape() {
        let input = "'foo\\'bar\\n'";
        let tokens: Vec<_> = Token::lexer(input).collect();
        assert_eq!(tokens, vec![Ok(Token::LitString("foo'bar\n".into()))]);
    }

    #[test]
    fn test_string_unterminated() {
        let input = "'unterminated";
        let mut lexer = Token::lexer(input);
        let tok = lexer.next();

        assert!(matches!(
            tok,
            Some(Err(LexicalError::UnterminatedString { .. }))
        ));
    }

    #[test]
    fn test_string_invalid_escape() {
        let input = "'foo\\xbar'";
        let mut lexer = Token::lexer(input);
        let tok = lexer.next();

        assert!(matches!(
            tok,
            Some(Err(LexicalError::StringError(
                StringError::UnknownCharacterEscape { .. }
            )))
        ));
    }

    #[test]
    fn test_comment_skip() {
        let input = "{# this is a comment #}{% if %}";
        let mut lexer = Token::lexer(input);
        text_callback(&mut lexer);
        let tokens: Vec<_> = lexer.collect();
        assert_eq!(tokens, vec![Ok(Token::If)]);
    }

    #[test]
    fn test_comment_unterminated() {
        let input = "{# unterminated";
        let mut lexer = Token::lexer(input);
        let tok = lexer.next();

        assert!(matches!(
            tok,
            Some(Err(LexicalError::UnterminatedComment { .. }))
        ));
    }

    #[test]
    fn test_invalid_token() {
        let input = "@";
        let mut lexer = Token::lexer(input);
        let tok = lexer.next();
        assert!(matches!(tok, Some(Err(LexicalError::InvalidToken { .. }))));
    }
}
