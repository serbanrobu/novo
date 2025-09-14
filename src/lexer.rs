use logos::{Logos, SpannedIter};

use crate::token::{LexicalError, Token, text_callback};

pub type Spanned<Tok, Loc> = (Loc, Tok, Loc);

pub struct Lexer<'source> {
    token_stream: SpannedIter<'source, Token>,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            token_stream: Token::lexer(source).spanned(),
        }
    }

    pub fn skip_text(&mut self) {
        text_callback(&mut self.token_stream);
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Spanned<Result<Token, LexicalError>, usize>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(result, span)| (span.start, result, span.end))
    }
}
