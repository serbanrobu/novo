use std::ops::Range;

use miette::ByteOffset;
use thiserror::Error;

use crate::{parser::ParsingError, r#type::Type};

#[derive(Clone, Debug, Error, PartialEq, miette::Diagnostic)]
pub enum Error {
    #[error(transparent)]
    #[diagnostic(transparent)]
    ParsingError(#[from] ParsingError),
    #[error("mismatched types")]
    #[diagnostic(code(novo::error::Error::MismatchedTypes))]
    MismatchedTypes {
        expected: Type,
        found: Type,
        #[label("expected `{expected}`, found `{found}`")]
        span: Range<ByteOffset>,
    },
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
