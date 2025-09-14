use std::ops::Range;

use miette::ByteOffset;
use smol_str::SmolStr;
use thiserror::Error;

use crate::{parser::ParsingError, r#type::Type};

#[derive(Clone, Debug, Error, PartialEq, miette::Diagnostic)]
pub enum Error {
    #[error("failed to unify `{lhs}` with `{rhs}`")]
    FailedToUnify {
        lhs: Type,
        rhs: Type,
        #[label]
        span: Range<ByteOffset>,
    },
    #[error("not a function")]
    #[diagnostic(code(novo::error::Error::NotAFunction))]
    NotAFunction {
        #[label]
        span: Range<ByteOffset>,
    },
    #[error("cannot find value `{ident}` in this scope")]
    #[diagnostic(code(novo::error::Error::NotFound))]
    NotFound {
        ident: SmolStr,
        #[label("not found in this scope")]
        span: Range<ByteOffset>,
    },
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
