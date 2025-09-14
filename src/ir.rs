use crate::{stmt::Stmt, r#type::Type};
use std::sync::Arc;

use im::HashMap;
use smol_str::SmolStr;
use thiserror::Error;

use crate::error::Error;

#[salsa::interned(debug)]
pub struct Ident<'db> {
    #[returns(ref)]
    pub text: SmolStr,
}

#[salsa::tracked(debug)]
pub struct Program<'db> {
    #[tracked]
    #[returns(ref)]
    pub stmts: Vec<Stmt<'db>>,
}

#[salsa::input(debug)]
pub struct SourceProgram {
    #[returns(ref)]
    pub text: Arc<String>,
}

#[salsa::accumulator]
#[derive(Clone, Debug, Error, PartialEq, miette::Diagnostic)]
#[error(transparent)]
#[diagnostic(transparent)]
pub struct Diagnostic(#[from] pub Error);

#[salsa::accumulator]
#[derive(Clone)]
pub struct Output {
    pub ident: SmolStr,
    pub r#type: Type,
}

pub type Scope = HashMap<SmolStr, Type>;

#[salsa::input(debug)]
pub struct Context {
    pub scope: Scope,
}
