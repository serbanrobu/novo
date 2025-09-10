use std::sync::Arc;

use logos::Span;
use ordered_float::OrderedFloat;
use smol_str::SmolStr;

use crate::op::{BinOp, UnOp};

pub type LitNumber = OrderedFloat<f64>;

#[salsa::interned(debug)]
pub struct LitString<'db> {
    #[returns(ref)]
    pub text: String,
}

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

#[derive(Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum Stmt<'db> {
    Error,
    If {
        cond: Expr<'db>,
        then_branch: Vec<Stmt<'db>>,
        elseif_branches: Vec<(Expr<'db>, Vec<Stmt<'db>>)>,
        else_branch: Option<Vec<Stmt<'db>>>,
    },
    Set {
        ident: Ident<'db>,
        expr: Expr<'db>,
    },
}

#[derive(Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum Expr<'db> {
    Binary {
        left: Box<Self>,
        op: BinOp,
        right: Box<Self>,
    },
    Call {
        func: Box<Self>,
        args: Vec<Self>,
        end: usize,
    },
    Error {
        span: Span,
    },
    LitNumber {
        start: usize,
        inner: LitNumber,
        end: usize,
    },
    LitString {
        start: usize,
        inner: LitString<'db>,
        end: usize,
    },
    Paren {
        start: usize,
        expr: Box<Self>,
        end: usize,
    },
    Unary {
        start: usize,
        op: UnOp,
        expr: Box<Self>,
    },
    Variable {
        start: usize,
        ident: Ident<'db>,
        end: usize,
    },
}

impl<'db> Expr<'db> {
    pub fn start(&self) -> usize {
        match self {
            Self::Binary { left, .. } => left.start(),
            Self::Call { func, .. } => func.start(),
            Self::Error { span } => span.start,
            Self::LitNumber { start, .. } => *start,
            Self::LitString { start, .. } => *start,
            Self::Paren { start, .. } => *start,
            Self::Unary { start, .. } => *start,
            Self::Variable { start, .. } => *start,
        }
    }

    pub fn end(&self) -> usize {
        match self {
            Self::Binary { right, .. } => right.end(),
            Self::Call { end, .. } => *end,
            Self::Error { span } => span.end,
            Self::LitNumber { end, .. } => *end,
            Self::LitString { end, .. } => *end,
            Self::Paren { end, .. } => *end,
            Self::Unary { expr, .. } => expr.end(),
            Self::Variable { end, .. } => *end,
        }
    }

    pub fn span(&self) -> Span {
        self.start()..self.end()
    }
}
