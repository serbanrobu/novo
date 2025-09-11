#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum BinOp {
    Add,
    And,
    Concat,
    Div,
    Eq,
    Filter,
    Ge,
    Gt,
    In,
    Le,
    Lt,
    Mul,
    Ne,
    Or,
    Sub,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug)]
pub enum UnOp {
    Neg,
    Pos,
}
