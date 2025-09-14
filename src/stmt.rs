use crate::{expr::Expr, ir::Ident};

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
