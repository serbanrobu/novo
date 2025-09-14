use salsa::Accumulator;

use crate::{
    error::Error,
    expr::Expr,
    ir::{Context, Diagnostic, Output, Program, Scope},
    op::{BinOp, UnOp},
    stmt::Stmt,
    r#type::Type,
};

#[salsa::tracked]
pub fn type_check_program<'db>(
    db: &'db dyn salsa::Database,
    program: Program<'db>,
    context: Context,
) {
    check_block(db, &mut context.scope(db), program.stmts(db));
}

fn check_block<'db>(db: &'db dyn salsa::Database, scope: &mut Scope, stmts: &[Stmt<'db>]) {
    let scope_1 = scope.clone();

    for stmt in stmts {
        check_stmt(db, scope, stmt);
    }

    *scope = scope_1;
}

fn check_stmt<'db>(db: &'db dyn salsa::Database, scope: &mut Scope, stmt: &Stmt<'db>) {
    match stmt {
        Stmt::Error => {}
        Stmt::If {
            cond,
            then_branch,
            elseif_branches,
            else_branch,
        } => {
            cond.check(db, scope, Type::Boolean);
            check_block(db, scope, then_branch);

            for (cond, then_branch) in elseif_branches {
                cond.check(db, scope, Type::Boolean);

                check_block(db, scope, then_branch);
            }

            if let Some(else_branch) = else_branch {
                check_block(db, scope, else_branch);
            }
        }
        Stmt::Set { ident, expr } => {
            if let Some(t) = expr.infer(db, scope) {
                let x = ident.text(db).clone();

                Output {
                    ident: x.clone(),
                    r#type: t.clone(),
                }
                .accumulate(db);

                scope.insert(x, t);
            }
        }
    }
}

impl<'db> Expr<'db> {
    fn check(&self, db: &'db dyn salsa::Database, scope: &mut Scope, r#type: Type) -> Option<()> {
        let t = self.infer(db, scope)?;

        if t != r#type {
            Diagnostic(Error::MismatchedTypes {
                expected: r#type,
                found: t,
                span: self.span(),
            })
            .accumulate(db);

            return None;
        }

        Some(())
    }

    fn infer(&self, db: &'db dyn salsa::Database, scope: &mut Scope) -> Option<Type> {
        match self {
            Self::Binary { left, op, right } => match op {
                BinOp::Add => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Number)
                }
                BinOp::And => {
                    left.check(db, scope, Type::Boolean)?;
                    right.check(db, scope, Type::Boolean)?;
                    Some(Type::Boolean)
                }
                BinOp::Concat => {
                    left.check(db, scope, Type::String)?;
                    right.check(db, scope, Type::String)?;
                    Some(Type::String)
                }
                BinOp::Div => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Number)
                }
                BinOp::Eq => {
                    let _t = left.infer(db, scope)?;
                    let _u = right.infer(db, scope)?;
                    Some(Type::Boolean)
                }
                BinOp::Filter => {
                    let at = left.infer(db, scope)?;
                    let ft = right.infer(db, scope)?;
                    let t = Type::Variable('a');
                    let ft_1 = Type::Function(at.into(), t.clone().into());
                    let s = ft.unify(&ft_1)?;
                    Some(t.substitute(&s))
                }
                BinOp::Ge => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Boolean)
                }
                BinOp::Gt => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Boolean)
                }
                BinOp::In => {
                    left.check(db, scope, Type::String)?;
                    right.check(db, scope, Type::String)?;
                    Some(Type::Boolean)
                }
                BinOp::Le => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Boolean)
                }
                BinOp::Lt => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Boolean)
                }
                BinOp::Mul => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Number)
                }
                BinOp::Ne => {
                    let _t = left.infer(db, scope)?;
                    let _u = right.infer(db, scope)?;
                    Some(Type::Boolean)
                }
                BinOp::Or => {
                    left.check(db, scope, Type::Boolean)?;
                    right.check(db, scope, Type::Boolean)?;
                    Some(Type::Boolean)
                }
                BinOp::Sub => {
                    left.check(db, scope, Type::Number)?;
                    right.check(db, scope, Type::Number)?;
                    Some(Type::Number)
                }
            },
            Self::Call { func, args, .. }
                if matches!(
                    **func,
                    Expr::Variable { ident, .. } if ident.text(db) == "max",
                ) =>
            {
                let _ = args
                    .iter()
                    .map(|a| a.check(db, scope, Type::Number))
                    .collect::<Option<Vec<_>>>()?;

                Some(Type::Number)
            }
            Self::Call { func, args, .. }
                if matches!(
                    **func,
                    Expr::Variable { ident, .. } if ident.text(db) == "min",
                ) =>
            {
                let _ = args
                    .iter()
                    .map(|a| a.check(db, scope, Type::Number))
                    .collect::<Option<Vec<_>>>()?;

                Some(Type::Number)
            }
            Self::Call { func, args, .. } => {
                let ft = func.infer(db, scope)?;

                let ats = args
                    .iter()
                    .map(|a| a.infer(db, scope))
                    .collect::<Option<Vec<_>>>()?;

                let t = Type::Variable('a');

                let ft_1 = ats
                    .into_iter()
                    .rfold(t.clone(), |acc, t| Type::Function(t.into(), acc.into()));

                let Some(s) = ft.unify(&ft_1) else {
                    Diagnostic(Error::FailedToUnify {
                        lhs: ft,
                        rhs: ft_1,
                        span: func.span(),
                    })
                    .accumulate(db);

                    return None;
                };

                Some(t.substitute(&s))
            }
            Self::Error { .. } => None,
            Self::LitNumber { .. } => Some(Type::Number),
            Self::LitString { .. } => Some(Type::String),
            Self::Paren { expr, .. } => expr.infer(db, scope),
            Self::Unary { op, expr, .. } => match op {
                UnOp::Neg => {
                    expr.check(db, scope, Type::Number)?;
                    Some(Type::Number)
                }
                UnOp::Pos => {
                    expr.check(db, scope, Type::Number)?;
                    Some(Type::Number)
                }
            },
            Self::Variable { start, ident, end } => {
                let Some(t) = scope.get(ident.text(db)) else {
                    Diagnostic(Error::NotFound {
                        ident: ident.text(db).clone(),
                        span: *start..*end,
                    })
                    .accumulate(db);

                    return None;
                };

                Some(t.clone())
            }
        }
    }
}
