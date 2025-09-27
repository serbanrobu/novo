use salsa::Accumulator;

use crate::{
    error::{Error, Result},
    expr::Expr,
    ir::{Context, Diagnostic, Program, Scope},
    op::{BinOp, UnOp},
    stmt::Stmt,
    r#type::Type,
    value::Value,
};

#[salsa::tracked]
pub fn type_check_program<'db>(
    db: &'db dyn salsa::Database,
    program: Program<'db>,
    context: Context,
) {
    let mut scope = context
        .environment(db)
        .iter()
        .map(|(x, v)| (x.clone(), v.clone().into()))
        .collect();

    check_block(db, &mut scope, program.stmts(db));
}

fn check_block<'db>(db: &'db dyn salsa::Database, scope: &mut Scope, stmts: &[Stmt<'db>]) {
    let scope_1 = scope.clone();

    for stmt in stmts {
        if let Err(e) = check_stmt(db, scope, stmt) {
            Diagnostic(e).accumulate(db);
        }
    }

    *scope = scope_1;
}

fn check_stmt<'db>(
    db: &'db dyn salsa::Database,
    scope: &mut Scope,
    stmt: &Stmt<'db>,
) -> Result<()> {
    match stmt {
        Stmt::Error => Ok(()),
        Stmt::If {
            cond,
            then_branch,
            elseif_branches,
            else_branch,
        } => check_if(
            db,
            scope,
            cond,
            then_branch,
            elseif_branches,
            else_branch.as_deref(),
        ),
        Stmt::Set { ident, expr } => {
            let t = expr.infer(db, scope)?;
            let x = ident.text(db).clone();
            scope.insert(x, t);
            Ok(())
        }
    }
}

fn check_if<'db>(
    db: &'db dyn salsa::Database,
    scope: &mut Scope,
    cond: &Expr<'db>,
    then_branch: &[Stmt<'db>],
    elseif_branches: &[(Expr<'db>, Vec<Stmt<'db>>)],
    else_branch: Option<&[Stmt<'db>]>,
) -> Result<()> {
    let t = cond.check(db, scope, Type::Boolean)?;

    if let Some(b) = t.as_value().map(|v| v.as_bool()) {
        if b {
            check_block(db, scope, then_branch);
            return Ok(());
        }
    } else {
        check_block(db, scope, then_branch);
    }

    if let Some(((cond, then_branch), elseif_branches)) = elseif_branches.split_first() {
        return check_if(db, scope, cond, then_branch, elseif_branches, else_branch);
    }

    if let Some(else_branch) = else_branch {
        check_block(db, scope, else_branch);
    }

    Ok(())
}

impl<'db> Expr<'db> {
    fn check(&self, db: &'db dyn salsa::Database, scope: &mut Scope, r#type: Type) -> Result<Type> {
        let t = self.infer(db, scope)?;

        r#type.subsumes(&t).ok_or_else(|| Error::MismatchedTypes {
            expected: r#type,
            found: t,
            span: self.span(),
        })
    }

    fn infer(&self, db: &'db dyn salsa::Database, scope: &mut Scope) -> Result<Type> {
        match self {
            Self::Binary { left, op, right } => match op {
                BinOp::Add => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(n), Some(o)) = (
                        t.as_value().and_then(|v| v.as_number()),
                        u.as_value().and_then(|v| v.as_number()),
                    ) {
                        return Ok(Type::Value((n + o).into()));
                    }

                    Ok(Type::Number)
                }
                BinOp::And => {
                    let t = left.check(db, scope, Type::Boolean)?;
                    let u = right.check(db, scope, Type::Boolean)?;

                    if let (Some(b), Some(c)) = (
                        t.as_value().map(|v| v.as_bool()),
                        u.as_value().map(|v| v.as_bool()),
                    ) {
                        return Ok(Type::Value((b & c).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Concat => {
                    let t = left.check(db, scope, Type::String)?;
                    let u = right.check(db, scope, Type::String)?;

                    if let (Some(s), Some(t)) = (
                        t.as_value().and_then(|v| v.as_string()),
                        u.as_value().and_then(|v| v.as_string()),
                    ) {
                        let mut u = String::with_capacity(s.len() + t.len());
                        u.push_str(&s);
                        u.push_str(&t);
                        return Ok(Type::Value(u.into()));
                    }

                    Ok(Type::String)
                }
                BinOp::Div => {
                    let t = left.infer(db, scope)?;
                    let u = right.infer(db, scope)?;

                    if let (Some(n), Some(o)) = (
                        t.as_value().and_then(|v| v.as_number()),
                        u.as_value().and_then(|v| v.as_number()),
                    ) {
                        return Ok(Type::Value((n / o).into()));
                    }

                    Ok(Type::Number)
                }
                BinOp::Eq => {
                    let t = left.infer(db, scope)?;
                    let u = right.infer(db, scope)?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value((v == w).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Filter => {
                    let t = left.infer(db, scope)?;

                    let u = right.check(
                        db,
                        scope,
                        Type::Function(t.clone().into(), Type::Any.into()),
                    )?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value(
                            w.clone()
                                .apply(v.clone())
                                .expect("should succeed as it's type checked"),
                        ));
                    }

                    Ok(u.function_output_type()
                        .expect("should have an output type as it's type checked"))
                }
                BinOp::Ge => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value((v >= w).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Gt => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value((v > w).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::In => {
                    let t = left.check(db, scope, Type::String)?;
                    let u = right.check(db, scope, Type::String)?;

                    if let (Some(s), Some(t)) = (
                        t.as_value().and_then(|v| v.as_string()),
                        u.as_value().and_then(|v| v.as_string()),
                    ) {
                        return Ok(Type::Value((t.contains(s.as_ref())).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Le => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value((v < w).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Lt => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value((v < w).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Mul => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(n), Some(o)) = (
                        t.as_value().and_then(|v| v.as_number()),
                        u.as_value().and_then(|v| v.as_number()),
                    ) {
                        return Ok(Type::Value((n * o).into()));
                    }

                    Ok(Type::Number)
                }
                BinOp::Ne => {
                    let t = left.infer(db, scope)?;
                    let u = right.infer(db, scope)?;

                    if let (Some(v), Some(w)) = (t.as_value(), u.as_value()) {
                        return Ok(Type::Value((v != w).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Or => {
                    let t = left.check(db, scope, Type::Boolean)?;
                    let u = right.check(db, scope, Type::Boolean)?;

                    if let (Some(b), Some(c)) = (
                        t.as_value().map(|v| v.as_bool()),
                        u.as_value().map(|v| v.as_bool()),
                    ) {
                        return Ok(Type::Value((b | c).into()));
                    }

                    Ok(Type::Boolean)
                }
                BinOp::Sub => {
                    let t = left.check(db, scope, Type::Number)?;
                    let u = right.check(db, scope, Type::Number)?;

                    if let (Some(n), Some(o)) = (
                        t.as_value().and_then(|v| v.as_number()),
                        u.as_value().and_then(|v| v.as_number()),
                    ) {
                        return Ok(Type::Value((n - o).into()));
                    }

                    Ok(Type::Number)
                }
            },
            Self::Call { func, args, .. }
                if matches!(
                    **func,
                    Expr::Variable { ident, .. } if ident.text(db) == "max",
                ) && !args.is_empty() =>
            {
                let ts = args
                    .iter()
                    .map(|a| a.check(db, scope, Type::Number))
                    .collect::<Result<Vec<_>>>()?;

                if let Some(n) = op_until_none(
                    ts.into_iter()
                        .map(|t| t.as_value().and_then(|v| v.as_number())),
                    Ord::max,
                ) {
                    return Ok(Type::Value(n.into()));
                }

                Ok(Type::Number)
            }
            Self::Call { func, args, .. }
                if matches!(
                    **func,
                    Expr::Variable { ident, .. } if ident.text(db) == "min",
                ) && !args.is_empty() =>
            {
                let ts = args
                    .iter()
                    .map(|a| a.check(db, scope, Type::Number))
                    .collect::<Result<Vec<_>>>()?;

                if let Some(n) = op_until_none(
                    ts.into_iter()
                        .map(|t| t.as_value().and_then(|v| v.as_number())),
                    Ord::min,
                ) {
                    return Ok(Type::Value(n.into()));
                }

                Ok(Type::Number)
            }
            Self::Call { func, args, .. } => {
                let t = func.infer(db, scope)?;

                let us = args
                    .iter()
                    .map(|a| a.infer(db, scope))
                    .collect::<Result<Vec<_>>>()?;

                let argc = us.len();

                let v = us.iter().rfold(Type::Any, |acc, u| {
                    Type::Function(u.clone().into(), acc.into())
                });

                let w = v.subsumes(&t).ok_or_else(|| Error::MismatchedTypes {
                    expected: v,
                    found: t.clone(),
                    span: func.span(),
                })?;

                if let Some(x) = t.as_value().cloned()
                    && let Ok(y) = us.into_iter().try_fold(x, |acc, u| match u.as_value() {
                        None => Err(()),
                        Some(y) => Ok(acc
                            .apply(y.clone())
                            .expect("should succeed as it's type checked")),
                    })
                {
                    return Ok(Type::Value(y));
                }

                Ok((0..argc).fold(w, |acc, _| {
                    acc.function_output_type()
                        .expect("should have an output type as it's type checked")
                }))
            }
            Self::Error { .. } => Ok(Type::Any),
            Self::LitBoolean { inner, .. } => Ok(Type::Value(Value::Boolean(*inner))),
            Self::LitNumber { inner, .. } => Ok(Type::Value(Value::Number(*inner))),
            Self::LitString { inner, .. } => Ok(Type::Value(Value::String(inner.text(db).clone()))),
            Self::Paren { expr, .. } => expr.infer(db, scope),
            Self::Unary { op, expr, .. } => match op {
                UnOp::Neg => {
                    let t = expr.check(db, scope, Type::Number)?;

                    if let Some(n) = t.as_value().and_then(|v| v.as_number()) {
                        return Ok(Type::Value((-n).into()));
                    }

                    Ok(Type::Number)
                }
                UnOp::Pos => {
                    let t = expr.check(db, scope, Type::Number)?;

                    if let Some(n) = t.as_value().and_then(|v| v.as_number()) {
                        return Ok(Type::Value(n.into()));
                    }

                    Ok(Type::Number)
                }
            },
            Self::Variable { ident, .. } => {
                let x = ident.text(db);

                Ok(scope.get(x).cloned().unwrap_or_else(|| {
                    Type::Value(match x.as_str() {
                        "default" => Value::Default,
                        "round_down" => Value::RoundDown,
                        "round_up" => Value::RoundUp,
                        "spaceless" => Value::Spaceless,
                        _ => Value::Null,
                    })
                }))
            }
        }
    }
}

fn op_until_none<T, I: Iterator<Item = Option<T>>>(
    mut iter: I,
    op: impl Fn(T, T) -> T,
) -> Option<T> {
    let first = iter.next()??;

    iter.try_fold(first, |m, x| match x {
        Some(v) => Ok(op(m, v)),
        None => Err(()),
    })
    .ok()
}
