use miette::Result;
use salsa::Accumulator;
use smol_str::SmolStr;

use crate::{
    error::Error,
    expr::{Expr, Number},
    ir::{Context, Diagnostic, Environment, Output, Program},
    op::{BinOp, UnOp},
    stmt::Stmt,
    r#type::Type,
    value::Value,
};

#[salsa::tracked]
pub fn interpret_program<'db>(
    db: &'db dyn salsa::Database,
    program: Program<'db>,
    context: Context,
) {
    let mut env = context.environment(db).clone();

    if let Err(e) = interpret_block(db, program.stmts(db), &mut env) {
        Diagnostic(e).accumulate(db);
    }
}

fn interpret_block<'db>(
    db: &'db dyn salsa::Database,
    stmts: &[Stmt<'db>],
    env: &mut Environment,
) -> Result<(), Error> {
    for stmt in stmts {
        stmt.interpret(db, env)?;
    }

    Ok(())
}

impl<'db> Stmt<'db> {
    pub fn interpret(
        &self,
        db: &'db dyn salsa::Database,
        env: &mut Environment,
    ) -> Result<(), Error> {
        match self {
            Stmt::Error => {}
            Stmt::If {
                cond,
                then_branch,
                elseif_branches,
                else_branch,
            } => {
                if cond.evaluate(db, env)?.into() {
                    return interpret_block(db, then_branch, env);
                }

                for (cond, then_branch) in elseif_branches {
                    if cond.evaluate(db, env)?.into() {
                        return interpret_block(db, then_branch, env);
                    }
                }

                if let Some(else_branch) = else_branch {
                    return interpret_block(db, else_branch, env);
                }
            }
            Stmt::Set { ident, expr } => {
                let x = ident.text(db).clone();
                let v = expr.evaluate(db, env)?;

                Output {
                    ident: x.clone(),
                    value: v.clone(),
                }
                .accumulate(db);

                env.insert(x, v);
            }
        }

        Ok(())
    }
}

impl<'db> Expr<'db> {
    pub fn evaluate(
        &self,
        db: &'db dyn salsa::Database,
        env: &mut Environment,
    ) -> Result<Value, Error> {
        match self {
            Self::Binary { left, op, right } => match op {
                BinOp::Add => {
                    let n = left.evaluate_number(db, env)?;
                    let o = right.evaluate_number(db, env)?;
                    Ok(Value::Number(n + o))
                }
                BinOp::And => {
                    let b = left.evaluate(db, env).map(bool::from)?;
                    let c = right.evaluate(db, env).map(bool::from)?;
                    Ok(Value::Boolean(b & c))
                }
                BinOp::Concat => {
                    let s = left.evaluate_string(db, env)?;
                    let t = right.evaluate_string(db, env)?;
                    let mut u = String::with_capacity(s.len() + t.len());
                    u.push_str(&s);
                    u.push_str(&t);
                    Ok(Value::String(u.into()))
                }
                BinOp::Div => {
                    let n = left.evaluate_number(db, env)?;
                    let o = right.evaluate_number(db, env)?;
                    Ok(Value::Number(n / o))
                }
                BinOp::Eq => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;

                    match v {
                        Value::Boolean(b) => {
                            let c: bool = w.into();
                            Ok(Value::Boolean(b == c))
                        }
                        Value::Null => {
                            let b: bool = w.into();
                            Ok(Value::Boolean(!b))
                        }
                        Value::Number(n) => {
                            let Ok(o): Result<Number, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(n == o))
                        }
                        Value::String(s) => {
                            let Ok(t): Result<SmolStr, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(s == t))
                        }
                        _ => Ok(Value::Boolean(v == w)),
                    }
                }
                BinOp::Filter => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;
                    Ok(w.apply(v).unwrap_or(Value::Null))
                }
                BinOp::Ge => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;

                    match v {
                        Value::Boolean(b) => {
                            let c = w.into();
                            Ok(Value::Boolean(b >= c))
                        }
                        Value::Number(n) => {
                            let Ok(o): Result<Number, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(n >= o))
                        }
                        Value::String(s) => {
                            let Ok(t): Result<SmolStr, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(s >= t))
                        }
                        _ => Ok(Value::Boolean(v == w)),
                    }
                }
                BinOp::Gt => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;

                    match v {
                        Value::Boolean(b) => {
                            let c: bool = w.into();
                            Ok(Value::Boolean(b & !c))
                        }
                        Value::Number(n) => {
                            let Ok(o): Result<Number, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(n > o))
                        }
                        Value::String(s) => {
                            let Ok(t): Result<SmolStr, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(s > t))
                        }
                        _ => Ok(Value::Boolean(false)),
                    }
                }
                BinOp::In => {
                    let s = left.evaluate_string(db, env)?;
                    let t = right.evaluate_string(db, env)?;
                    Ok(Value::Boolean(t.contains(s.as_str())))
                }
                BinOp::Le => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;

                    match v {
                        Value::Boolean(b) => {
                            let c = w.into();
                            Ok(Value::Boolean(b <= c))
                        }
                        Value::Number(n) => {
                            let Ok(o): Result<Number, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(n <= o))
                        }
                        Value::String(s) => {
                            let Ok(t): Result<SmolStr, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(s <= t))
                        }
                        _ => Ok(Value::Boolean(v == w)),
                    }
                }
                BinOp::Lt => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;

                    match v {
                        Value::Boolean(b) => {
                            let c: bool = w.into();
                            Ok(Value::Boolean(!b & c))
                        }
                        Value::Number(n) => {
                            let Ok(o): Result<Number, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(n < o))
                        }
                        Value::String(s) => {
                            let Ok(t): Result<SmolStr, _> = w.try_into() else {
                                return Ok(Value::Boolean(false));
                            };

                            Ok(Value::Boolean(s < t))
                        }
                        _ => Ok(Value::Boolean(false)),
                    }
                }
                BinOp::Mul => {
                    let n = left.evaluate_number(db, env)?;
                    let o = right.evaluate_number(db, env)?;
                    Ok(Value::Number(n * o))
                }
                BinOp::Ne => {
                    let v = left.evaluate(db, env)?;
                    let w = right.evaluate(db, env)?;

                    match v {
                        Value::Boolean(b) => {
                            let c: bool = w.into();
                            Ok(Value::Boolean(b != c))
                        }
                        Value::Number(n) => {
                            let Ok(o): Result<Number, _> = w.try_into() else {
                                return Ok(Value::Boolean(true));
                            };

                            Ok(Value::Boolean(n != o))
                        }
                        Value::String(s) => {
                            let Ok(t): Result<SmolStr, _> = w.try_into() else {
                                return Ok(Value::Boolean(true));
                            };

                            Ok(Value::Boolean(s != t))
                        }
                        _ => Ok(Value::Boolean(v != w)),
                    }
                }
                BinOp::Or => {
                    let b = left.evaluate(db, env).map(bool::from)?;
                    let c = right.evaluate(db, env).map(bool::from)?;
                    Ok(Value::Boolean(b | c))
                }
                BinOp::Sub => {
                    let n = left.evaluate_number(db, env)?;
                    let o = right.evaluate_number(db, env)?;
                    Ok(Value::Number(n - o))
                }
            },
            Self::Call { func, args, .. } => {
                let v = func.evaluate(db, env)?;

                let ws = args
                    .iter()
                    .map(|v| v.evaluate(db, env))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(ws
                    .into_iter()
                    .try_fold(v, Value::apply)
                    .unwrap_or(Value::Null))
            }
            Self::Error { .. } => Ok(Value::Null),
            Self::LitBoolean { inner, .. } => Ok(Value::Boolean(*inner)),
            Self::LitNumber { inner, .. } => Ok(Value::Number(*inner)),
            Self::LitString { inner, .. } => Ok(Value::String(inner.text(db).clone())),
            Self::Paren { expr, .. } => expr.evaluate(db, env),
            Self::Unary { op, expr, .. } => match op {
                UnOp::Neg => expr.evaluate_number(db, env).map(|n| Value::Number(-n)),
                UnOp::Pos => expr.evaluate_number(db, env).map(Value::Number),
            },
            Self::Variable { ident, .. } => {
                let x = ident.text(db);

                Ok(env.get(x).cloned().unwrap_or_else(|| match x.as_str() {
                    "default" => Value::Default,
                    "round_down" => Value::RoundDown,
                    "round_up" => Value::RoundUp,
                    "spaceless" => Value::Spaceless,
                    _ => Value::Null,
                }))
            }
        }
    }

    pub fn evaluate_number(
        &self,
        db: &'db dyn salsa::Database,
        env: &mut Environment,
    ) -> Result<Number, Error> {
        self.evaluate(db, env)?
            .try_into()
            .map_err(|v: Value| Error::MismatchedTypes {
                expected: Type::Number,
                found: v.into(),
                span: self.span(),
            })
    }

    pub fn evaluate_string(
        &self,
        db: &'db dyn salsa::Database,
        env: &mut Environment,
    ) -> Result<SmolStr, Error> {
        self.evaluate(db, env)?
            .try_into()
            .map_err(|v: Value| Error::MismatchedTypes {
                expected: Type::String,
                found: v.into(),
                span: self.span(),
            })
    }
}
