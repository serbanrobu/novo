use std::{fmt, sync::Arc};

use smol_str::ToSmolStr;

use crate::{expr::Number, value::Value};

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum Type {
    Any,
    Boolean,
    Function(Arc<Self>, Arc<Self>),
    Number,
    String,
    Value(Value),
}

impl Type {
    pub fn as_value(&self) -> Option<&Value> {
        match self {
            Type::Value(v) => Some(v),
            _ => None,
        }
    }

    fn fmt_atom(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => f.write_str("any"),
            t @ Self::Function(..) => write!(f, "({t})"),
            Self::Boolean => f.write_str("boolean"),
            Self::Number => f.write_str("number"),
            Self::String => f.write_str("string"),
            Self::Value(v) => write!(f, "{v}"),
        }
    }

    pub fn function_output_type(&self) -> Option<Self> {
        match self {
            Type::Function(_, x) => Some(x.as_ref().clone()),
            Type::Value(Value::Default) => Some(Type::Function(Type::Any.into(), Type::Any.into())),
            Type::Value(Value::Default1(_v)) => Some(Type::Any),
            Type::Value(Value::RoundDown) => {
                Some(Type::Function(Type::Number.into(), Type::Number.into()))
            }
            Type::Value(Value::RoundDown1(_n)) => Some(Type::Number),
            Type::Value(Value::RoundUp) => {
                Some(Type::Function(Type::Number.into(), Type::Number.into()))
            }
            Type::Value(Value::RoundUp1(_n)) => Some(Type::Number),
            Type::Value(Value::Spaceless) => Some(Type::String),
            _ => None,
        }
    }

    pub fn subsumes(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Any, _) => Some(other.clone()),
            (Self::Boolean, _) => Some(Self::Boolean),
            (Self::Function(t, u), Self::Function(v, w)) => {
                let x = v.subsumes(t)?;
                let y = u.subsumes(w)?;
                Some(Self::Function(x.into(), y.into()))
            }
            (Self::Function(t, u), Self::Value(Value::Default)) => {
                let _v = Self::Any.subsumes(t)?;
                let _w = u.subsumes(&Self::Function(Self::Any.into(), Self::Any.into()))?;
                Some(Self::Value(Value::Default))
            }
            (Self::Function(t, u), Self::Value(Value::Default1(v))) => {
                if !v.as_bool() {
                    let _v = u.subsumes(t)?;
                }

                Some(Self::Value(Value::Default1(v.clone())))
            }
            (Self::Function(t, u), Self::Value(Value::RoundDown)) => {
                let _v = Self::Number.subsumes(t)?;
                let _w = u.subsumes(&Self::Function(Self::Number.into(), Self::Number.into()))?;
                Some(Self::Value(Value::RoundDown))
            }
            (Self::Function(t, u), Self::Value(Value::RoundDown1(n))) => {
                let _v = Self::Number.subsumes(t)?;
                let _w = u.subsumes(&Self::Number)?;
                Some(Self::Value(Value::RoundDown1(*n)))
            }
            (Self::Function(t, u), Self::Value(Value::RoundUp)) => {
                let _v = Self::Number.subsumes(t)?;
                let _w = u.subsumes(&Self::Function(Self::Number.into(), Self::Number.into()))?;
                Some(Self::Value(Value::RoundUp))
            }
            (Self::Function(t, u), Self::Value(Value::RoundUp1(n))) => {
                let _v = Self::Number.subsumes(t)?;
                let _w = u.subsumes(&Self::Number)?;
                Some(Self::Value(Value::RoundUp1(*n)))
            }
            (Self::Function(t, u), Self::Value(Value::Spaceless)) => {
                let _v = Self::String.subsumes(t)?;
                let _w = u.subsumes(&Self::String)?;
                Some(Self::Value(Value::Spaceless))
            }
            (Self::Number, Self::Boolean) => Some(Self::Number),
            (Self::Number, Self::Number) => Some(Self::Number),
            (Self::Number, Self::Value(Value::Boolean(b))) => Some(Self::Value(Value::Number(
                if *b { 1.0 } else { 0.0 }.into(),
            ))),
            (Self::Number, Self::Value(Value::Number(n))) => Some(Self::Value(Value::Number(*n))),
            (Self::Number, Self::Value(Value::String(s))) => s
                .parse::<Number>()
                .ok()
                .map(|n| Self::Value(Value::Number(n))),
            (Self::Number, Self::Value(Value::Null)) => {
                Some(Self::Value(Value::Number(0.0.into())))
            }
            (Self::String, Self::Boolean) => Some(Self::String),
            (Self::String, Self::String) => Some(Self::String),
            (Self::String, Self::Value(Value::Boolean(b))) => {
                Some(Self::Value(Value::String(if *b { "1" } else { "" }.into())))
            }
            (Self::String, Self::Value(Value::Number(n))) => {
                Some(Self::Value(Value::String(n.to_smolstr())))
            }
            (Self::String, Self::Value(Value::String(s))) => {
                Some(Self::Value(Value::String(s.clone())))
            }
            (Self::String, Self::Value(Value::Null)) => Some(Self::Value(Value::String("".into()))),
            (Self::Value(v), Self::Value(w)) if v == w => Some(Self::Value(v.clone())),
            _ => None,
        }
    }
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        Type::Value(value)
    }
}

impl TryFrom<Type> for Value {
    type Error = Type;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Value(value) => Ok(value),
            _ => Err(value),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function(t, u) => {
                t.fmt_atom(f)?;
                write!(f, " -> {u}")
            }
            _ => self.fmt_atom(f),
        }
    }
}
