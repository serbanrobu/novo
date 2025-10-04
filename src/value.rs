use std::{borrow::Cow, cmp::Ordering, fmt, sync::Arc};

use regex::Regex;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use crate::expr::Number;

#[derive(Clone, Debug, Deserialize, Hash, PartialEq, Eq, Serialize, salsa::Update)]
#[serde(untagged)]
pub enum Value {
    #[serde(skip)]
    Default,
    #[serde(skip)]
    Default1(Arc<Self>),
    Boolean(bool),
    Null,
    Number(Number),
    #[serde(skip)]
    RoundDown,
    #[serde(skip)]
    RoundDown1(Number),
    #[serde(skip)]
    RoundUp,
    #[serde(skip)]
    RoundUp1(Number),
    #[serde(skip)]
    Spaceless,
    String(SmolStr),
}

impl Value {
    pub fn apply(self, arg: Self) -> Result<Self, (Self, Self)> {
        match self {
            Self::Default => Ok(Self::Default1(arg.into())),
            Self::Default1(v) => Ok(if v.as_bool() {
                Arc::unwrap_or_clone(v)
            } else {
                arg
            }),
            Self::RoundDown => {
                let n: Number = arg.try_into().map_err(move |v| (self, v))?;
                Ok(Self::RoundDown1(n))
            }
            Self::RoundDown1(n) => {
                let o: Number = arg.try_into().map_err(move |v| (self, v))?;
                let factor = 10_f64.powi(o.into_inner() as i32);
                Ok(Self::Number(((n * factor).floor() / factor).into()))
            }
            Self::RoundUp => {
                let n: Number = arg.try_into().map_err(move |v| (self, v))?;
                Ok(Self::RoundUp1(n))
            }
            Self::RoundUp1(n) => {
                let o: Number = arg.try_into().map_err(move |v| (self, v))?;
                let factor = 10_f64.powi(o.into_inner() as i32);
                Ok(Self::Number(((n * factor).ceil() / factor).into()))
            }
            Self::Spaceless => {
                let s: SmolStr = arg.try_into().map_err(|v| (self, v))?;
                let re = Regex::new(r">\s+<").unwrap();
                Ok(Self::String(re.replace_all(&s, "><").into()))
            }
            _ => Err((self, arg)),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Self::Default => true,
            Self::Default1(_) => true,
            Self::Boolean(b) => *b,
            Self::Null => false,
            Self::Number(n) => *n != 0.0,
            Self::RoundDown => true,
            Self::RoundDown1(_) => true,
            Self::RoundUp => true,
            Self::RoundUp1(_) => true,
            Self::Spaceless => true,
            Self::String(s) => !matches!(s.as_str(), "0" | ""),
        }
    }

    pub fn as_number(&self) -> Option<Number> {
        match self {
            Self::Boolean(b) => Some((*b).into()),
            Self::Null => Some(0.0.into()),
            Self::Number(n) => Some(*n),
            Self::String(s) => s.parse().ok(),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<Cow<'_, str>> {
        match self {
            Self::Boolean(b) => Some(Cow::Borrowed(if *b { "1" } else { "" })),
            Self::Null => Some("".into()),
            Self::Number(n) => Some(Cow::Owned(n.to_string())),
            Self::String(s) => Some(Cow::Borrowed(s)),
            _ => None,
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        value.as_bool()
    }
}

impl TryFrom<Value> for Number {
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.as_number().ok_or(value)
    }
}

impl TryFrom<Value> for SmolStr {
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        value.as_string().map(|s| s.into()).ok_or(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<Number> for Value {
    fn from(value: Number) -> Self {
        Self::Number(value)
    }
}

impl From<SmolStr> for Value {
    fn from(value: SmolStr) -> Self {
        Self::String(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value.into())
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Boolean(b), v) => Some(b.cmp(&v.as_bool())),
            (Self::Null, v) => {
                if v.as_bool() {
                    Some(Ordering::Less)
                } else {
                    Some(Ordering::Equal)
                }
            }
            (Self::Number(n), v) => {
                let o = v.as_number()?;
                n.partial_cmp(&o)
            }
            (Self::String(s), v) => {
                let t = v.as_string()?;
                s.as_str().partial_cmp(t.as_ref())
            }
            (v, w) if v == w => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default => f.write_str("default"),
            Self::Default1(v) => write!(f, "default({v})"),
            Self::Boolean(b) => b.fmt(f),
            Self::Null => f.write_str("null"),
            Self::Number(n) => n.fmt(f),
            Self::RoundDown => f.write_str("round_down"),
            Self::RoundDown1(v) => write!(f, "round_down({v})"),
            Self::RoundUp => f.write_str("round_up"),
            Self::RoundUp1(v) => write!(f, "round_up({v})"),
            Self::Spaceless => f.write_str("spaceless"),
            Self::String(s) => write!(f, "{s:?}"),
        }
    }
}
