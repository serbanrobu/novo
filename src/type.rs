use std::{
    fmt::{self, Write},
    sync::Arc,
};

use im::{HashMap, hashmap};

pub type Substitution = HashMap<TypeVariable, Type>;

pub type TypeVariable = char;

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum Type {
    Boolean,
    Function(Arc<Self>, Arc<Self>),
    Number,
    String,
    Variable(char),
}

impl Type {
    fn contains(&self, x: TypeVariable) -> bool {
        match self {
            Self::Boolean => false,
            Self::Function(t, u) => t.contains(x) || u.contains(x),
            Self::Number => false,
            Self::String => false,
            Self::Variable(y) => x == *y,
        }
    }

    fn fmt_atom(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            t @ Self::Function(..) => write!(f, "({t})"),
            Self::Boolean => f.write_str("Boolean"),
            Self::Number => f.write_str("Number"),
            Self::String => f.write_str("String"),
            Self::Variable(a) => f.write_char(*a),
        }
    }

    pub fn substitute(&self, s: &Substitution) -> Self {
        match self {
            Self::Boolean => Self::Boolean,
            Self::Function(t, u) => Self::Function(t.substitute(s).into(), u.substitute(s).into()),
            Self::Number => Self::Number,
            Self::String => Self::String,
            Self::Variable(x) => s.get(x).cloned().unwrap_or(Self::Variable(*x)),
        }
    }

    pub fn unify(&self, other: &Self) -> Option<Substitution> {
        match (self, other) {
            (Type::Boolean, Type::Boolean) => Some(hashmap! {}),
            (Type::Function(t, u), Type::Function(t_1, u_1)) => {
                let s = t.unify(t_1)?;
                u.substitute(&s).unify(&u_1.substitute(&s))
            }
            (Type::Number, Type::Number) => Some(hashmap! {}),
            (Type::String, Type::String) => Some(hashmap! {}),
            (Type::Variable(x), Type::Variable(y)) if x == y => Some(hashmap! {}),
            (Type::Variable(x), other) if other.contains(*x) => None,
            (Type::Variable(x), other) => Some(hashmap! { *x => other.clone() }),
            (_, Type::Variable(_)) => other.unify(self),
            _ => None,
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
