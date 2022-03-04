use super::{Atom, Exp};
use crate::lib::types::TinError;
use std::convert::{From, TryFrom};
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl From<i64> for Number {
    fn from(x: i64) -> Self {
        Number::Int(x)
    }
}

impl From<f64> for Number {
    fn from(x: f64) -> Self {
        Number::Float(x)
    }
}

impl TryFrom<Exp> for Number {
    type Error = TinError;
    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Number(n) => Ok(n),
            _ => Err(TinError::TypeMismatch(
                "number".to_string(),
                format!("{:?}", value),
            )),
        }
    }
}

impl TryFrom<&Exp> for Number {
    type Error = TinError;
    fn try_from(value: &Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Number(n) => Ok(*n),
            _ => Err(TinError::TypeMismatch(
                "number".to_string(),
                format!("{:?}", value),
            )),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Number::Int(x), Number::Int(y)) => (Number::Int(x + y)),
            (Number::Float(x), Number::Float(y)) => (Number::Float(x + y)),
            (Number::Float(x), Number::Int(y)) => (Number::Float(x + y as f64)),
            (Number::Int(x), Number::Float(y)) => (Number::Float(x as f64 + y)),
        }
    }
}

impl Sub for Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (Number::Int(x), Number::Int(y)) => (Number::Int(x - y)),
            (Number::Float(x), Number::Float(y)) => (Number::Float(x - y)),
            (Number::Float(x), Number::Int(y)) => (Number::Float(x - y as f64)),
            (Number::Int(x), Number::Float(y)) => (Number::Float(x as f64 - y)),
        }
    }
}

impl Mul for Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (Number::Int(x), Number::Int(y)) => (Number::Int(x * y)),
            (Number::Float(x), Number::Float(y)) => (Number::Float(x * y)),
            (Number::Float(x), Number::Int(y)) => (Number::Float(x * y as f64)),
            (Number::Int(x), Number::Float(y)) => (Number::Float(x as f64 * y)),
        }
    }
}

impl Div for Number {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (Number::Int(x), Number::Int(y)) => (Number::Int(x / y)),
            (Number::Float(x), Number::Float(y)) => (Number::Float(x / y)),
            (Number::Float(x), Number::Int(y)) => (Number::Float(x / y as f64)),
            (Number::Int(x), Number::Float(y)) => (Number::Float(x as f64 / y)),
        }
    }
}

impl Number {
    pub fn floor(&self) -> Self {
        match self {
            Number::Int(n) => Number::Int(*n),
            Number::Float(f) => Number::Int(f.floor() as i64),
        }
    }

    pub fn abs(&self) -> Self {
        match self {
            Number::Int(n) => Number::Int(if *n < 0 { -n } else { *n }),
            Number::Float(n) => Number::Float(if *n < 0.0 { -n } else { *n }),
        }
    }
}
