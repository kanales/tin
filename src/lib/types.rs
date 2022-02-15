pub type Symbol = String;
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Number {
    Int(i64),
    Float(f64),
}

use std::ops::{Add, Div, Mul, Sub};

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
    pub fn abs(&self) -> Self {
        match self {
            Number::Int(n) => Number::Int(if *n < 0 { -n } else { *n }),
            Number::Float(n) => Number::Float(if *n < 0.0 { -n } else { *n }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Symbol(Symbol),
    Number(Number),
    Bool(bool),
}

pub type List = Vec<Exp>;
#[derive(Debug, PartialEq, Clone)]
pub enum Exp {
    Atom(Atom),
    List(List),
}

#[derive(Debug, PartialEq)]
pub enum SchemeError {
    SyntaxError(String),
    TypeMismatch(String, String),
    NotAProcedure(Exp),
    NotASymbol(Exp),
    ArityMismatch(usize, usize),
}

pub type SchemeResult<R> = Result<R, SchemeError>;

impl Exp {
    pub fn truthy(&self) -> bool {
        if let Exp::Atom(Atom::Bool(false)) = self {
            false
        } else {
            true
        }
    }
}

use std::fmt;
impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Atom(Atom::Symbol(s)) => write!(f, "{}", s),
            Exp::Atom(Atom::Number(Number::Int(n))) => write!(f, "{}", n),
            Exp::Atom(Atom::Number(Number::Float(n))) => write!(f, "{}", n),
            Exp::Atom(Atom::Bool(b)) => write!(f, "{}", b),
            Exp::List(l) => {
                write!(f, "( ")?;
                for exp in l {
                    write!(f, "{} ", exp)?;
                }
                write!(f, ")")
            }
        }
    }
}
