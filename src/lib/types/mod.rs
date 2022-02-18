mod environment;
mod list;
use crate::lib::eval::eval;
use std::convert::From;

pub type Symbol = String;
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

impl From<String> for Atom {
    fn from(x: String) -> Self {
        Atom::Symbol(x)
    }
}

impl<T: Into<Number>> From<T> for Atom {
    fn from(x: T) -> Self {
        Atom::Number(x.into())
    }
}

impl From<bool> for Atom {
    fn from(x: bool) -> Self {
        Atom::Bool(x)
    }
}

pub use list::List;

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Atom(Atom),
    List(List),
    String(String),
    Proc(Proc),
    Closure(Closure),
}

impl From<List> for Exp {
    fn from(x: List) -> Self {
        Exp::List(x)
    }
}

impl<T: Into<Atom>> From<T> for Exp {
    fn from(x: T) -> Self {
        Exp::Atom(x.into())
    }
}

impl From<Proc> for Exp {
    fn from(x: Proc) -> Self {
        Exp::Proc(x)
    }
}

impl From<Closure> for Exp {
    fn from(x: Closure) -> Self {
        Exp::Closure(x)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TinError {
    SyntaxError(String),
    TypeMismatch(String, String),
    NotAProcedure(Exp),
    NotASymbol(Exp),
    ArityMismatch(usize, usize),
    Undefined(Symbol),
    Null,
}

pub type TinResult<R> = Result<R, TinError>;

impl Exp {
    pub fn truthy(&self) -> bool {
        if let Exp::Atom(Atom::Bool(false)) = self {
            false
        } else {
            true
        }
    }
}

use std::fmt::{self, Debug};
impl fmt::Display for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Exp::Atom(Atom::Symbol(s)) => write!(f, "{}", s),
            Exp::Atom(Atom::Number(Number::Int(n))) => write!(f, "{}", n),
            Exp::Atom(Atom::Number(Number::Float(n))) => write!(f, "{}", n),
            Exp::Atom(Atom::Bool(b)) => write!(f, "{}", b),
            Exp::String(s) => write!(f, "\"{}\"", s),
            Exp::Proc(_) => write!(f, "#proc"),
            Exp::Closure(_) => write!(f, "#proc"),
            Exp::List(l) => {
                write!(f, "( ")?;
                for exp in l.clone() {
                    write!(f, "{} ", exp)?;
                }
                write!(f, ")")
            }
        }
    }
}

pub use environment::{Environment, EnvironmentRef};

pub trait Evaluable {
    fn eval(&self, arg: &[Exp]) -> TinResult<Exp>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proc {
    params: Vec<Symbol>,
    body: Box<Exp>,
    env: EnvironmentRef,
}

impl Proc {
    pub fn new(params: Vec<Symbol>, body: Exp, env: EnvironmentRef) -> Self {
        Proc {
            params,
            body: Box::new(body),
            env,
        }
    }
}

impl Evaluable for Proc {
    fn eval(&self, args: &[Exp]) -> TinResult<Exp> {
        let env = EnvironmentRef::from(&self.params, args, self.env.clone());
        eval(env, *self.body.clone())
    }
}

#[derive(Clone)]
pub struct Closure {
    closure: Box<fn(&[Exp]) -> TinResult<Exp>>,
}

impl Closure {
    pub fn new(f: fn(&[Exp]) -> TinResult<Exp>) -> Exp {
        Exp::Closure(Closure {
            closure: Box::new(f),
        })
    }
}
impl Evaluable for Closure {
    fn eval(&self, args: &[Exp]) -> TinResult<Exp> {
        (self.closure)(args)
    }
}

impl Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RustProc {{ closure: # }}")
    }
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        *self.closure as usize == *other.closure as usize
    }
}
