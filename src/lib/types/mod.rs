mod environment;
mod list;
use crate::lib::eval::eval;
use std::collections::HashMap;
use std::convert::{From, TryFrom, TryInto};

pub type Symbol = String;
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
pub enum Number {
    Int(i64),
    Float(f64),
}
pub static NUMBER: &str = "number";

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
            Exp::Atom(Atom::Number(n)) => Ok(n),
            _ => Err(TinError::TypeMismatch(
                NUMBER.to_string(),
                value.to_string(),
            )),
        }
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

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Symbol(Symbol),
    Number(Number),
    Bool(bool),
    Char(char),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Key {
    Int(i64),
    Bool(bool),
    Symbol(String),
    Char(char),
    String(String),
}

impl From<Key> for Exp {
    fn from(k: Key) -> Self {
        match k {
            Key::Int(i) => Exp::Atom(Atom::Number(Number::Int(i))),
            Key::String(s) => Exp::String(s),
            Key::Bool(b) => Exp::Atom(Atom::Bool(b)),
            Key::Symbol(s) => Exp::Atom(Atom::Symbol(s)),
            Key::Char(s) => Exp::Atom(Atom::Char(s)),
        }
    }
}

impl TryFrom<Exp> for Key {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Atom(Atom::Number(Number::Int(n))) => Ok(Key::Int(n)),
            Exp::String(s) => Ok(Key::String(s)),
            Exp::Atom(Atom::Bool(b)) => Ok(Key::Bool(b)),
            Exp::Atom(Atom::Symbol(s)) => Ok(Key::Symbol(s)),
            Exp::Atom(Atom::Char(s)) => Ok(Key::Char(s)),
            _ => Err(TinError::TypeMismatch(
                "Hashable".to_string(),
                value.to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Map(HashMap<Key, Exp>);

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_of!(self) == std::ptr::addr_of!(other)
    }
}

type Pairs<'a> = std::iter::Map<
    std::collections::hash_map::Iter<'a, Key, Exp>,
    fn((&'a Key, &'a Exp)) -> (Exp, Exp),
>;
impl Map {
    pub fn new() -> Self {
        Map(HashMap::new())
    }

    pub fn len(&self) -> Number {
        Number::Int(self.0.len() as i64)
    }

    pub fn includes(&self, k: &Key) -> bool {
        self.0.contains_key(k)
    }

    pub fn try_insert(&mut self, k: Exp, v: Exp) -> TinResult<()> {
        self.0.insert(k.try_into()?, v);
        Ok(())
    }

    pub fn iter(&self) -> Pairs {
        self.0.iter().map(|(k, v)| (k.clone().into(), v.clone()))
    }
}

impl std::ops::Index<&Key> for Map {
    type Output = Exp;
    fn index(&self, index: &Key) -> &Self::Output {
        &self.0[index]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Atom(Atom),
    List(List),
    Vector(Vec<Exp>),
    String(String),
    Map(Map),
    Macro(Macro),
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

type Expect = String;
type Got = String;

#[derive(Debug, Clone, PartialEq)]
pub enum TinError {
    SyntaxError(String),
    TypeMismatch(Expect, Got),
    NotAProcedure(Exp),
    NotASymbol(Exp),
    ArityMismatch(usize, usize),
    Undefined(Symbol),
    KeyNotFound(Key),
    OutOfRange(usize),
    Null,
}

pub type TinResult<R> = Result<R, TinError>;

impl Exp {
    pub fn is_null(&self) -> bool {
        if let Exp::List(l) = self {
            return l.is_empty();
        }
        false
    }
    pub fn truthy(&self) -> bool {
        if let Exp::Atom(Atom::Bool(false)) = self {
            false
        } else {
            true
        }
    }

    pub fn replace(self, pat: &Exp, repl: Exp) -> Exp {
        match self {
            Exp::Atom(_) => {
                if self == *pat {
                    repl
                } else {
                    self
                }
            }
            Exp::List(lst) => Exp::List(lst.replace(pat, repl)),
            _ => self,
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
            Exp::Atom(Atom::Char(c)) => write!(f, "\\{}", c),
            Exp::String(s) => write!(f, "\"{}\"", s),
            Exp::Macro(_) => write!(f, "#macro"),
            Exp::Proc(_) => write!(f, "#proc"),
            Exp::Closure(_) => write!(f, "#proc"),
            Exp::Map(m) => {
                write!(f, "{{ ")?;
                for (k, v) in m.iter() {
                    write!(f, "{} {} ", k, v)?;
                }
                write!(f, "}}")
            }
            Exp::Vector(v) => {
                write!(f, "[ ")?;
                for exp in v {
                    write!(f, "{} ", exp)?;
                }
                write!(f, "]")
            }
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
pub struct Closure(Box<fn(&[Exp]) -> TinResult<Exp>>);

impl Closure {
    pub fn new(f: fn(&[Exp]) -> TinResult<Exp>) -> Exp {
        Exp::Closure(Closure(Box::new(f)))
    }
}

impl From<for<'a> fn(&'a [Exp]) -> TinResult<Exp>> for Closure {
    fn from(f: fn(&[Exp]) -> TinResult<Exp>) -> Self {
        Closure(Box::new(f))
    }
}

impl Evaluable for Closure {
    fn eval(&self, args: &[Exp]) -> TinResult<Exp> {
        (self.0)(args)
    }
}

impl Debug for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RustProc {{ closure: # }}")
    }
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        *self.0 as usize == *other.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern {
    Symbol(Symbol),
    List(Vec<Pattern>),
}

impl TryFrom<List> for Pattern {
    type Error = TinError;

    fn try_from(value: List) -> Result<Self, Self::Error> {
        let vals: Vec<_> = value.map(|x| x.try_into()).collect::<TinResult<_>>()?;
        Ok(Pattern::List(vals))
    }
}

impl TryFrom<Exp> for Pattern {
    type Error = TinError;

    fn try_from(value: Exp) -> Result<Self, Self::Error> {
        match value {
            Exp::Atom(Atom::Symbol(sym)) => Ok(Pattern::Symbol(sym)),
            Exp::List(lst) => lst.try_into(),
            _ => Err(TinError::TypeMismatch(
                "Symbol | List".into(),
                value.to_string(),
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Macro {
    params: Vec<Pattern>,
    arity: usize,
    rule: Box<Exp>,
}

impl Macro {
    pub fn new(params: Vec<Pattern>, rule: Exp) -> Self {
        let arity = params.len();
        Macro {
            params,
            arity,
            rule: Box::new(rule),
        }
    }
    pub fn expand(&self, args: &[Exp]) -> TinResult<Exp> {
        if args.len() != self.arity {
            return Err(TinError::ArityMismatch(self.arity, args.len()));
        }
        unimplemented!();

        //         let params = self.params.clone();
        //         let mut res: Exp = *self.rule.clone();
        //         for (i, param) in params.enumerate() {
        //             res = res.replace(&param, args[i].clone());
        //         }
        // Ok(res)
    }
}

impl From<Macro> for Exp {
    fn from(m: Macro) -> Self {
        Exp::Macro(m)
    }
}
