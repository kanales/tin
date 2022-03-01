mod environment;
use crate::lib::eval::eval;
use std::convert::From;

mod atom;
mod macros;
mod map;
mod number;
mod symbol;

pub use atom::Atom;
pub use macros::Macro;
pub use number::Number;
pub use symbol::Symbol;

pub use map::{Key, Map};

pub type List = persistent::list::List<Exp>;

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
            Exp::List(lst) => unreachable!(), // Exp::List(lst.replace(pat, repl)),
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
                for exp in l.iter() {
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

//pub use macros::Macro;
