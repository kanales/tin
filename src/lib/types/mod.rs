mod environment;
use crate::lib::eval::eval;
use std::convert::From;

mod atom;
mod exp;
mod macros;
mod map;
mod number;
mod symbol;

pub use atom::Atom;
pub use exp::Exp;
pub use macros::Macro;
pub use map::{Key, Map};
pub use number::Number;
pub use symbol::Symbol;

pub type List = persistent::list::List<Exp>;

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
    UnpairedDefinition,
    Null,
}

pub type TinResult<R> = Result<R, TinError>;
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

use std::fmt;
impl fmt::Debug for Closure {
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
