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
use persistent::list;
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
    fn eval(&self, arg: List) -> TinResult<Exp>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Proc {
    params: list::List<Symbol>,
    va: Option<Symbol>,
    body: Box<Exp>,
    env: EnvironmentRef,
}

impl Proc {
    pub fn new(params: list::List<Symbol>, body: Exp, env: EnvironmentRef) -> Self {
        Proc {
            params,
            body: Box::new(body),
            va: None,
            env,
        }
    }
    pub fn new_va(params: list::List<Symbol>, va: Symbol, body: Exp,env: EnvironmentRef) -> Self {
        Proc {
            params,
            body: Box::new(body),
            va: Some(va),
            env,
        }
    }

}

impl Evaluable for Proc {
    fn eval(&self, args: List) -> TinResult<Exp> {
        let argc = args.len();
        let plen = self.params.len();
        if argc < plen {
            return Err(TinError::ArityMismatch(plen, argc));
        }
        let mut env = Environment::from(self.env.clone());
        let mut it = args;
        for p in self.params.iter() {
            env.insert(p.clone(), it.head().unwrap().clone());
            it = it.tail();
        }

        match (self.va.as_ref(), it.len()) {
            (None, 0) => {}
            (None, _) => return Err(TinError::ArityMismatch(plen, argc)),
            (Some(x), _) => env.insert(x.clone(), it.into()),
        }

        eval(env.as_ref(), *self.body.clone())
    }
}

#[derive(Clone)]
pub struct Closure(Box<fn(List) -> TinResult<Exp>>);

impl Closure {
    pub fn new(f: fn(List) -> TinResult<Exp>) -> Exp {
        Exp::Closure(Closure(Box::new(f)))
    }
}

impl From<fn(List) -> TinResult<Exp>> for Closure {
    fn from(f: fn(List) -> TinResult<Exp>) -> Self {
        Closure(Box::new(f))
    }
}

impl Evaluable for Closure {
    fn eval(&self, args: List) -> TinResult<Exp> {
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

#[test]
fn test_va() {
    use super::parser::parse;
    let env = Environment::new().as_ref();
    let exp = parse("(define (foo x . va) va)").unwrap();
    eval(env.clone(), exp).unwrap();
    let exp = parse("(foo 1 2 3)").unwrap();
    assert_eq!(Ok(list!(2.into(), 3.into()).into()), eval(env, exp));
}

//pub use macros::Macro;
