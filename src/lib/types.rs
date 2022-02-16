use crate::lib::eval::eval;
use std::rc::Rc;

pub type Symbol = String;
#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum Exp {
    Atom(Atom),
    List(List),
    Proc(Proc),
}

#[derive(Debug, Clone, PartialEq)]
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
            Exp::Proc(_) => write!(f, "#proc"),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Proc {
    params: Vec<Symbol>,
    body: Box<Exp>,
    env: Environment,
}

impl Proc {
    pub fn new(params: Vec<Symbol>, body: Exp, env: Environment) -> Self {
        Proc {
            params,
            body: Box::new(body),
            env,
        }
    }

    pub fn eval(&self, args: &[Exp]) -> SchemeResult<Exp> {
        let mut env = Environment::from(&self.params, args, self.env.clone());
        eval(&mut env, &self.body)
    }
}

use std::collections::hash_map::HashMap;
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    env: HashMap<Symbol, Exp>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            env: HashMap::new(),
            outer: None,
        }
    }

    pub fn from(params: &[Symbol], args: &[Exp], outer: Environment) -> Self {
        if params.len() != args.len() {
            panic!("Mismatched length creating Environment");
        }
        let mut this = Environment::new();

        for (i, p) in params.iter().enumerate() {
            this.env.insert(p.to_string(), args[i].clone());
        }

        this.outer = Some(Box::new(outer));
        this
    }

    pub fn get(&self, var: &Symbol) -> &Exp {
        let env = if self.env.contains_key(var) {
            self
        } else {
            self.outer.as_ref().unwrap().find(var)
        };
        &env.env[var]
    }

    pub fn insert(&mut self, var: Symbol, val: Exp) {
        self.env.insert(var, val);
    }

    pub fn find(&self, var: &Symbol) -> &Environment {
        return if self.env.contains_key(var) {
            self
        } else {
            self.outer.as_ref().unwrap().find(var)
        };
    }

    pub fn find_mut(&mut self, var: &Symbol) -> &mut Environment {
        return if self.env.contains_key(var) {
            self
        } else {
            self.outer.as_mut().unwrap().find_mut(var)
        };
    }
}
