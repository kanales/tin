use crate::lib::eval::eval;

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
    Undefined(Symbol),
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

use std::cell::{Ref, RefCell, RefMut};
use std::collections::hash_map::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    env: HashMap<Symbol, Exp>,
    outer: Option<Box<EnvironmentRef>>,
}

use std::rc::Rc;
#[derive(Debug, Clone, PartialEq)]
pub struct EnvironmentRef(Rc<RefCell<Environment>>);

impl EnvironmentRef {
    pub fn new() -> Self {
        EnvironmentRef(Rc::new(RefCell::new(Environment::new())))
    }

    pub fn from(params: &[Symbol], args: &[Exp], outer: EnvironmentRef) -> Self {
        EnvironmentRef(Rc::new(RefCell::new(Environment::from(
            params, args, outer,
        ))))
    }
    pub fn borrow_mut(&self) -> RefMut<Environment> {
        self.0.borrow_mut()
    }
    pub fn borrow(&self) -> Ref<Environment> {
        self.0.borrow()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            env: HashMap::new(),
            outer: None,
        }
    }

    pub fn from(params: &[Symbol], args: &[Exp], outer: EnvironmentRef) -> Self {
        if params.len() != args.len() {
            panic!("Mismatched length creating Environment");
        }
        let mut this = Environment::new();

        for (i, p) in params.iter().enumerate() {
            this.env.insert(p.to_string(), args[i].clone());
        }

        this.outer = Some(Box::new(outer.clone()));
        this
    }

    pub fn get<'a>(&'a self, var: &'a Symbol) -> Option<Exp> {
        if self.env.contains_key(var) {
            Some(self.env[var].clone())
        } else {
            self.outer.as_ref()?.borrow().get(var)
        }
    }

    pub fn insert(&mut self, var: Symbol, val: Exp) {
        self.env.insert(var, val);
    }

    pub fn update(&mut self, k: Symbol, v: Exp) {
        if self.env.contains_key(&k) {
            self.env.insert(k, v);
        } else {
            self.outer.as_ref().unwrap().borrow_mut().update(k, v)
        }
    }
}

#[test]
fn env_test() {
    let env = EnvironmentRef::new();
    let inner = EnvironmentRef::from(
        &vec!["x".to_string()],
        &vec![Exp::Atom(Atom::Symbol("x".to_string()))],
        env.clone(),
    );
    env.borrow_mut()
        .insert("y".to_string(), Exp::Atom(Atom::Symbol("y".to_string())));
    let res = inner.borrow().get(&"y".to_string()).unwrap();
    assert_eq!(res, Exp::Atom(Atom::Symbol("y".to_string())))
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

    pub fn eval(&self, args: &[Exp]) -> SchemeResult<Exp> {
        let env = EnvironmentRef::from(&self.params, args, self.env.clone());
        eval(env, &self.body)
    }
}
