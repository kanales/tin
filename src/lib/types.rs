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
#[derive(Debug, Clone)]
pub enum Exp {
    Atom(Atom),
    List(List),
    Proc(Proc),
}

#[derive(Debug)]
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

#[derive(Debug, Clone)]
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
        Environment::from(&self.params, args, self.env.clone()).eval(&self.body)
    }
}

use std::collections::hash_map::HashMap;
#[derive(Debug, Clone)]
pub struct Environment {
    env: HashMap<Symbol, Exp>,
    outer: Option<Box<Environment>>,
}

use crate::lib::procs;
use SchemeError::{ArityMismatch, NotAProcedure};

impl Environment {
    pub fn new() -> Self {
        Environment {
            env: HashMap::new(),
            outer: None,
        }
    }

    fn from(params: &[Symbol], args: &[Exp], outer: Environment) -> Self {
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

    fn find(&mut self, var: &Symbol) -> &mut Environment {
        return if self.env.contains_key(var) {
            self
        } else {
            self.outer.as_mut().unwrap().find(var)
        };
    }

    pub fn eval(&mut self, x: &Exp) -> SchemeResult<Exp> {
        match x {
            Exp::Atom(Atom::Symbol(s)) => Ok(self.env[&*s].clone()),
            Exp::Atom(_) => Ok(x.clone()),
            Exp::Proc(p) => p.eval(&[]),
            Exp::List(lst) => {
                if lst.is_empty() {
                    return Err(NotAProcedure(x.clone()));
                }

                // let op = self.eval(lst[0]))?;
                self.eval_list(&lst[0], &lst[1..])
            }
        }
    }

    fn eval_list(&mut self, op: &Exp, args: &[Exp]) -> SchemeResult<Exp> {
        match op {
            Exp::Atom(Atom::Symbol(s)) => self.eval_symbol(s, args),
            Exp::Proc(p) => p.eval(args),
            x => {
                match self.eval(&x)? {
                    Exp::Atom(Atom::Symbol(s)) => self.eval_symbol(&s.to_string(), args),
                    Exp::Atom(x) => Err(SchemeError::NotAProcedure(Exp::Atom(x))),
                    x => self.eval_list(&x, args),
                }
                // let vals: Vec<_> = args
                //     .iter()
                //     .map(|arg| self.eval(arg))
                //     .collect::<SchemeResult<_>>()?;
            }
        }
    }

    fn eval_symbol(&mut self, op: &Symbol, args: &[Exp]) -> SchemeResult<Exp> {
        match op.as_ref() {
            "quote" => {
                if args.len() > 1 {
                    Err(ArityMismatch(2, args.len()))
                } else {
                    Ok(args[0].clone())
                }
            }
            "if" => {
                if args.len() != 3 {
                    return Err(ArityMismatch(3, args.len()));
                }
                if self.eval(&args[0])?.truthy() {
                    self.eval(&args[1])
                } else {
                    self.eval(&args[2])
                }
            }
            "define" => {
                if args.len() != 2 {
                    return Err(ArityMismatch(2, args.len()));
                }
                match &args[0] {
                    Exp::Atom(Atom::Symbol(s)) => {
                        // TODO check the clone
                        let res = self.eval(&args[1])?;
                        self.env.insert(s.to_string(), res.clone());
                        Ok(res)
                    }
                    x => Err(SchemeError::NotASymbol(x.clone())),
                }
            }
            "set!" => {
                if args.len() != 2 {
                    return Err(ArityMismatch(2, args.len()));
                }
                if let Exp::Atom(Atom::Symbol(sym)) = &args[0] {
                    let exp = self.eval(&args[1])?;
                    self.find(&sym).env.insert(sym.to_string(), exp.clone());
                    Ok(exp)
                } else {
                    Err(SchemeError::TypeMismatch(
                        "Symbol".to_string(),
                        format!("{}", args[1]),
                    ))
                }
            }
            "lambda" => {
                if args.len() != 2 {
                    return Err(ArityMismatch(2, args.len()));
                }
                if let Exp::List(lst) = &args[0] {
                    let mut params = Vec::new();
                    for p in lst {
                        if let Exp::Atom(Atom::Symbol(s)) = p {
                            params.push(s.to_string())
                        } else {
                            return Err(SchemeError::TypeMismatch(
                                "Symbol".to_string(),
                                format!("{}", p),
                            ));
                        }
                    }

                    Ok(Exp::Proc(Proc::new(params, args[1].clone(), self.clone())))
                } else {
                    Err(SchemeError::TypeMismatch(
                        "List".to_string(),
                        format!("{}", args[0]),
                    ))
                }
            }
            x => {
                let args: Vec<_> = args
                    .iter()
                    .map(|arg| self.eval(arg))
                    .collect::<SchemeResult<_>>()?;

                match x {
                    "+" => procs::add(&args),
                    "-" => procs::sub(&args),
                    "*" => procs::mul(&args),
                    "/" => procs::div(&args),
                    "abs" => procs::abs(&args),
                    ">" => procs::gt(&args),
                    "<" => procs::lt(&args),
                    ">=" => procs::ge(&args),
                    "<=" => procs::le(&args),
                    "=" => procs::eq(&args),
                    "append" => {
                        unimplemented!()
                    }
                    "begin" => {
                        unimplemented!()
                    }
                    "car" => {
                        unimplemented!()
                    }
                    "cdr" => {
                        unimplemented!()
                    }
                    "cons" => {
                        unimplemented!()
                    }
                    "eq?" => {
                        unimplemented!()
                    }
                    "expt" => {
                        unimplemented!()
                    }
                    "length" => {
                        unimplemented!()
                    }
                    "list" => {
                        unimplemented!()
                    }
                    "map" => {
                        unimplemented!()
                    }
                    "max" => {
                        unimplemented!()
                    }
                    "min" => {
                        unimplemented!()
                    }
                    "not" => {
                        unimplemented!()
                    }
                    "null?" => {
                        unimplemented!()
                    }
                    "number?" => {
                        unimplemented!()
                    }
                    "print" => {
                        unimplemented!()
                    }
                    "round" => {
                        unimplemented!()
                    }
                    "symbol?" => unimplemented!(),
                    sym => {
                        let head = &self.env[sym];
                        if let Exp::Proc(proc) = head.clone() {
                            let vals: Vec<_> = args
                                .iter()
                                .map(|arg| self.eval(arg))
                                .collect::<SchemeResult<_>>()?;
                            proc.eval(&vals)
                        } else {
                            Err(SchemeError::TypeMismatch(
                                "Proc".to_string(),
                                format!("{:?}", head),
                            ))
                        }
                    }
                }
            }
        }
    }
}
