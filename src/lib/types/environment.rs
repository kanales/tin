use persistent::list;

use crate::lib::types::{Closure, Exp, List, Number, Symbol, TinError};
use crate::lib::{procs, utils};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::hash_map::HashMap;
use std::convert::{Into, TryInto};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    env: HashMap<Symbol, Exp>,
    outer: Option<Box<EnvironmentRef>>,
    counter: usize,
}

use std::convert::TryFrom;
use std::rc::Rc;
#[derive(Debug, Clone, PartialEq)]
pub struct EnvironmentRef {
    env: Rc<RefCell<Environment>>,
}

use std::f64;

use super::{Evaluable, Function};

impl EnvironmentRef {
    pub fn new() -> Self {
        EnvironmentRef {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn gensym(&self) -> Symbol {
        self.env.borrow_mut().gensym()
    }
    pub fn default() -> Self {
        EnvironmentRef {
            env: Rc::new(RefCell::new(Environment::default())),
        }
    }

    pub fn from(outer: EnvironmentRef) -> Self {
        EnvironmentRef {
            env: Rc::new(RefCell::new(Environment::from(outer))),
        }
    }

    pub fn borrow_mut(&self) -> RefMut<Environment> {
        self.env.borrow_mut()
    }
    pub fn borrow(&self) -> Ref<Environment> {
        self.env.borrow()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            env: HashMap::new(),
            outer: None,
            counter: 0,
        }
    }
    pub fn gensym(&mut self) -> Symbol {
        let sym = Symbol::new(format!("G__{}", self.counter));
        self.counter += 1;
        sym
    }
    pub fn default() -> Self {
        let env = make_hash! {
            "+" => Closure::new(procs::add),
            "-" => Closure::new(procs::sub),
            "*" => Closure::new(procs::mul),
            "/" => Closure::new(procs::div),
            "abs" => Closure::new(procs::abs),
            "exp" => Closure::new(|_| unimplemented!()),
            ">" => Closure::new(procs::gt),
            "<" => Closure::new(procs::lt),
            ">=" => Closure::new(procs::ge),
            "<=" => Closure::new(procs::le),
            "=" => Closure::new(procs::eq),
            "not" => Closure::new(|_| unimplemented!()),
            "eq?" => Closure::new(procs::eq ),
            "len" => Closure::new(|args| {
             match utils::list1(args)? {
                Exp::List(lst) => Ok((lst.len() as i64).into()),
                Exp::Vector(v) => Ok((v.len() as i64).into()),
                Exp::Map(m) => Ok(m.len().into()),
                Exp::String(s) => Ok(s.len().into()),
                x => Err(TinError::TypeMismatch(vec![
                    "list".into(),
                    "vector".into(),
                    "hash".into()
                ], x.to_string()))
             }
            }),
            "list" => Closure::new(|args| Ok(Exp::List(List::from(args)))),
            "max" => Closure::new(procs::max),
            "min" => Closure::new(procs::min),
            "map" => Closure::new(|args| {
                if args.len() != 2 {
                    return Err(TinError::ArityMismatch(2, args.len()));
                }
                unimplemented!()

            }),
            "null?" => Closure::new(|args| {
                if let Exp::List(x) = utils::list1(args)? {
                    Ok((x.len() == 0).into())
                } else {
                    Ok(false.into())
                }
            }),
            "number?" => Closure::new(|args| {
                if let Exp::Number(_) = utils::list1(args)? {
                    return Ok(true.into())
                } else {
                    Ok(false.into())
                }
            }),
            "round" => Closure::new(|args| {
                let arg = utils::list1(args)?;
                match  Number::try_from(arg)? {
                    Number::Int(x) => Ok(x.into()),
                    Number::Float(f) => Ok(Number::Int(f.round() as i64).into())
                }
            }),
            "symbol?" => Closure::new(|args| {
                let arg = utils::list1(args)?;
                match arg {
                    Exp::Symbol(_) => Ok(true.into()),
                    _ => Ok(false.into())
                }
            }),
            "car" => Closure::new(|args| {
                let lst: List = utils::list1(args)?.try_into()?;

                 lst.head()
                     .map(|x| x.clone())
                     .ok_or(TinError::TypeMismatch(vec!["list".to_string()],Exp::List(lst).to_string()))
            }),
            "cdr" => Closure::new(|args| {
                let lst: List = utils::list1(args)?.try_into()?;

                 Ok(lst.tail().into())
            }),
            "cons" => Closure::new(|args| {
                if args.len() != 2 {
                    return Err(TinError::ArityMismatch(1, args.len()))
                }
                let (head, tail) = utils::list2(args)?;
                let tail: List = tail.try_into()?;

                 Ok(tail.cons(head).into())
            }),
            "pi" => Exp::Number(f64::consts::PI.into()),
            "inf" => Exp::Number(f64::INFINITY.into()),
            "-inf" => Exp::Number(f64::NEG_INFINITY.into()),
            "float-max" => Exp::Number(f64::MAX.into()),
            "float-min" => Exp::Number(f64::MIN.into()),

            "fold" => Closure::new(|args| {
                let (f, init, lst) = utils::list3(args)?;
                let f: Function = f.try_into()?;
                let lst: List = lst.try_into()?;
                let mut init = init;
                for el in lst.iter() {
                    init = f.eval(list!(init, el.clone()))?
                }
                Ok(init)
            }),
            "map" => Closure::new(|args| {
                let (f , lst) = utils::list2(args)?;
                let f: Function = f.try_into()?;
                let lst: List = lst.try_into()?;
                let mut acc = Vec::new();
                for el in lst.iter() {
                    acc.push(  f.eval(list!(el.clone()))?)
                }
                Ok(Exp::List(acc.into()))

            }),
            "filter" => Closure::new(|args| {
                let (f , lst) = utils::list2(args)?;
                let f: Function = f.try_into()?;
                let lst: List = lst.try_into()?;
                let mut acc = Vec::new();
                for el in lst.iter() {
                    let flag: bool = f.eval(list!(el.clone()))?.try_into()?;
                    if flag {
                       acc.push(el.clone())
                    }
                }
                Ok(Exp::List(acc.into()))

            })
        };
        Environment {
            env,
            outer: None,
            counter: 0,
        }
    }

    pub fn from(outer: EnvironmentRef) -> Self {
        let mut this = Environment::new();
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

    pub fn as_ref(self) -> EnvironmentRef {
        EnvironmentRef {
            env: Rc::new(RefCell::new(self)),
        }
    }
}
