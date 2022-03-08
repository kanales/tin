use persistent::list;

use crate::lib::procs;
use crate::lib::types::{Closure, Exp, List, Number, Symbol, TinError};
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

    pub fn from(params: &[Symbol], args: &[Exp], outer: EnvironmentRef) -> Self {
        EnvironmentRef {
            env: Rc::new(RefCell::new(Environment::from(params, args, outer))),
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
              if args.len() != 1 {
                 return Err(TinError::ArityMismatch(1, args.len()));
             }
             match &args[0] {
                Exp::List(lst) => Ok((lst.len() as i64).into()),
                Exp::Vector(v) => Ok((v.len() as i64).into()),
                Exp::Map(m) => Ok(m.len().into()),
                Exp::String(s) => Ok(s.len().into()),
                _ => Err(TinError::TypeMismatch("list | vector | hash".to_string(), args[0].to_string()))
             }
            }),
            "list" => Closure::new(|args| Ok(Exp::List(List::from(args.to_vec())))),
            "max" => Closure::new(procs::max),
            "min" => Closure::new(procs::min),
            "map" => Closure::new(|args| {
                if args.len() != 2 {
                    return Err(TinError::ArityMismatch(2, args.len()));
                }
                unimplemented!()

            }),
            "null?" => Closure::new(|args| {
                if args.len() == 1 {
                    if let Exp::List(x) = &args[0] {
                        return Ok((x.len() == 0).into())
                    }
                }
                Ok(false.into())
            }),
            "number?" => Closure::new(|args| {
                if args.len() == 1 {
                    if let Exp::Number(_) = &args[0] {
                        return Ok(true.into())
                    }
                }
                Ok(false.into())
            }),
            "round" => Closure::new(|args| {
                if args.len() == 1 {
                    match  Number::try_from(args[0].clone())? {
                        Number::Int(x) => Ok(x.into()),
                        Number::Float(f) => Ok(Number::Int(f.round() as i64).into())
                    }
                } else {
                    Err(TinError::ArityMismatch(1, args.len()))
                }
            }),
            "symbol?" => Closure::new(|args| {
                Ok((args.len() == 0 && (
                    if let Exp::Symbol(_) = args[0] {
                        true
                    } else {
                        false
                    })).into())
            }),
            "car" => Closure::new(|args| {
                if args.len() != 1 {
                    return Err(TinError::ArityMismatch(1, args.len()))
                }
                let lst: List = args[0].clone().try_into()?;

                 lst.head()
                     .map(|x| x.clone())
                     .ok_or(TinError::TypeMismatch("pair".to_string(),Exp::List(lst).to_string()))
            }),
            "cdr" => Closure::new(|args| {
                if args.len() != 1 {
                    return Err(TinError::ArityMismatch(1, args.len()))
                }
                let lst: List = args[0].clone().try_into()?;

                 Ok(lst.tail().into())
            }),
            "cons" => Closure::new(|args| {
                if args.len() != 2 {
                    return Err(TinError::ArityMismatch(1, args.len()))
                }
                let head = args[0].clone();
                let tail: List = args[1].clone().try_into()?;

                 Ok(tail.cons(head).into())
            }),
            "pi" => Exp::Number(f64::consts::PI.into()),
            "inf" => Exp::Number(f64::INFINITY.into()),
            "-inf" => Exp::Number(f64::NEG_INFINITY.into()),
            "float-max" => Exp::Number(f64::MAX.into()),
            "float-min" => Exp::Number(f64::MIN.into())
        };
        Environment {
            env,
            outer: None,
            counter: 0,
        }
    }

    pub fn from(params: &[Symbol], args: &[Exp], outer: EnvironmentRef) -> Self {
        if params.len() != args.len() {
            panic!("Mismatched length creating Environment");
        }
        let mut this = Environment::new();

        for (i, a) in args.into_iter().enumerate() {
            this.env.insert(params[i].clone().into(), a.clone());
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
        &vec!["x".into()],
        &vec![Exp::Symbol("x".into())],
        env.clone(),
    );
    env.borrow_mut().insert("y".into(), Exp::Symbol("y".into()));
    let res = inner.borrow().get(&"y".into()).unwrap();
    assert_eq!(res, Exp::Symbol("y".into()))
}
