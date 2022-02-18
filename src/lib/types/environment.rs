use crate::lib::types::{Atom, Exp, List, Symbol};
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

        for (i, a) in args.into_iter().enumerate() {
            this.env.insert(params[i].to_string(), a.clone());
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
