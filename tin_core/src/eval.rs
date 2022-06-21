use std::rc::Rc;

use crate::datum::{Datum, Symbol};
use crate::error::{TinError, TinResult};
use crate::value::{App, Def, Ident, If, Pair, TinCell, TinValue};
use crate::{ensure_arity, ensure_symbol};

pub trait Environ {
    fn fetch(&self, sym: &Ident) -> Option<TinValue>;
    fn put(&mut self, sym: Ident, val: TinValue);
}
pub trait Evaluable {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ;
}

pub trait Interner {
    fn find(&self, sym: &Symbol) -> Option<Ident>;
    fn intern(&mut self, sym: &Symbol) -> Ident;
    fn unintern(&self, sym: Ident) -> &Symbol;
}

type NonEmpty<T> = Vec<T>;
fn eval_list<VM>(list: NonEmpty<Datum>, vm: &mut VM) -> TinResult<TinValue>
where
    VM: Interner + Environ,
{
    #[allow(unused_mut)]
    match &list[0] {
        Datum::Symbol(s) => match s.as_str() {
            "define" => {
                let (a, b) = ensure_arity!(2, list.into_iter().skip(1));
                let sym = ensure_symbol!(a);
                let val = b.eval_using(vm)?;
                Ok(Def::new(vm.intern(&sym), val).into())
            }
            "begin" => {
                let mut last = TinValue::Nil;
                for el in list.into_iter().skip(1) {
                    last = el.eval_using(vm)?;
                }
                Ok(last)
            }
            "quote" => {
                let a: Datum = ensure_arity!(1, list.into_iter().skip(1));
                Ok(TinValue::Quote(Rc::new(a)).into())
            }
            "if" => {
                let mut it = list.into_iter().skip(1);
                let (test, conseq) = ensure_arity!(2, it.by_ref());
                if let Some(v) = it.next() {
                    Ok(If::new_with_alternative(
                        test.eval_using(vm)?,
                        conseq.eval_using(vm)?,
                        v.eval_using(vm)?,
                    )
                    .into())
                } else {
                    Ok(If::new(test.eval_using(vm)?, conseq.eval_using(vm)?).into())
                }
            }
            _ => {
                let sym = vm.intern(&s);
                match vm.fetch(&sym) {
                    Some(v) => {
                        let exps: Vec<_> = list
                            .into_iter()
                            .skip(1)
                            .map(|e| e.eval_using(vm))
                            .collect::<TinResult<_>>()?;
                        Ok(App::new(v.into(), exps).into())
                    }
                    None => Err(TinError::UndefinedSymbol(sym.to_string())),
                }
            }
        },
        Datum::List(lst) => {
            let cmd = eval_list(lst.clone(), vm)?;
            let exps: Vec<_> = list
                .into_iter()
                .skip(1)
                .map(|e| e.eval_using(vm))
                .collect::<TinResult<_>>()?;
            Ok(App::new(cmd.into(), exps).into())
        }
        x @ (Datum::Bool(_)
        | Datum::Number(_)
        | Datum::Char(_)
        | Datum::String(_)
        | Datum::Bytes(_)
        | Datum::Nil
        | Datum::Vector(_)
        | Datum::DotList(_, _)) => Err(TinError::NotAProc(Box::new(x.clone()))),
    }
}

impl Evaluable for Datum {
    fn eval_using<VM>(&self, vm: &mut VM) -> TinResult<TinValue>
    where
        VM: Interner + Environ,
    {
        match self {
            Datum::Bool(b) => b.eval_using(vm),
            Datum::Number(n) => n.eval_using(vm),
            Datum::Char(c) => c.eval_using(vm),
            Datum::String(s) => Ok(TinValue::String(Rc::new(s.clone()))),
            Datum::Symbol(s) => s.eval_using(vm),
            Datum::Bytes(bs) => Ok(TinValue::Bytes(Rc::new(bs.clone()))),
            Datum::Nil => Ok(TinValue::Nil.into()),
            Datum::List(lst) => eval_list(lst.clone(), vm),
            Datum::DotList(lefts, right) => {
                if lefts.is_empty() {
                    panic!("ill formed dotted list.")
                }
                let right = right.eval_using(vm);
                lefts
                    .into_iter()
                    .rev()
                    .fold(right, |a, el| Ok(Pair::new(el.eval_using(vm)?, a?).into()))
            }
            Datum::Vector(v) => {
                let vals: Vec<TinCell> = v
                    .into_iter()
                    .map(|el| el.eval_using(vm).map(TinCell::new))
                    .collect::<TinResult<_>>()?;
                Ok(vals.into())
            }
        }
    }
}
