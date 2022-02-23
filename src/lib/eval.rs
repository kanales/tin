use std::convert::{TryFrom, TryInto};

use crate::lib::types::{
    Atom, Closure, EnvironmentRef, Evaluable, Exp, Key, List, Map, Number, Proc, Symbol, TinError,
    TinResult,
};
use crate::list;
use TinError::{ArityMismatch, NotAProcedure};

pub fn eval(env: EnvironmentRef, x: Exp) -> TinResult<Exp> {
    match x {
        Exp::Atom(Atom::Symbol(s)) => env.borrow().get(&s).ok_or(TinError::Undefined(s.into())),
        Exp::List(lst) => match lst.snoc() {
            Some((head, tail)) => eval_list(env, head, tail),
            _ => Err(NotAProcedure(Exp::List(List::new()))),
        },
        _ => Ok(x),
    }
}

#[test]
fn eval_simple_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::default();
    let input = parse("( + 1 2 3 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(6)))),
        eval(env.clone(), input)
    )
}

#[test]
fn eval_define_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::default();
    eval(env.clone(), parse("( define x 3 )").unwrap()).unwrap();
    let input = parse("( + x 4 )").unwrap();

    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(7)))),
        eval(env, input)
    );
}

#[test]
fn eval_rec_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::default();

    eval(
        env.clone(),
        parse("( define fact (lambda (x) (if (< x 2) 1 ( * x ( fact ( - x 1 ) ) ) ) ))").unwrap(),
    )
    .unwrap();
    let input = parse("( fact 5 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(120)))),
        eval(env, input)
    );
}

fn eval_vector(env: EnvironmentRef, v: Vec<Exp>, args: List) -> TinResult<Exp> {
    if args.len() == 1 {
        let res = eval(env.clone(), args.head().unwrap().clone())?;
        let head = Number::try_from(res)?.floor();
        if let Number::Int(idx) = head {
            let idx = idx as usize;
            if idx >= v.len() {
                return Err(TinError::OutOfRange(idx));
            }
            return Ok(v[idx].clone());
        }
        unreachable!()
    } else {
        Err(TinError::ArityMismatch(1, args.len()))
    }
}

fn eval_map(env: EnvironmentRef, m: Map, args: List) -> TinResult<Exp> {
    if args.len() == 1 {
        let key: Key = eval(env.clone(), args.head().unwrap().clone())?.try_into()?;
        if !m.includes(&key) {
            return Err(TinError::KeyNotFound(key));
        }
        return Ok(m[&key].clone());
    } else {
        Err(TinError::ArityMismatch(1, args.len()))
    }
}

fn eval_list(env: EnvironmentRef, op: Exp, args: List) -> TinResult<Exp> {
    match op {
        Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, s, args),
        Exp::Proc(p) => p.eval(&args.as_vec()),
        Exp::Closure(p) => p.eval(&args.as_vec()),
        Exp::Macro(m) => m.expand(&args.as_vec()),
        Exp::Vector(v) => eval_vector(env, v, args),
        Exp::Map(m) => eval_map(env, m, args),
        x => match eval(env.clone(), x)? {
            Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, s, args),
            Exp::Atom(x) => Err(TinError::NotAProcedure(Exp::Atom(x))),
            x => eval_list(env, x, args),
        },
    }
}

#[test]
fn eval_rustproc() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::new();
    env.borrow_mut().insert(
        "foo".into(),
        Closure::new(|args| match (args[0].clone(), args[1].clone()) {
            (Exp::Atom(Atom::Number(x)), Exp::Atom(Atom::Number(y))) => Ok((x + y).into()),
            _ => unreachable!(),
        }),
    );
    let res = eval(env.clone(), parse("(foo 1 2)").unwrap());
    assert_eq!(Ok(3.into()), res);
}

fn define(env: EnvironmentRef, s: Symbol, body: Exp) -> TinResult<()> {
    if let Exp::Proc(p) = body {
        env.borrow_mut().insert(s, Exp::Proc(p));
    } else {
        let res = eval(env.clone(), body)?;
        env.borrow_mut().insert(s, res);
    }
    Ok(())
}

fn lambda(env: EnvironmentRef, params: List, body: Exp) -> TinResult<Exp> {
    let params = params
        .map(|x| match x {
            Exp::Atom(Atom::Symbol(x)) => Ok(x),
            x => Err(TinError::NotASymbol(x)),
        })
        .collect::<TinResult<Vec<_>>>()?;
    Ok(Exp::Proc(Proc::new(
        params,
        body,
        env.clone(), // TODO check
    )))
}

fn defmacro(env: EnvironmentRef, name: Symbol, params: List, rule: Exp) -> TinResult<()> {
    unimplemented!()
    // let m = Macro::new(params, rule);
    // define(env, name, m.into())
}

fn eval_symbol(env: EnvironmentRef, op: Symbol, mut args: List) -> TinResult<Exp> {
    match op.as_ref() {
        "quote" => {
            if args.len() > 1 {
                Err(ArityMismatch(2, args.len()))
            } else {
                Ok(args.head().unwrap().clone())
            }
        }
        "make-vector" => {
            let mut out = Vec::new();
            for arg in args {
                out.push(eval(env.clone(), arg.clone())?);
            }
            Ok(Exp::Vector(out))
        }
        "make-hash" => {
            if args.len() % 2 != 0 {
                println!("{:?}", args);
                return Err(TinError::SyntaxError("Unbalanced map".to_string()));
            }

            let mut m = Map::new();
            for c in args.as_vec().chunks(2) {
                let key = eval(env.clone(), c[0].clone())?;
                let val = eval(env.clone(), c[1].clone())?;
                m.try_insert(key, val)?;
            }

            return Ok(Exp::Map(m));
        }
        "if" => {
            if args.len() != 3 {
                return Err(ArityMismatch(3, args.len()));
            }
            let (test, ok, ko) = (
                args.pop().unwrap(),
                args.pop().unwrap(),
                args.pop().unwrap(),
            );
            if eval(env.clone(), test)?.truthy() {
                eval(env, ok)
            } else {
                eval(env, ko)
            }
        }
        "define" | "def" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            let def = args.pop().unwrap();
            let body = args.pop().unwrap();
            match def {
                Exp::Atom(Atom::Symbol(s)) => {
                    define(env, s, body)?;

                    Ok(Exp::List(List::new()))
                }
                Exp::List(lst) => match lst.snoc() {
                    None => Err(TinError::Null),
                    Some((def, args)) => {
                        let body = lambda(env.clone(), args, body)?;
                        let def = def.try_into()?;
                        define(env, def, body)?;

                        Ok(Exp::List(List::new()))
                    }
                },
                _ => Err(TinError::NotASymbol(def)),
            }
        }
        "do" => {
            let mut last = Exp::List(list!());
            for arg in args {
                last = eval(env.clone(), arg)?;
            }
            Ok(last)
        }
        "set!" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            let def = args.pop().unwrap();
            let val = args.pop().unwrap();

            if let Exp::Atom(Atom::Symbol(sym)) = def {
                let exp = eval(env.clone(), val)?;
                env.borrow_mut().update(sym.into(), exp.clone());
                Ok(exp)
            } else {
                Err(TinError::TypeMismatch(
                    "Symbol".to_string(),
                    format!("{}", def),
                ))
            }
        }
        "print" => {
            if args.len() != 1 {
                return Err(ArityMismatch(1, args.len()));
            }
            println!("{}", eval(env, args.pop().unwrap())?);
            Ok(Exp::List(List::new()))
        }
        "eprint" => {
            if args.len() != 1 {
                return Err(ArityMismatch(1, args.len()));
            }
            eprintln!("{}", eval(env, args.pop().unwrap())?);
            Ok(Exp::List(List::new()))
        }
        "lambda" | "λ" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            let params = args.pop().unwrap();
            let body = args.pop().unwrap();
            if let Exp::List(params) = params {
                lambda(env, params, body)
            } else {
                Err(TinError::TypeMismatch(
                    "List".to_string(),
                    format!("{}", body),
                ))
            }
        }
        "defmacro" => {
            if args.len() != 3 {
                return Err(TinError::ArityMismatch(3, args.len()));
            }
            let name: Symbol = args.pop().unwrap().try_into()?;
            let params = args.pop().unwrap().try_into()?;
            let rule = args.pop().unwrap();

            defmacro(env, name, params, rule)?;

            Ok(Exp::List(List::new()))
        }
        "macroexpand" => {
            unimplemented!()
        }
        x => {
            let head = eval(
                env.clone(),
                env.borrow()
                    .get(&x.into())
                    .ok_or(TinError::Undefined(x.into()))?,
            )?;
            match head {
                Exp::Proc(proc) => {
                    let args: Vec<_> = args
                        .map(|a| eval(env.clone(), a))
                        .collect::<TinResult<_>>()?;
                    proc.eval(&args)
                }
                Exp::Closure(proc) => {
                    let args: Vec<_> = args
                        .map(|a| eval(env.clone(), a))
                        .collect::<TinResult<_>>()?;
                    proc.eval(&args)
                }
                Exp::Macro(m) => {
                    let vals: Vec<Exp> = args.collect();

                    eval(env, m.expand(&vals)?)
                }
                Exp::Vector(v) => eval_vector(env, v, args),
                Exp::Map(m) => eval_map(env, m, args),
                _ => Err(TinError::NotAProcedure(head)),
            }
        }
    }
}
