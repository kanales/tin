use crate::lib::procs;
use crate::lib::types::*;
use crate::list;
use TinError::{ArityMismatch, NotAProcedure};

pub fn eval(env: EnvironmentRef, x: Exp) -> TinResult<Exp> {
    match x {
        Exp::Atom(Atom::Symbol(s)) => env
            .borrow()
            .get(&s)
            .ok_or(TinError::Undefined(s.to_string())),
        Exp::String(_) => Ok(x),
        Exp::Atom(_) => Ok(x.clone()),
        Exp::Closure(_) => Ok(x.clone()),
        Exp::Proc(_) => Ok(x.clone()),
        Exp::List(lst) => match lst.snoc() {
            Some((head, tail)) => eval_list(env, head, tail),
            _ => Err(NotAProcedure(Exp::List(List::new()))),
        },
    }
}

#[test]
fn eval_simple_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::new();
    let input = parse("( + 1 2 3 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(6)))),
        eval(env.clone(), input)
    )
}

#[test]
fn eval_define_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::new();
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
    let env = EnvironmentRef::new();

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
fn eval_list(env: EnvironmentRef, op: Exp, args: List) -> TinResult<Exp> {
    match op {
        Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, s, args),
        Exp::Proc(p) => p.eval(&args.as_vec()),
        Exp::Closure(p) => p.eval(&args.as_vec()),
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
        "foo".to_string(),
        Closure::new(|args| match (args[0].clone(), args[1].clone()) {
            (Exp::Atom(Atom::Number(x)), Exp::Atom(Atom::Number(y))) => Ok((x + y).into()),
            _ => unreachable!(),
        }),
    );
    let res = eval(env.clone(), parse("(foo 1 2)").unwrap());
    assert_eq!(Ok(3.into()), res);
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
        "define" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            let def = args.pop().unwrap();
            let body = args.pop().unwrap();
            if let Exp::Atom(Atom::Symbol(s)) = def {
                if let Exp::Proc(p) = body {
                    env.borrow_mut().insert(s, Exp::Proc(p));
                } else {
                    env.borrow_mut().insert(s, eval(env.clone(), body)?);
                }
                Ok(Exp::List(List::new()))
            } else {
                Err(TinError::NotASymbol(def))
            }
        }
        "begin" => {
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
                env.borrow_mut().update(sym.to_string(), exp.clone());
                Ok(exp)
            } else {
                Err(TinError::TypeMismatch(
                    "Symbol".to_string(),
                    format!("{}", def),
                ))
            }
        }
        "display" => {
            if args.len() != 1 {
                return Err(ArityMismatch(1, args.len()));
            }
            println!("{}", eval(env, args.pop().unwrap())?);
            Ok(Exp::List(List::new()))
        }
        "lambda" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            let params = args.pop().unwrap();
            let body = args.pop().unwrap();

            if let Exp::List(params) = params {
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
            } else {
                Err(TinError::TypeMismatch(
                    "List".to_string(),
                    format!("{}", body),
                ))
            }
        }
        x => {
            let mut args: Vec<_> = args
                .map(|a| eval(env.clone(), a))
                .collect::<TinResult<_>>()?;

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
                    if args.len() != 1 {
                        return Err(TinError::ArityMismatch(1, args.len()));
                    }
                    let lst = &args[0];
                    if let Exp::List(lst) = lst {
                        Ok((lst.len() as i64).into())
                    } else {
                        Err(TinError::TypeMismatch("List".to_string(), lst.to_string()))
                    }
                }
                "list" => Ok(Exp::List(args.into())),
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
                "null?" => Ok((args.len() == 1
                    && (if let Exp::List(x) = args.pop().unwrap() {
                        x.len() == 0
                    } else {
                        false
                    }))
                .into()),
                "number?" => Ok((args.len() == 0
                    && (if let Exp::Atom(Atom::Number(_)) = args.pop().unwrap() {
                        true
                    } else {
                        false
                    }))
                .into()),
                "round" => {
                    unimplemented!()
                }
                "symbol?" => Ok((args.len() == 0
                    && (if let Exp::Atom(Atom::Symbol(_)) = args.pop().unwrap() {
                        true
                    } else {
                        false
                    }))
                .into()),
                sym => {
                    let head = eval(
                        env.clone(),
                        env.borrow()
                            .get(&sym.to_string())
                            .ok_or(TinError::Undefined(sym.to_string()))?,
                    )?;
                    match head {
                        Exp::Proc(proc) => {
                            let mut vals: Vec<Exp> = Vec::new();
                            for arg in args {
                                vals.push(eval(env.clone(), arg)?);
                            }
                            proc.eval(&vals)
                        }
                        Exp::Closure(proc) => {
                            let mut vals: Vec<Exp> = Vec::new();
                            for arg in args {
                                vals.push(eval(env.clone(), arg)?);
                            }
                            proc.eval(&vals)
                        }
                        _ => Err(TinError::NotAProcedure(head)),
                    }
                }
            }
        }
    }
}
