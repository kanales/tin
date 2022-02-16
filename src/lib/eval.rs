use crate::lib::procs;
use crate::lib::types::*;
use SchemeError::{ArityMismatch, NotAProcedure};

pub fn eval(env: EnvironmentRef, x: &Exp) -> SchemeResult<Exp> {
    match x {
        Exp::Atom(Atom::Symbol(s)) => env
            .borrow()
            .get(&*s)
            .ok_or(SchemeError::Undefined(s.to_string())),
        Exp::Atom(_) => Ok(x.clone()),
        Exp::Proc(p) => Ok(Exp::Proc(p.clone())),
        Exp::List(lst) => {
            if lst.is_empty() {
                return Err(NotAProcedure(x.clone()));
            }

            eval_list(env, &lst[0], &lst[1..])
        }
    }
}

#[test]
fn eval_simple_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::new();
    let input = parse("( + 1 2 3 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(6)))),
        eval(env.clone(), &input)
    )
}

#[test]
fn eval_define_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::new();
    eval(env.clone(), &parse("( define x 3 )").unwrap()).unwrap();
    let input = parse("( + x 4 )").unwrap();

    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(7)))),
        eval(env, &input)
    );
}

#[test]
fn eval_rec_test() {
    use crate::lib::parser::parse;
    let env = EnvironmentRef::new();

    eval(
        env.clone(),
        &parse("( define fact (lambda (x) (if (< x 2) 1 ( * x ( fact ( - x 1 ) ) ) ) ))").unwrap(),
    )
    .unwrap();
    let input = parse("( fact 5 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(120)))),
        eval(env, &input)
    );
}
fn eval_list(env: EnvironmentRef, op: &Exp, args: &[Exp]) -> SchemeResult<Exp> {
    match op {
        Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, s, args),
        Exp::Proc(p) => p.eval(args),
        x => match eval(env.clone(), &x)? {
            Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, &s.to_string(), args),
            Exp::Atom(x) => Err(SchemeError::NotAProcedure(Exp::Atom(x))),
            x => eval_list(env, &x, args),
        },
    }
}

fn eval_symbol(env: EnvironmentRef, op: &Symbol, args: &[Exp]) -> SchemeResult<Exp> {
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
            if eval(env.clone(), &args[0])?.truthy() {
                eval(env, &args[1])
            } else {
                eval(env, &args[2])
            }
        }
        "define" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            match &args[0] {
                Exp::Atom(Atom::Symbol(s)) => {
                    let arg = args[1].clone();
                    if let Exp::Proc(p) = arg {
                        env.borrow_mut().insert(s.to_string(), Exp::Proc(p));
                    } else {
                        env.borrow_mut()
                            .insert(s.to_string(), eval(env.clone(), &arg)?);
                    }
                    Ok(Exp::List(Vec::new()))
                }
                x => Err(SchemeError::NotASymbol(x.clone())),
            }
        }
        "do" => {
            let mut last = Exp::List(Vec::new());
            for arg in args {
                last = eval(env.clone(), &arg)?;
            }
            Ok(last)
        }
        "set!" => {
            if args.len() != 2 {
                return Err(ArityMismatch(2, args.len()));
            }
            if let Exp::Atom(Atom::Symbol(sym)) = &args[0] {
                let exp = eval(env.clone(), &args[1])?;
                env.borrow_mut().update(sym.to_string(), exp.clone());
                Ok(exp)
            } else {
                Err(SchemeError::TypeMismatch(
                    "Symbol".to_string(),
                    format!("{}", args[1]),
                ))
            }
        }
        "display" => {
            if args.len() != 1 {
                return Err(ArityMismatch(1, args.len()));
            }
            println!("{}", eval(env, &args[0])?);
            Ok(Exp::List(Vec::new()))
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
                Ok(Exp::Proc(Proc::new(
                    params,
                    args[1].clone(),
                    env.clone(), // TODO check
                )))
            } else {
                Err(SchemeError::TypeMismatch(
                    "List".to_string(),
                    format!("{}", args[0]),
                ))
            }
        }
        x => {
            let pars = args;
            let mut args: Vec<Exp> = Vec::new();

            for arg in pars {
                args.push(eval(env.clone(), &arg)?);
            }

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
                    let head = eval(
                        env.clone(),
                        &env.borrow()
                            .get(&sym.to_string())
                            .ok_or(SchemeError::Undefined(sym.to_string()))?,
                    )?;
                    if let Exp::Proc(proc) = head {
                        let mut vals: Vec<Exp> = Vec::new();
                        for arg in args {
                            vals.push(eval(env.clone(), &arg)?);
                        }
                        proc.eval(&vals)
                    } else {
                        Err(SchemeError::NotAProcedure(head))
                    }
                }
            }
        }
    }
}
