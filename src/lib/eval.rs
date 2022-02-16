use crate::lib::procs;
use crate::lib::types::*;
use SchemeError::{ArityMismatch, NotAProcedure};

pub fn eval(env: &mut Environment, x: &Exp) -> SchemeResult<Exp> {
    match x {
        Exp::Atom(Atom::Symbol(s)) => Ok(env.get(&*s).clone()),
        Exp::Atom(_) => Ok(x.clone()),
        Exp::Proc(p) => p.eval(&[]),
        Exp::List(lst) => {
            if lst.is_empty() {
                return Err(NotAProcedure(x.clone()));
            }

            // let op = self.eval(lst[0]))?;
            eval_list(env, &lst[0], &lst[1..])
        }
    }
}
#[test]
fn eval_test() {
    use crate::lib::parser::parse;
    let mut env = Environment::new();
    let input = parse("( + 1 2 3 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(6)))),
        eval(&mut env, &input)
    );

    eval(&mut env, &parse("( define x 3 )").unwrap()).unwrap();
    let input = parse("( + x 4 )").unwrap();

    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(7)))),
        eval(&mut env, &input)
    );
}
#[test]
fn eval_rec_test() {
    use crate::lib::parser::parse;
    let mut env = Environment::new();
    eval(
        &mut env,
        &parse("( define fact (lambda (x) (if (< x 2) 1 ( * x ( fact ( - 1 x) ) ) ) )").unwrap(),
    )
    .unwrap();
    let input = parse("( fact 5 )").unwrap();
    assert_eq!(
        Ok(Exp::Atom(Atom::Number(Number::Int(120)))),
        eval(&mut env, &input)
    );
}
fn eval_list(env: &mut Environment, op: &Exp, args: &[Exp]) -> SchemeResult<Exp> {
    match op {
        Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, s, args),
        Exp::Proc(p) => p.eval(args),
        x => {
            match eval(env, &x)? {
                Exp::Atom(Atom::Symbol(s)) => eval_symbol(env, &s.to_string(), args),
                Exp::Atom(x) => Err(SchemeError::NotAProcedure(Exp::Atom(x))),
                x => eval_list(env, &x, args),
            }
            // let vals: Vec<_> = args
            //     .iter()
            //     .map(|arg| self.eval(arg))
            //     .collect::<SchemeResult<_>>()?;
        }
    }
}

fn eval_symbol(env: &mut Environment, op: &Symbol, args: &[Exp]) -> SchemeResult<Exp> {
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
            if eval(env, &args[0])?.truthy() {
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
                    // TODO check the clone
                    let res = eval(env, &args[1])?;
                    env.insert(s.to_string(), res.clone());
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
                let exp = eval(env, &args[1])?;
                env.find_mut(&sym).insert(sym.to_string(), exp.clone());
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
            let args: Vec<_> = args
                .iter()
                .map(|arg| eval(env, arg))
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
                    let head = &env.get(&sym.to_string()).clone();
                    if let Exp::Proc(proc) = head.clone() {
                        let vals: Vec<_> = args
                            .iter()
                            .map(|arg| eval(env, arg))
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
