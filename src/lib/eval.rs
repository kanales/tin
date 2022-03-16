use std::convert::{TryFrom, TryInto};

use crate::lib::{
    types::{
        Closure, EnvironmentRef, Evaluable, Exp, Key, List, Macro, Map, Number, Proc, Symbol,
        TinError, TinResult,
    },
    utils,
};
use persistent::list;
use TinError::{ArityMismatch, NotCallable};

pub fn eval(env: EnvironmentRef, x: Exp) -> TinResult<Exp> {
    match x {
        Exp::Ident(s) => env
            .borrow()
            .get(&s)
            .ok_or(TinError::Undefined(s.into()).into()),
        Exp::List(lst) => match lst.snoc() {
            Some((head, tail)) => eval_list(env, head.clone(), tail),
            _ => NotCallable(Exp::List(List::new())).into(),
        },
        _ => Ok(x),
    }
}

fn eval_vector(env: EnvironmentRef, v: Vec<Exp>, args: List) -> TinResult<Exp> {
    if args.len() == 1 {
        let res = eval(env.clone(), args.head().unwrap().clone())?;
        let head = Number::try_from(res)?.floor();
        if let Number::Int(idx) = head {
            let idx = idx as usize;
            if idx >= v.len() {
                return TinError::OutOfRange(idx).into();
            }
            return Ok(v[idx].clone());
        }
        unreachable!()
    } else {
        TinError::ArityMismatch(1, args.len()).into()
    }
}

fn eval_map(_env: EnvironmentRef, m: Map, args: List) -> TinResult<Exp> {
    let key: Key = utils::list1(args)?.clone().try_into()?;
    if !m.includes(&key) {
        return TinError::KeyNotFound(key).into();
    }
    return Ok(m[&key].clone());
}

fn eval_unqote(env: EnvironmentRef, lst: List) -> TinResult<Exp> {
    if lst.len() == 1 {
        let head = lst.head().unwrap();
        eval(env, head.clone())
    } else {
        TinError::ArityMismatch(1, lst.len()).into()
    }
}

fn eval_quasi(env: EnvironmentRef, exp: Exp) -> TinResult<Exp> {
    match exp {
        Exp::List(lst) => {
            if let Some((head, rest)) = lst.snoc() {
                if let Exp::Ident(s) = head {
                    if s.as_ref() == "unquote" {
                        return eval_unqote(env, rest);
                    }
                }
            }
            let out: List = lst
                .iter()
                .map(|el| eval_quasi(env.clone(), el.clone()))
                .collect::<TinResult<_>>()?;
            Ok(out.into())
        }
        x => Ok(x),
    }
}
fn eval_list(env: EnvironmentRef, op: Exp, args: List) -> TinResult<Exp> {
    match op {
        Exp::Macro(m) => {
            let args: List = args.iter().map(|x| x.clone()).collect();
            eval(env, m.eval(args)?)
        }
        Exp::Ident(s) => eval_symbol(env, s, args),
        Exp::List(lst) => eval_list(env.clone(), eval(env, lst.into())?, args),
        Exp::Number(_) | Exp::Char(_) | Exp::Bool(_) => TinError::NotCallable(op).into(),
        op => {
            let args: List = args
                .iter()
                .map(|a| eval(env.clone(), a.clone()))
                .collect::<TinResult<_>>()?;
            match op {
                Exp::Proc(p) => p.eval(args),
                Exp::Closure(p) => p.eval(args),
                Exp::Vector(v) => eval_vector(env, v, args),
                Exp::Map(m) => eval_map(env, m, args),
                _ => unreachable!(),
            }
        }
    }
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

fn lambda(env: EnvironmentRef, pars: List, body: Exp) -> TinResult<Exp> {
    let params = pars
        .iter()
        .map(|x| match x {
            Exp::Ident(x) => Ok(x.clone()),
            x => TinError::NotASymbol(x.clone()).into(),
        })
        .collect::<TinResult<_>>()?;
    Ok(Exp::Proc(Proc::new(
        params,
        body,
        env.clone(), // TODO check
    )))
}

fn lambda_va(env: EnvironmentRef, pars: List, va: Exp, body: Exp) -> TinResult<Exp> {
    let params = pars
        .iter()
        .map(|x| match x {
            Exp::Ident(x) => Ok(x.clone()),
            x => TinError::NotASymbol(x.clone()).into(),
        })
        .collect::<TinResult<_>>()?;
    let va = va.try_into()?;
    Ok(Exp::Proc(Proc::new_va(
        params,
        va,
        body,
        env.clone(), // TODO check
    )))
}

fn defmacro(env: EnvironmentRef, name: Symbol, params: List, rule: Exp) -> TinResult<()> {
    let parms = params
        .iter()
        .map(|p| p.clone().try_into())
        .collect::<TinResult<_>>()?;
    let m = Macro::new(env.clone(), parms, rule);
    define(env, name, m.into())
}

fn eval_symbol(env: EnvironmentRef, op: Symbol, args: List) -> TinResult<Exp> {
    match op.as_ref() {
        "quote" => {
            if args.len() > 1 {
                ArityMismatch(2, args.len()).into()
            } else {
                Ok(args.head().unwrap().clone())
            }
        }
        "quasi" => {
            if args.len() > 1 {
                ArityMismatch(2, args.len()).into()
            } else {
                let head = args.head().unwrap().clone();
                eval_quasi(env, head)
            }
        }
        "make_vector" => {
            let mut out = Vec::new();
            for arg in args.iter() {
                out.push(eval(env.clone(), arg.clone())?);
            }
            Ok(Exp::Vector(out))
        }
        "make_hash" => {
            if args.len() % 2 != 0 {
                return TinError::SyntaxError("Unbalanced map".to_string()).into();
            }
            let mut m = Map::new();
            for c in args.as_vec().chunks(2) {
                let key = eval(env.clone(), c[0].clone())?;
                let val = eval(env.clone(), c[1].clone())?;
                m.try_insert(key, val)?;
            }

            return Ok(Exp::Map(m));
        }
        "let" => {
            if args.len() < 2 {
                return ArityMismatch(3, args.len()).into();
            }
            let mut args = args.iter();
            let v = match args.next().unwrap().clone() {
                Exp::Vector(v) => v,
                Exp::List(mut lst) => {
                    if lst.head() == Some(&Symbol::new("make_vector".to_string()).into()) {
                        lst = lst.tail();
                    }
                    let mut out = Vec::new();
                    for arg in lst.iter() {
                        out.push(arg.clone());
                    }
                    out
                }
                x => {
                    return TinError::TypeMismatch(vec!["vector".to_string()], x.to_string()).into()
                }
            };

            if v.len() % 2 != 0 {
                return TinError::SyntaxError("Unbalanced let".to_string()).into();
            }
            let body = args.next().unwrap();

            let mut params = Vec::new();
            let mut values = Vec::new();
            for c in v.chunks(2) {
                params.push(c[0].clone());
                values.push(c[1].clone());
            }

            let lambda = lambda(env.clone(), params.into(), body.clone())?;

            values.insert(0, lambda);
            eval(env, Exp::List(values.into()))
        }
        "if" => {
            if args.len() != 3 {
                return ArityMismatch(3, args.len()).into();
            }
            let (test, args) = args.snoc().unwrap();
            let (ok, args) = args.snoc().unwrap();
            let (ko, _) = args.snoc().unwrap();
            if eval(env.clone(), test.clone())?.truthy() {
                eval(env, ok.clone())
            } else {
                eval(env, ko.clone())
            }
        }
        "define" | "def" => {
            if args.len() != 2 {
                return ArityMismatch(2, args.len()).into();
            }
            let (def, args) = args.snoc().unwrap();
            let def = def.clone();
            let (body, _) = args.snoc().unwrap();
            let body = body.clone();
            match def {
                Exp::Ident(s) => {
                    define(env, s, body)?;

                    Ok(Exp::List(List::new()))
                }
                Exp::DotList(lst, va) => match lst.snoc() {
                    None => TinError::Null.into(),
                    Some((def, args)) => {
                        let body = lambda_va(env.clone(), args, *va.clone(), body)?;
                        let def = def.clone().try_into()?;
                        define(env, def, body)?;

                        Ok(Exp::List(List::new()))
                    }
                },

                Exp::List(lst) => match lst.snoc() {
                    None => TinError::Null.into(),
                    Some((def, args)) => {
                        let body = lambda(env.clone(), args, body)?;
                        let def = def.clone().try_into()?;
                        define(env, def, body)?;

                        Ok(Exp::List(List::new()))
                    }
                },
                _ => TinError::NotASymbol(def).into(),
            }
        }
        "do" => {
            let mut last = Exp::List(list!());
            for arg in args.iter() {
                last = eval(env.clone(), arg.clone())?;
            }
            Ok(last)
        }
        "set!" => {
            if args.len() != 2 {
                return ArityMismatch(2, args.len()).into();
            }
            let (def, args) = args.snoc().unwrap();
            let val = args.head().unwrap();

            let sym: Symbol = def.clone().try_into()?;

            let exp = eval(env.clone(), val.clone())?;
            env.borrow_mut().update(sym.clone().into(), exp.clone());
            Ok(exp)
        }
        "print" => {
            if args.len() != 1 {
                return ArityMismatch(1, args.len()).into();
            }
            println!("{}", eval(env, args.head().unwrap().clone())?);
            Ok(Exp::List(List::new()))
        }
        "eprint" => {
            if args.len() != 1 {
                return ArityMismatch(1, args.len()).into();
            }
            eprintln!("{}", eval(env, args.head().unwrap().clone())?);
            Ok(Exp::List(List::new()))
        }
        "lambda" | "λ" => {
            if args.len() != 2 {
                return ArityMismatch(2, args.len()).into();
            }
            let (params, args) = args.snoc().unwrap();
            let (body, _) = args.snoc().unwrap();
            match params {
                Exp::List(lst) => lambda(env, lst.clone(), body.clone()),
                Exp::DotList(lst, va) => lambda_va(env, lst.clone(), *va.clone(), body.clone()),
                _ => TinError::TypeMismatch(vec!["list".to_string()], params.to_string()).into(),
            }
        }
        "gensym" => {
            if !args.is_empty() {
                unimplemented!()
            }

            Ok(Exp::Ident(env.gensym()))
        }
        "defmacro" => {
            if args.len() != 3 {
                return TinError::ArityMismatch(3, args.len()).into();
            }
            let mut args = args.iter();
            let name = args.next().unwrap().clone();
            let params = args.next().unwrap().clone();
            let rule = args.next().unwrap().clone();

            defmacro(env, name.try_into()?, params.try_into()?, rule)?;
            Ok(Exp::List(List::new()))
        }
        x => {
            let head = eval(
                env.clone(),
                env.borrow()
                    .get(&x.into())
                    .ok_or(TinError::Undefined(x.into()))?,
            )?;

            eval_list(env, head, args)
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lib::{
        eval::eval,
        types::{Closure, Environment, EnvironmentRef, Exp, Number},
        utils,
    };

    #[test]
    fn eval_simple_test() {
        use crate::lib::parser::parse;
        let env = Environment::default();
        let input = parse("( + 1 2 3 )").unwrap();
        assert_eq!(Ok(Exp::Number(Number::Int(6))), eval(env.as_ref(), input))
    }

    #[test]
    fn eval_define_test() {
        use crate::lib::parser::parse;
        let env = EnvironmentRef::default();
        eval(env.clone(), parse("( define x 3 )").unwrap()).unwrap();
        println!("env: {:?}", env);
        let input = parse("( + x 4 )").unwrap();

        assert_eq!(Ok(Exp::Number(Number::Int(7))), eval(env, input));
    }

    #[test]
    fn eval_rec_test() {
        use crate::lib::parser::parse;
        let env = EnvironmentRef::default();

        eval(
            env.clone(),
            parse("( define fact (lambda (x) (if (< x 2) 1 ( * x ( fact ( - x 1 ) ) ) ) ))")
                .unwrap(),
        )
        .unwrap();
        let input = parse("( fact 5 )").unwrap();
        assert_eq!(Ok(Exp::Number(Number::Int(120))), eval(env, input));
    }

    #[test]
    fn eval_rustproc() {
        use crate::lib::parser::parse;
        let env = EnvironmentRef::new();
        env.borrow_mut().insert(
            "foo".into(),
            Closure::new(|args| match utils::list2(args)? {
                (Exp::Number(x), Exp::Number(y)) => Ok((x + y).into()),
                _ => unreachable!(),
            }),
        );
        let exp = parse("(foo 1 2)").unwrap();
        println!("{}", exp);
        let res = eval(env.clone(), exp);
        assert_eq!(Ok(3.into()), res);
    }
}
