mod lib;

fn main() {
    let mut interpreter = Interpreter::new();
    eprint!("> ");
    loop {
        let mut buffer = String::new();
        match std::io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                let exp = lib::parser::parse(&buffer).unwrap();
                let res = interpreter.eval(&exp).unwrap();
                println!("{}", res);
                eprint!("> ");
            }
            Err(error) => println!("error: {}", error),
        }
    }
}

use crate::lib::procs;
use crate::lib::types::{Atom, Exp, SchemeError, SchemeResult};

use std::collections::hash_map::HashMap;

struct Interpreter {
    env: HashMap<String, Exp>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
        }
    }

    fn eval<'a>(&'a mut self, x: &'a Exp) -> SchemeResult<Exp> {
        use SchemeError::{ArityMismatch, NotAProcedure};

        match x {
            Exp::Atom(Atom::Symbol(s)) => Ok(self.env[&*s].clone()),
            Exp::Atom(_) => Ok(x.clone()),

            Exp::List(lst) => {
                if lst.is_empty() {
                    return Err(NotAProcedure(x.clone()));
                }

                let head = match lst.first().unwrap() {
                    Exp::Atom(Atom::Symbol(s)) => s,
                    x => return Err(NotAProcedure(x.clone())),
                };

                let args = &lst[1..];
                match head.as_ref() {
                    "if" => {
                        if args.len() != 3 {
                            return Err(ArityMismatch(3, args.len()));
                        }
                        return Ok(if self.eval(&args[0])?.truthy() {
                            args[1].clone()
                        } else {
                            args[2].clone()
                        });
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
                    x => {
                        let _proc = &lst[0];
                        let args: Vec<_> = lst[1..]
                            .iter()
                            .map(|arg| match arg {
                                Exp::Atom(Atom::Symbol(s)) => self.env[&*s].clone(),
                                _ => arg.clone(),
                            })
                            .collect();

                        match x {
                            "+" => procs::add(&args),
                            "-" => procs::sub(&args),
                            "*" => procs::mul(&args),
                            "/" => procs::div(&args),
                            "abs" => procs::abs(&args),
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
                            "equal?" => {
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
                            "symbol?" => {
                                unimplemented!()
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
        }
    }
}
