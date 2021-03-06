use crate::lib::types::{Atom, Exp, List, Map, Number, TinError, TinResult};
use persistent::list;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Popen,
    Pclose,
    Vopen,
    Mopen,
    Mclose,
    Vclose,
    Quote,
    Quasi,
    Unquote,
    Dot,
    Atom(String),
    String(String),
}

#[derive(Debug)]
enum State {
    Initial,
    MatchingAtom(Vec<char>), // could be Symbol or Number
    MatchingString(Vec<char>),
}

struct TokenIter<'a> {
    input: Peekable<Chars<'a>>,
    state: State,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut state = State::Initial;
        std::mem::swap(&mut self.state, &mut state);
        match state {
            State::Initial => match self.input.next()? {
                '(' => Some(Token::Popen),
                '[' => Some(Token::Vopen),
                '{' => Some(Token::Mopen),
                '}' => Some(Token::Mclose),
                ')' => Some(Token::Pclose),
                ']' => Some(Token::Vclose),
                '\'' => Some(Token::Quote),
                '`' => Some(Token::Quasi),
                ',' => Some(Token::Unquote),
                '.' => Some(Token::Dot),
                '"' => {
                    self.state = State::MatchingString(Vec::new());
                    self.next()
                }
                x if x.is_whitespace() => self.next(),
                x => {
                    self.state = State::MatchingAtom(vec![x]);
                    self.next()
                }
            },
            State::MatchingAtom(mut curr) => {
                if let Some(c) = self.input.peek() {
                    match c {
                        '(' | ')' | '[' | ']' | ',' | '\'' | '"' => {
                            self.state = State::Initial;
                            return Some(Token::Atom(curr.into_iter().collect()));
                        }
                        x if x.is_whitespace() => {
                            self.state = State::Initial;
                            return Some(Token::Atom(curr.into_iter().collect()));
                        }
                        _ => {}
                    }
                }
                let x = self.input.next()?;
                curr.push(x);
                self.state = State::MatchingAtom(curr);
                self.next()
            }
            State::MatchingString(mut curr) => match self.input.next()? {
                '"' => {
                    self.state = State::Initial;
                    Some(Token::String(curr.into_iter().collect()))
                }
                x => {
                    curr.push(x);
                    self.state = State::MatchingString(curr);
                    self.next()
                }
            },
        }
    }
}

fn tokenize<'a>(input: &'a str) -> TokenIter<'a> {
    TokenIter {
        input: input.chars().peekable(),
        state: State::Initial,
    }
}

#[test]
fn tokenize_test() {
    let input = "(begin (define r 10) (* pi (* r r)))";
    let expect = vec![
        Token::Popen,
        Token::Atom("begin".to_string()),
        Token::Popen,
        Token::Atom("define".to_string()),
        Token::Atom("r".to_string()),
        Token::Atom("10".to_string()),
        Token::Pclose,
        Token::Popen,
        Token::Atom("*".to_string()),
        Token::Atom("pi".to_string()),
        Token::Popen,
        Token::Atom("*".to_string()),
        Token::Atom("r".to_string()),
        Token::Atom("r".to_string()),
        Token::Pclose,
        Token::Pclose,
        Token::Pclose,
    ];

    let res: Vec<_> = tokenize(input).collect();

    assert_eq!(res, expect);
}

fn from_tokens(tokens: &mut Peekable<TokenIter>) -> TinResult<Exp> {
    match tokens.next() {
        None => Err(TinError::SyntaxError("Unexpected EOF".to_string())),
        Some(Token::Quote) => Ok(list![Exp::Symbol("quote".into()), from_tokens(tokens)?].into()),
        Some(Token::Quasi) => Ok(list![Exp::Symbol("quasi".into()), from_tokens(tokens)?].into()),
        Some(Token::Unquote) => {
            Ok(list![Exp::Symbol("unquote".into()), from_tokens(tokens)?].into())
        }
        Some(Token::Vopen) => {
            let mut v = Vec::new();
            loop {
                if let Some(c) = tokens.peek() {
                    if *c == Token::Vclose {
                        break;
                    }
                }
                v.push(from_tokens(tokens)?);
            }
            tokens.next();
            let lst: List = v.into();

            Ok(Exp::List(lst.cons(Exp::Symbol("make-vector".into()))))
        }
        Some(Token::Popen) => {
            let mut v = Vec::new();
            loop {
                if let Some(c) = tokens.peek() {
                    match *c {
                        Token::Dot => {
                            tokens.next();
                            let tok = from_tokens(tokens)?;
                            if tokens.next() != Some(Token::Pclose) {
                                return Err(TinError::SyntaxError("missing ')'".to_owned()));
                            }
                            return Ok(Exp::DotList(v.into(), Box::new(tok)));
                        }
                        Token::Pclose => break,
                        _ => {}
                    }
                }
                v.push(from_tokens(tokens)?);
            }
            tokens.next();

            return Ok(Exp::List(v.into()));
        }
        Some(Token::Mopen) => {
            let mut v = Vec::new();
            loop {
                if let Some(c) = tokens.peek() {
                    if *c == Token::Mclose {
                        break;
                    }
                }
                v.push(from_tokens(tokens)?);
            }
            tokens.next();

            let lst: List = v.into();
            Ok(Exp::List(lst.cons(Exp::Symbol("make-hash".into()))))
        }
        Some(Token::Dot) => Err(TinError::SyntaxError("Unexpected '.'".to_string())),
        Some(Token::Mclose) => Err(TinError::SyntaxError("Unexpected '}'".to_string())),
        Some(Token::Vclose) => Err(TinError::SyntaxError("Unexpected ']'".to_string())),
        Some(Token::Pclose) => Err(TinError::SyntaxError("Unexpected ')'".to_string())),
        Some(Token::Atom(a)) => Ok(atom(a)),
        Some(Token::String(s)) => Ok(Exp::String(s)),
    }
}

fn atom(token: String) -> Exp {
    if let Ok(r) = token.parse::<i64>() {
        return Exp::Number(Number::Int(r));
    }

    if let Ok(r) = token.parse::<f64>() {
        return Exp::Number(Number::Float(r));
    }

    match token.as_ref() {
        "#t" => return Exp::Bool(true),
        "#f" => return Exp::Bool(false),
        _ => (),
    }

    if token.len() == 2 && token.starts_with("\\") {
        return Exp::Char(token.chars().nth(1).unwrap());
    }

    if token.starts_with("#b") {
        let chrs = &token[2..];
        let mut acc = 0;
        for c in chrs.chars() {
            match c {
                '1' => acc = acc * 2 + 1,
                '0' => acc = acc * 2,
                _ => return Exp::Symbol(token.into()),
            }
        }
        return Exp::Number(Number::Int(acc));
    }

    if token.starts_with("#o") {
        let chrs = &token[2..];
        let mut acc = 0;
        for c in chrs.chars() {
            match c {
                '0'..='7' => acc = acc * 8 + c.to_digit(8).unwrap(),
                _ => return Exp::Symbol(token.into()),
            }
        }
        return Exp::Number(Number::Int(acc as i64));
    }

    if token.starts_with("#x") {
        let chrs = &token[2..];
        let mut acc = 0;
        for c in chrs.chars() {
            match c {
                '0'..='9' | 'a'..='f' => acc = acc * 16 + c.to_digit(16).unwrap(),
                _ => return Exp::Symbol(token.into()),
            }
        }
        return Exp::Number(Number::Int(acc as i64));
    }

    Exp::Symbol(token.into())
}

pub fn parse(program: &str) -> TinResult<Exp> {
    from_tokens(&mut tokenize(program).peekable())
}

#[test]
fn parse_test() {
    let prog = "(begin (define r 10) (* pi (* r r)))";
    // let expect = Exp::List(list![
    //     Exp(Atom::Symbol("begin".to_string())),
    //     Exp::List(list![
    //         Exp(Atom::Symbol("define".to_string())),
    //         Exp(Atom::Symbol("r".to_string())),
    //         Exp(Atom::Number(Number::Int(10))),
    //     ]),
    //     Exp::List(list![
    //         Exp(Atom::Symbol("*".to_string())),
    //         Exp(Atom::Symbol("pi".to_string())),
    //         Exp::List(vec![
    //             Exp(Atom::Symbol("*".to_string())),
    //             Exp(Atom::Symbol("r".to_string())),
    //             Exp(Atom::Symbol("r".to_string())),
    //         ]),
    //     ]),
    // ]);

    // assert_eq!(parse(prog), Ok(expect));
}
