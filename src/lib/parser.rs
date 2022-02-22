use std::iter::Peekable;
use std::str::Chars;

use crate::lib::types::{Atom, Exp, List, Map, Number, TinError, TinResult};

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
    Atom(String),
    String(String),
}

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
                ' ' => self.next(),
                '"' => {
                    self.state = State::MatchingString(Vec::new());
                    self.next()
                }
                x => {
                    self.state = State::MatchingAtom(vec![x]);
                    self.next()
                }
            },
            State::MatchingAtom(mut curr) => match self.input.peek()? {
                '(' | '[' | ')' | ']' | '\'' | ' ' | '"' => {
                    self.state = State::Initial;
                    Some(Token::Atom(curr.into_iter().collect()))
                }
                _ => {
                    let x = self.input.next()?;
                    curr.push(x);
                    self.state = State::MatchingAtom(curr);
                    self.next()
                }
            },
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
        Some(Token::Quote) => Ok(list!["quote".to_string().into(), from_tokens(tokens)?].into()),
        Some(Token::Quasi) => Ok(list!["quasi".to_string().into(), from_tokens(tokens)?].into()),
        Some(Token::Unquote) => {
            Ok(list!["unquote".to_string().into(), from_tokens(tokens)?].into())
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
            let mut lst = List::new();
            for exp in v.into_iter().rev() {
                lst.push(exp);
            }

            lst.push(Exp::Atom(Atom::Symbol("make-vector".to_string())));

            Ok(Exp::List(lst))
        }
        Some(Token::Popen) => {
            let mut v = Vec::new();
            loop {
                if let Some(c) = tokens.peek() {
                    if *c == Token::Pclose {
                        break;
                    }
                }
                v.push(from_tokens(tokens)?);
            }
            tokens.next();

            let mut lst = List::new();
            for exp in v.into_iter().rev() {
                lst.push(exp);
            }
            return Ok(Exp::List(lst));
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
            let mut lst = List::new();
            for exp in v.into_iter().rev() {
                lst.push(exp);
            }

            lst.push(Exp::Atom(Atom::Symbol("make-hash".to_string())));

            Ok(Exp::List(lst))
        }
        Some(Token::Mclose) => Err(TinError::SyntaxError("Unexpected '}'".to_string())),
        Some(Token::Vclose) => Err(TinError::SyntaxError("Unexpected ']'".to_string())),
        Some(Token::Pclose) => Err(TinError::SyntaxError("Unexpected ')'".to_string())),
        Some(Token::Atom(a)) => Ok(Exp::Atom(atom(a))),
        Some(Token::String(s)) => Ok(Exp::String(s)),
    }
}

fn atom(token: String) -> Atom {
    if let Ok(r) = token.parse::<i64>() {
        return Atom::Number(Number::Int(r));
    }

    if let Ok(r) = token.parse::<f64>() {
        return Atom::Number(Number::Float(r));
    }

    match token.as_ref() {
        "#t" => return Atom::Bool(true),
        "#f" => return Atom::Bool(false),
        _ => (),
    }

    if token.len() == 2 && token.starts_with("\\") {
        return Atom::Char(token.chars().nth(1).unwrap());
    }

    if token.starts_with("#b") {
        let chrs = &token[2..];
        let mut acc = 0;
        for c in chrs.chars() {
            match c {
                '1' => acc = acc * 2 + 1,
                '0' => acc = acc * 2,
                _ => return Atom::Symbol(token),
            }
        }
        return Atom::Number(Number::Int(acc));
    }

    if token.starts_with("#o") {
        let chrs = &token[2..];
        let mut acc = 0;
        for c in chrs.chars() {
            match c {
                '0'..='7' => acc = acc * 8 + c.to_digit(8).unwrap(),
                _ => return Atom::Symbol(token),
            }
        }
        return Atom::Number(Number::Int(acc as i64));
    }

    if token.starts_with("#x") {
        let chrs = &token[2..];
        let mut acc = 0;
        for c in chrs.chars() {
            match c {
                '0'..='9' | 'a'..='f' => acc = acc * 16 + c.to_digit(16).unwrap(),
                _ => return Atom::Symbol(token),
            }
        }
        return Atom::Number(Number::Int(acc as i64));
    }

    Atom::Symbol(token)
}

pub fn parse(program: &str) -> TinResult<Exp> {
    from_tokens(&mut tokenize(program).peekable())
}

#[test]
fn parse_test() {
    let prog = "(begin (define r 10) (* pi (* r r)))";
    // let expect = Exp::List(list![
    //     Exp::Atom(Atom::Symbol("begin".to_string())),
    //     Exp::List(list![
    //         Exp::Atom(Atom::Symbol("define".to_string())),
    //         Exp::Atom(Atom::Symbol("r".to_string())),
    //         Exp::Atom(Atom::Number(Number::Int(10))),
    //     ]),
    //     Exp::List(list![
    //         Exp::Atom(Atom::Symbol("*".to_string())),
    //         Exp::Atom(Atom::Symbol("pi".to_string())),
    //         Exp::List(vec![
    //             Exp::Atom(Atom::Symbol("*".to_string())),
    //             Exp::Atom(Atom::Symbol("r".to_string())),
    //             Exp::Atom(Atom::Symbol("r".to_string())),
    //         ]),
    //     ]),
    // ]);

    // assert_eq!(parse(prog), Ok(expect));
}
