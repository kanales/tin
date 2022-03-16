use crate::lib::types::{Atom, Exp, List, Map, Number, TinError, TinResult};
use persistent::list;
use std::ascii::AsciiExt;
use std::iter::Peekable;
use std::str::Chars;

use unicode_id::UnicodeID;

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
    Ident(String),
    Keyword(String),
    String(String),
    Symbol(char),
    Int(String),
    Float(String),
}

#[derive(Debug)]
enum State {
    Initial,      //
    MatchedColon, //

    MatchedSign(char),                //
    MatchingInteger(char, Vec<char>), //
    MatchingFloat(char, Vec<char>),   //

    MatchingString(Vec<char>),
    MatchingIdent(Vec<char>),
    MatchingKeyword(Vec<char>),
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
                ':' => {
                    self.state = State::MatchedColon;
                    self.next()
                }
                '"' => {
                    self.state = State::MatchingString(Vec::new());
                    self.next()
                }
                '+' => {
                    self.state = State::MatchedSign('+');
                    self.next()
                }
                '-' => {
                    self.state = State::MatchedSign('-');
                    self.next()
                }
                x if x.is_ascii_digit() => {
                    self.state = State::MatchingInteger('+', vec![x]);
                    self.next()
                }
                x if x.is_whitespace() => self.next(),
                x => {
                    if x.is_id_start() {
                        self.state = State::MatchingIdent(vec![x]);
                        self.next()
                    } else {
                        self.state = State::Initial;
                        return Some(Token::Symbol(x));
                    }
                }
            },
            State::MatchedColon => {
                let c = self.input.next()?;
                if c.is_id_start() {
                    self.state = State::MatchingKeyword(vec![c]);
                    self.next()
                } else {
                    return Some(Token::Keyword(c.to_string()));
                }
            }
            State::MatchedSign(s) => {
                if let Some(c) = self.input.peek() {
                    if c.is_ascii_digit() {
                        let c = self.input.next()?;
                        self.state = State::MatchingInteger(s, vec![c]);
                        return self.next();
                    }
                }
                self.state = State::Initial;
                return Some(Token::Symbol(s));
            }
            State::MatchingInteger(s, mut v) => {
                if let Some(c) = self.input.peek() {
                    if c.is_ascii_digit() {
                        let c = self.input.next()?;
                        v.push(c);
                        self.state = State::MatchingInteger(s, v);

                        return self.next();
                    }
                    if *c == '.' {
                        let c = self.input.next()?;
                        v.push(c);
                        self.state = State::MatchingFloat(s, v);
                        return self.next();
                    }
                }
                self.state = State::Initial;
                v.insert(0, s);
                return Some(Token::Int(v.iter().collect()));
            }
            State::MatchingFloat(s, mut v) => {
                if let Some(c) = self.input.peek() {
                    if c.is_ascii_digit() {
                        let c = self.input.next()?;
                        v.push(c);
                        self.state = State::MatchingFloat(s, v);

                        return self.next();
                    }
                }
                self.state = State::Initial;
                v.insert(0, s);
                return Some(Token::Float(v.iter().collect()));
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
            State::MatchingIdent(mut cs) => {
                if let Some(c) = self.input.peek() {
                    if UnicodeID::is_id_continue(*c) {
                        cs.push(self.input.next()?);
                        self.state = State::MatchingIdent(cs);
                        return self.next();
                    }
                }

                self.state = State::Initial;
                return Some(Token::Ident(cs.into_iter().collect()));
            }
            State::MatchingKeyword(mut cs) => {
                if let Some(c) = self.input.peek() {
                    if UnicodeID::is_id_continue(*c) {
                        cs.push(self.input.next()?);
                        self.state = State::MatchingKeyword(cs);
                        return self.next();
                    }
                }

                self.state = State::Initial;
                return Some(Token::Keyword(cs.into_iter().collect()));
            }
        }
    }
}

fn tokenize<'a>(input: &'a str) -> TokenIter<'a> {
    TokenIter {
        input: input.chars().peekable(),
        state: State::Initial,
    }
}

fn from_tokens(tokens: &mut Peekable<TokenIter>) -> TinResult<Exp> {
    match tokens.next() {
        None => TinError::SyntaxError("Unexpected EOF".to_string()).into(),
        Some(Token::Quote) => Ok(list![Exp::Ident("quote".into()), from_tokens(tokens)?].into()),
        Some(Token::Quasi) => Ok(list![Exp::Ident("quasi".into()), from_tokens(tokens)?].into()),
        Some(Token::Unquote) => {
            Ok(list![Exp::Ident("unquote".into()), from_tokens(tokens)?].into())
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

            Ok(Exp::List(lst.cons(Exp::Ident("make_vector".into()))))
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
                                return TinError::SyntaxError("missing ')'".to_owned()).into();
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
            Ok(Exp::List(lst.cons(Exp::Ident("make_hash".into()))))
        }
        Some(Token::Dot) => TinError::SyntaxError("Unexpected '.'".to_string()).into(),
        Some(Token::Mclose) => TinError::SyntaxError("Unexpected '}'".to_string()).into(),
        Some(Token::Vclose) => TinError::SyntaxError("Unexpected ']'".to_string()).into(),
        Some(Token::Pclose) => TinError::SyntaxError("Unexpected ')'".to_string()).into(),
        Some(Token::Ident(a)) => Ok(Exp::Ident(a.into())),
        Some(Token::String(s)) => Ok(Exp::String(s.into())),
        Some(Token::Symbol(s)) => Ok(Exp::Ident(s.to_string().into())),
        Some(Token::Int(s)) => match s.parse::<i64>() {
            Ok(i) => Ok(i.into()),
            Err(_) => unreachable!("failed to parse int {}", s),
        },
        Some(Token::Float(s)) => match s.parse::<f64>() {
            Ok(i) => Ok(i.into()),
            Err(_) => unreachable!("failed to parse int {}", s),
        },
        Some(Token::Keyword(kw)) => Ok(Exp::Keyword(kw.into())),
    }
}

pub fn parse(program: &str) -> TinResult<Exp> {
    from_tokens(&mut tokenize(program).peekable())
}

#[cfg(test)]
mod test {
    use persistent::list;

    use crate::lib::{
        parser::{parse, tokenize, Token},
        types::{Exp, List},
    };

    #[test]
    fn test_keywords() {
        let input = ":abc";
        assert_eq!(Ok(Exp::Keyword("abc".into())), parse(input));
    }

    #[test]
    fn tokenize_test() {
        let input = "(begin (define r 10) (* pi (* r r)))";
        let expect = vec![
            Token::Popen,
            Token::Ident("begin".to_string()),
            Token::Popen,
            Token::Ident("define".to_string()),
            Token::Ident("r".to_string()),
            Token::Int("+10".to_string()),
            Token::Pclose,
            Token::Popen,
            Token::Symbol('*'),
            Token::Ident("pi".to_string()),
            Token::Popen,
            Token::Symbol('*'),
            Token::Ident("r".to_string()),
            Token::Ident("r".to_string()),
            Token::Pclose,
            Token::Pclose,
            Token::Pclose,
        ];

        let res: Vec<_> = tokenize(input).collect();

        assert_eq!(res, expect);
    }
    #[test]
    fn test_parse_form() {
        // let exp = parse("(foo )");
        let exp = parse("(foo 2 3.1)");
        assert_eq!(
            Ok(Exp::List(list!(
                Exp::Ident("foo".into()),
                Exp::Number(2.into()),
                Exp::Number(3.1.into())
            ))),
            exp
        );
    }

    #[test]
    fn test_parse_map() {
        let map = parse("{ :a 1 :b 2.1 }");
        assert_eq!(
            Ok(Exp::List(list!(
                Exp::Ident("make_hash".into()),
                Exp::Keyword("a".into()),
                Exp::Number(1.into()),
                Exp::Keyword("b".into()),
                Exp::Number(2.1.into())
            ))),
            map
        );
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
}
