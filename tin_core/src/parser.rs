use crate::datum::{Datum, Vector};
use crate::error::{TinError, TinResult};
use crate::scanner::{tokenize, Token};
use crate::symbol;
use std::convert::TryInto;
use std::iter::Peekable;

pub fn parse_str(s: &str) -> TinResult<Datum> {
    let toks = tokenize(s);
    parse_form(toks)
}

pub fn parse_form<I>(src: I) -> TinResult<Datum>
where
    I: IntoIterator<Item = Token>,
{
    parse_datum(&mut src.into_iter().peekable())
}

pub fn parse_datum<I>(it: &mut Peekable<I>) -> TinResult<Datum>
where
    I: Iterator<Item = Token>,
{
    if let Some(tok) = it.next() {
        match tok {
            Token::Identifier(i) => Ok(Datum::symbol(i.try_into()?)),
            Token::String(s) => Ok(Datum::string(s)),
            Token::Bool(b) => Ok(b.into()),
            Token::Number(n) => Ok(n.into()),
            Token::Char(c) => Ok(c.into()),

            Token::Popen => Ok(parse_list(it.by_ref())?),
            Token::Pclose => Err(TinError::SyntaxError("unexpected ')' found".to_string())),
            Token::Vector => Ok(Datum::Vector(parse_vector(it.by_ref())?)),
            Token::ByteVec => todo!(),
            Token::Quote => {
                let (left, right) = (symbol!(quote), parse_datum(it)?);
                Ok(Datum::List(vec![left, right]))
            }
            Token::Quasi => {
                let (left, right) = (symbol!(quasiquote), parse_datum(it)?);
                Ok(Datum::List(vec![left, right]))
            }
            Token::Unquote => {
                let (left, right) = (symbol!(unquote), parse_datum(it)?);
                Ok(Datum::List(vec![left, right]))
            }
            Token::Splice => {
                let (left, right) = (symbol!(unquote - splicing), parse_datum(it)?);
                Ok(Datum::List(vec![left, right]))
            }
            Token::Dot => Err(TinError::SyntaxError("ill formed syntax".to_string())),
            Token::Error(err) => Err(TinError::SyntaxError(err)),
        }
    } else {
        Err(TinError::EOF)
    }
}

fn parse_vector<I>(it: &mut Peekable<I>) -> TinResult<Vector>
where
    I: Iterator<Item = Token>,
{
    let mut acc = Vec::new();

    loop {
        match it.peek() {
            Some(Token::Pclose) => {
                it.next();
                return Ok(acc);
            }
            Some(Token::Error(e)) => return Err(TinError::SyntaxError(e.clone())),
            None => return Err(TinError::EOF),
            _ => acc.push(parse_datum(it.by_ref())?),
        }
    }
}

fn parse_list<I>(it: &mut Peekable<I>) -> TinResult<Datum>
where
    I: Iterator<Item = Token>,
{
    let mut acc = Vec::new();
    let mut tail = None;
    loop {
        match it.peek() {
            Some(Token::Pclose) => {
                it.next();
                return match (tail, acc.len()) {
                    (None, 0) => Ok(Datum::Nil),
                    (None, _) => Ok(Datum::List(acc)),
                    (Some(_), 0) => {
                        return Err(TinError::SyntaxError("ill formed dotted list".to_string()))
                    }
                    (Some(t), _) => Ok(Datum::DotList(acc, Box::new(t))),
                };
            }
            Some(Token::Dot) => {
                it.next();
                let t = parse_datum(it.by_ref())?;
                tail = Some(t);
            }
            Some(Token::Error(e)) => return Err(TinError::SyntaxError(e.clone())),
            None => return Err(TinError::EOF),
            _ => acc.push(parse_datum(it.by_ref())?),
        }
    }
}

#[cfg(test)]
mod tests {

    use std::convert::TryInto;

    use crate::datum::Datum;
    use crate::scanner::Token;
    use crate::symbol;

    use super::parse_form;

    #[test]
    fn test_parse_list() {
        let toks = vec![
            Token::Popen,
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            Token::Pclose,
        ];
        let expect = vec![symbol!(a), symbol!(b), symbol!(c)];
        let got = parse_form(toks);
        assert_eq!(got, Ok(Datum::List(expect)))
    }

    #[test]
    fn test_parse_nested() {
        let toks = vec![
            Token::Popen,
            Token::Identifier("a".to_string()),
            Token::Popen,
            Token::Identifier("b".to_string()),
            Token::Pclose,
            Token::Identifier("c".to_string()),
            Token::Pclose,
        ];

        let inner = vec![symbol!(b)];
        let outer = vec![symbol!(a), Datum::List(inner), symbol!(c)];
        let expect = Datum::List(outer);
        let got = parse_form(toks);
        assert_eq!(got, Ok(expect))
    }

    #[test]
    fn test_parse_dotlist() {
        let toks = vec![
            Token::Popen,
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            Token::Dot,
            Token::Identifier("d".to_string()),
            Token::Pclose,
        ];
        let lst = vec![symbol!(a), symbol!(b), symbol!(c)];
        let expect = Datum::DotList(lst, Box::new(Datum::symbol("d".try_into().unwrap())));
        let got = parse_form(toks);
        assert_eq!(got, Ok(expect))
    }

    #[test]
    fn test_parse_quote() {
        let toks = vec![
            Token::Quote,
            Token::Popen,
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            Token::Pclose,
        ];
        let lst = vec![symbol!(a), symbol!(b), symbol!(c)];
        let expect = Datum::List(vec![symbol!(quote), Datum::List(lst)]);
        let got = parse_form(toks);
        assert_eq!(got, Ok(expect))
    }

    #[test]
    fn test_parse_bytevec() {
        let toks = vec![
            Token::Quote,
            Token::Popen,
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            Token::Pclose,
        ];
    }
}
