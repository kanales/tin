#![allow(dead_code)]

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, PartialEq, Eq)]
enum LispVal {
    Atom(String),
    List(Vec<LispVal>),
    DottedList(Vec<LispVal>, Box<LispVal>),
    Number(isize),
    String(String),
    Bool(bool),
}

mod parser {
    use crate::LispVal;

    #[derive(Debug, PartialEq, Eq)]
    enum ParseError {
        NoMatch(String, String),
        EOF,
        SyntaxError(String),
    }
    use ParseError::*;

    type ParseResult<'a, A> = Result<(A, &'a str), ParseError>;

    fn is_symbol(c: char) -> bool {
        let syms: Vec<char> = "!$%&|*+-/:<=?>@^_~#".chars().collect();
        syms.contains(&c)
    }

    fn parse_symbol(input: &str) -> ParseResult<char> {
        if input.is_empty() {
            return Err(EOF);
        }
        let (h, t) = input.split_at(1);
        let c = h.chars().next().unwrap();
        if is_symbol(c) {
            Ok((c, t))
        } else {
            Err(NoMatch(h.to_string(), "Symbol".to_string()))
        }
    }

    #[test]
    fn symbol() {
        assert_eq!(Ok(('$', "")), parse_symbol("$"));
        assert_eq!(
            Err(NoMatch("a".to_string(), "Symbol".to_string())),
            parse_symbol("a")
        );
    }

    /// Remove one or more spaces
    fn parse_whitespace(input: &str) -> ParseResult<()> {
        Ok(((), input.trim_start()))
    }

    /// Remove one or more spaces
    fn parse_whitespace1(input: &str) -> ParseResult<()> {
        if input.len() == 0 {
            return Err(EOF);
        }

        if let Some(c) = input.chars().next() {
            if !c.is_whitespace() {
                return Err(NoMatch(
                    format!("{}", c).to_string(),
                    "Whitespace".to_string(),
                ));
            }
        } else {
            return Err(EOF);
        }

        Ok(((), input.trim_start()))
    }
    #[test]
    fn whitespace() {
        let res = parse_whitespace1("    %").and_then(|(_, input)| parse_symbol(input));
        assert_eq!(Ok(('%', "")), res);

        let res = parse_whitespace1("%").and_then(|(_, input)| parse_symbol(input));
        assert_eq!(Err(NoMatch("%".to_string(), "Whitespace".to_string())), res);
    }

    ///parses a string
    fn parse_string(input: &str) -> ParseResult<LispVal> {
        // TODO allow for character escapes
        let mut chars = input.char_indices();
        match chars.next() {
            Some((_, '"')) => (),
            Some((_, x)) => return Err(NoMatch(format!("{}", x), "Quote".to_string())),
            None => return Err(EOF),
        }

        let mut acc: Vec<String> = Vec::new();

        while let Some((i, x)) = chars.next() {
            if x == '"' {
                let s: String = acc.concat();
                return Ok((LispVal::String(s), &input[i + 1..]));
            } else {
                acc.push(x.to_string());
            }
        }
        Err(EOF)
    }

    #[test]
    fn string() {
        assert_eq!(
            Ok((LispVal::String("abcde".to_string()), "fge")),
            parse_string("\"abcde\"fge")
        );
    }

    fn parse_atom(input: &str) -> ParseResult<LispVal> {
        if input.len() == 0 {
            return Err(EOF);
        }
        let mut chars = input.char_indices();

        let (mut idx, x) = chars.next().unwrap();
        if !x.is_alphabetic() && !is_symbol(x) {
            return Err(NoMatch(x.to_string(), "atom".to_string()));
        }

        let mut acc: Vec<String> = vec![x.to_string()];

        loop {
            match chars.next() {
                Some((i, x)) if x.is_alphanumeric() || is_symbol(x) => {
                    acc.push(x.to_string());
                    idx = i;
                }
                _ => break,
            }
        }
        let s = acc.concat();
        let val = match s.as_str() {
            "#t" => LispVal::Bool(true),
            "#f" => LispVal::Bool(false),
            _ => LispVal::Atom(s),
        };
        Ok((val, &input[idx + 1..]))
    }
    #[test]
    fn atom() {
        let input = "abc-1234\")";
        let expect = Ok((LispVal::Atom("abc-1234".to_string()), "\")"));
        assert_eq!(expect, parse_atom(input));
    }
    fn parse_number(input: &str) -> ParseResult<LispVal> {
        if input.len() == 0 {
            return Err(EOF);
        }
        let mut chars = input.char_indices();

        let (mut idx, x) = chars.next().unwrap();
        if !x.is_digit(10) {
            return Err(NoMatch(x.to_string(), "Number".to_string()));
        }

        let mut acc: Vec<String> = vec![x.to_string()];

        loop {
            match chars.next() {
                Some((i, x)) if x.is_digit(10) => {
                    acc.push(x.to_string());
                    idx = i;
                }
                _ => break,
            }
        }
        let s = acc.concat();
        let val = s.parse::<isize>().unwrap();
        Ok((LispVal::Number(val), &input[idx + 1..]))
    }

    #[test]
    fn number() {
        let input = "1234)";
        let expect = Ok((LispVal::Number(1234), ")"));
        assert_eq!(expect, parse_number(input));
    }

    fn parse_quoted(input: &str) -> ParseResult<LispVal> {
        if input.len() == 0 {
            return Err(EOF);
        }
        let mut chars = input.char_indices();
        if let Some((i, '\'')) = chars.next() {
            let (expr, res) = parse_expr(&input[i + 1..])?;
            Ok((
                LispVal::List(vec![LispVal::Atom("quote".to_string()), expr]),
                res,
            ))
        } else {
            Err(SyntaxError(input.to_string()))
        }
    }

    fn parse_char(input: &str, c: char) -> ParseResult<char> {
        if input.len() == 0 {
            return Err(EOF);
        }
        let p = input.chars().next().unwrap();
        if p != c {
            return Err(NoMatch(p.to_string(), c.to_string()));
        }

        Ok((p, &input[1..]))
    }

    fn parse_list(input: &str) -> ParseResult<LispVal> {
        // TODO allow for character escapes
        let (_, input) = parse_char(input, '(')?;
        let (_, mut input) = parse_whitespace(input)?;

        let mut list = vec![];
        loop {
            let (exp, res) = parse_expr(input)?;
            input = res;
            list.push(exp);

            let (_, res) = parse_whitespace(input)?;
            if let Ok((_, res)) = parse_char(res, ')') {
                return Ok((LispVal::List(list), res));
            } else {
                let (_, res) = parse_whitespace1(input)?;
                input = res;
            }
        }
    }

    #[test]
    fn list() {
        use LispVal::{Atom, List, Number};
        let input = "( a b 13 (c d e) )";
        let expect = List(vec![
            Atom("a".to_string()),
            Atom("b".to_string()),
            Number(13),
            List(vec![
                Atom("c".to_string()),
                Atom("d".to_string()),
                Atom("e".to_string()),
            ]),
        ]);
        assert_eq!(Ok((expect, "")), parse_list(input));
    }
    fn parse_dottedlist(input: &str) -> ParseResult<LispVal> {
        // TODO allow for character escapes
        let (_, res) = parse_char(input, '(')?;

        let (left, res) = parse_expr(res)?;
        let (_, res) = parse_whitespace1(res)?;
        let (_, res) = parse_char(res, '.')?;
        let (_, res) = parse_whitespace1(res)?;

        let mut input = res;
        let mut list = vec![];
        loop {
            let (exp, res) = parse_expr(input)?;
            input = res;

            list.push(exp);

            if let Ok((_, res)) = parse_char(input, ')') {
                return Ok((LispVal::DottedList(list, Box::new(left)), res));
            } else {
                let (_, res) = parse_whitespace1(input)?;
                input = res;
            }
        }
    }

    #[test]
    fn dottedlist() {
        use LispVal::*;
        let input = "( a (dotted . list) test )";
        let expect = List(vec![
            Atom("a".to_string()),
            DottedList(
                vec![Atom("list".to_string())],
                Box::new(Atom("dotted".to_string())),
            ),
            Atom("test".to_string()),
        ]);
        assert_eq!(Ok((expect, "")), parse_list(input));
    }

    fn parse_expr(input: &str) -> ParseResult<LispVal> {
        if let Ok((v, r)) = parse_string(input) {
            return Ok((v, r));
        }
        if let Ok((v, r)) = parse_atom(input) {
            return Ok((v, r));
        }
        if let Ok((v, r)) = parse_number(input) {
            return Ok((v, r));
        }
        if let Ok((v, r)) = parse_quoted(input) {
            return Ok((v, r));
        }
        if let Ok((v, r)) = parse_list(input) {
            return Ok((v, r));
        }

        if let Ok((v, r)) = parse_dottedlist(input) {
            return Ok((v, r));
        }
        Err(SyntaxError(input.to_string()))
    }

    #[test]
    fn expression() {
        assert_eq!(Ok((LispVal::Number(25), ")")), parse_expr("25)"));
        assert_eq!(
            Ok((LispVal::String("string".to_string()), ")")),
            parse_expr("\"string\")")
        );
        assert_eq!(
            Ok((LispVal::Atom("atom".to_string()), ")")),
            parse_expr("atom)")
        );
        assert_eq!(Ok((LispVal::Bool(true), ")")), parse_expr("#t)"));
    }
}
