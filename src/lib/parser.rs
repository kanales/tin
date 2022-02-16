use crate::lib::types::{Atom, Exp, Number, TinError, TinResult};

fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = String> + 'a {
    input
        .replace("(", " ( ")
        .replace(")", " ) ")
        .split_ascii_whitespace()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .into_iter()
}

#[test]
fn tokenize_test() {
    let input = "(begin (define r 10) (* pi (* r r)))";
    let expect = vec![
        "(", "begin", "(", "define", "r", "10", ")", "(", "*", "pi", "(", "*", "r", "r", ")", ")",
        ")",
    ];

    let res: Vec<_> = tokenize(input).collect();

    assert_eq!(res, expect);
}

fn from_tokens<I>(tokens: &mut std::iter::Peekable<I>) -> TinResult<Exp>
where
    I: Iterator<Item = String>,
{
    if let None = tokens.peek() {
        return Err(TinError::SyntaxError("Unexpected EOF".to_string()));
    }
    let token = tokens.next().unwrap();
    if token == "(" {
        let mut v = Vec::new();
        loop {
            if let Some(c) = tokens.peek() {
                if c == ")" {
                    break;
                }
            }
            v.push(from_tokens(tokens)?);
        }
        tokens.next();
        return Ok(Exp::List(v));
    }
    if token == ")" {
        return Err(TinError::SyntaxError("Unexpected ')'".to_string()));
    }

    Ok(Exp::Atom(atom(&token)))
}

fn atom(token: &str) -> Atom {
    if let Ok(r) = token.parse::<i64>() {
        return Atom::Number(Number::Int(r));
    }

    if let Ok(r) = token.parse::<f64>() {
        return Atom::Number(Number::Float(r));
    }
    match token {
        "#t" => Atom::Bool(true),
        "#f" => Atom::Bool(false),
        _ => Atom::Symbol(token.to_string()),
    }
}

pub fn parse(program: &str) -> TinResult<Exp> {
    from_tokens(&mut tokenize(program).peekable())
}

#[test]
fn parse_test() {
    let prog = "(begin (define r 10) (* pi (* r r)))";
    let expect = Exp::List(vec![
        Exp::Atom(Atom::Symbol("begin".to_string())),
        Exp::List(vec![
            Exp::Atom(Atom::Symbol("define".to_string())),
            Exp::Atom(Atom::Symbol("r".to_string())),
            Exp::Atom(Atom::Number(Number::Int(10))),
        ]),
        Exp::List(vec![
            Exp::Atom(Atom::Symbol("*".to_string())),
            Exp::Atom(Atom::Symbol("pi".to_string())),
            Exp::List(vec![
                Exp::Atom(Atom::Symbol("*".to_string())),
                Exp::Atom(Atom::Symbol("r".to_string())),
                Exp::Atom(Atom::Symbol("r".to_string())),
            ]),
        ]),
    ]);

    assert_eq!(parse(prog), Ok(expect));
}
