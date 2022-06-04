use std::{fmt::format, str::Chars, vec};

fn main() {
    println!("Hello, world!");
}

#[derive(Debug, PartialEq)]
enum Token {
    Identifier(String),
    Bool(bool),
    Number(f64),
    Char(char),
    String(String),
    Popen,
    Pclose,
    Vopen,
    Quote,
    Quasi,
    Unquote,
    Splice,
    Dot,
}
struct TokenStream<'a> {
    src: Chars<'a>,
    top: Option<char>,
}

fn is_initial(c: &char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '!' | '$' | '&' | '*' => true,
        '/' | ':' | '<' | '=' => true,
        '>' | '?' | '^' | '_' | '~' => true,
        _ => false,
    }
}

fn is_explicit_sign(c: &char) -> bool {
    *c == '+' || *c == '-'
}
fn is_subsequent(c: &char) -> bool {
    if c.is_digit(10) || is_initial(c) || is_explicit_sign(c) {
        return true;
    }

    match c {
        '.' | '@' => true,
        _ => false,
    }
}
fn is_sign_subsequent(c: &char) -> bool {
    is_initial(c) || is_explicit_sign(c) || *c == '@'
}

fn is_delimiter(c: &char) -> bool {
    match *c {
        '(' | ')' | '"' | ';' | '|' => true,
        c => c.is_whitespace(),
    }
}

fn as_escaped(c: &char) -> Option<char> {
    let s = match c {
        'a' => '\x07',
        'b' => '\x08',
        't' => '\t',
        'n' => '\n',
        'r' => '\r',
        _ => return None,
    };
    Some(s)
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let top = self.top.or_else(|| self.src.next())?;

        // BEGIN identifiers
        if is_initial(&top) {
            let it = self.src.by_ref();
            let mut chars = vec![top.to_string()];
            for c in it.take_while(is_subsequent).into_iter() {
                chars.push(c.to_string())
            }
            let id: String = chars.join("");
            return Some(Token::Identifier(id));
        }

        // pipe delimited
        if top == '|' {
            let mut chars = String::new();
            while let Some(c) = self.src.next() {
                if c == '|' {
                    break;
                }
                if c == '\\' {
                    match self.src.next()? {
                        '|' => chars.push('|'),
                        'x' => {
                            println!("found hexhchar '{}'", chars);
                            let mut hexchars = String::new();
                            while let Some(c) = self.src.next() {
                                match c {
                                    'A'..='F' | 'a'..='f' | '0'..='9' => hexchars.push(c),
                                    ';' => {
                                        let u = u32::from_str_radix(&hexchars, 16).unwrap();
                                        chars.push(char::from_u32(u)?);
                                        break;
                                    }
                                    x => panic!("unexpected hex char '{}'", x),
                                }
                            }
                        }
                        x => chars.push(as_escaped(&x)?),
                    }
                    // escape sequence
                } else {
                    chars.push(c);
                }
            }
            return Some(Token::Identifier(chars));
        }
        // peculiar
        if is_explicit_sign(&top) {
            let mut chars = vec![top.to_string()];
            if let Some(c) = self.src.next() {
                if is_sign_subsequent(&c) {
                    let it = self.src.by_ref().take_while(is_subsequent);
                    chars.push(c.to_string());
                    chars.extend(it.map(|c| c.to_string()));
                } else if c == '.' {
                    chars.push(c.to_string());
                    if let Some(c) = self.src.next() {
                        if is_dot_subsequent(&c) {
                            chars.push(c.to_string());
                        }
                        let it = self.src.by_ref().take_while(is_subsequent);
                        chars.extend(it.map(|c| c.to_string()));
                    }
                    let id: String = chars.join("");
                    return Some(Token::Identifier(id));
                } else {
                    self.top = Some(c);
                }
                let id: String = chars.join("");
                return Some(Token::Identifier(id));
            }
            return Some(Token::Identifier(chars.join("")));
        }
        if top == '.' {
            let mut chars = vec![top.to_string()];
            if let Some(c) = self.src.next() {
                if is_dot_subsequent(&c) {
                    chars.push(c.to_string());
                }
                let it = self.src.by_ref().take_while(is_subsequent);
                chars.extend(it.map(|c| c.to_string()));
            }
            let id: String = chars.join("");
            return Some(Token::Identifier(id));
        }
        // END identifiers

        // BEGIN hash
        if top == '#' {
            let top = self.src.next()?;
            let it = self.src.by_ref().take_while(|c| !is_delimiter(c));
            // BEGIN booleans
            if top == 't' {
                let rest: String = it.take_while(|c| !c.is_whitespace()).collect();
                if !rest.is_empty() && rest != "rue" {
                    panic!("unknown token '#t{}'", rest);
                }
                return Some(Token::Bool(true));
            }

            if top == 'f' {
                let rest: String = it.take_while(|c| !c.is_whitespace()).collect();
                if !rest.is_empty() && rest != "alse" {
                    panic!("unknown token '#f{}'", rest);
                }
                return Some(Token::Bool(false));
            }
            // END booleans
        }

        // END hash

        None
    }
}

fn is_dot_subsequent(c: &char) -> bool {
    is_sign_subsequent(c) || *c == '.'
}

fn tokenize(s: &str) -> TokenStream {
    TokenStream {
        src: s.chars(),
        top: None,
    }
}

#[cfg(test)]
mod tests {
    use super::{tokenize, Token};

    #[test]
    fn tokenize_identifiers() {
        let ts = vec![
            ("a34kTMNs", "a34kTMNs"),
            ("lambda", "lambda"),
            ("list->vector", "list->vector"),
            ("q", "q"),
            ("V17a", "V17a"),
            ("|two words|", "two words"),
            ("|two\\x20;words|", "two words"),
            ("..", ".."),
            ("+..", "+.."),
            ("+", "+"),
            ("+soup+", "+soup+"),
            ("<=?", "<=?"),
            ("->string", "->string"),
            (
                "the-word-recursion-has-many-meanings",
                "the-word-recursion-has-many-meanings",
            ),
        ];

        for (src, dst) in ts {
            let expect = Token::Identifier(dst.to_string());
            let mut stream = tokenize(src);
            assert_eq!(Some(expect), stream.next())
        }
    }

    #[test]
    fn tokenize_booleans() {
        let ts = vec![
            ("#t", true),
            ("#f", false),
            ("#true", true),
            ("#false", false),
        ];
        for (inp, expect) in ts {
            let expect = Token::Bool(expect);
            let mut stream = tokenize(inp);
            assert_eq!(Some(expect), stream.next())
        }
    }

    #[test]
    fn tokenize_numbers() {
        let ts = vec![
            ("#b101", 5.0),
            ("#o33", 27.0),
            ("#d123456789", 123456789.0),
            ("#xdeadbeef", 3735928559.0),
            ("123.456", 123.456),
            // TODO: #e #i exactness prefix
        ];
        for (inp, expect) in ts {
            let expect = Token::Number(expect);
            let mut stream = tokenize(inp);
            assert_eq!(Some(expect), stream.next())
        }
    }
}
