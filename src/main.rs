use std::{convert::TryInto, fmt::format, str::Chars, vec};

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
    Vector,
    ByteVec,
    Quote,
    Quasi,
    Unquote,
    Splice,
    Dot,
    Error(String),
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

macro_rules! token_error {
    ($s:literal) => {
        return Some(Token::Error($s.to_string()))
    };
    ($s:literal, $($a:expr),+) => {
        return Some(Token::Error(format!($s, $($a),+ )))
    };
}

impl TokenStream<'_> {
    fn try_identifier(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        // BEGIN identifiers
        if is_initial(top) {
            let it = self.src.by_ref();
            let mut chars = vec![top.to_string()];
            for c in it.take_while(is_subsequent).into_iter() {
                chars.push(c.to_string())
            }
            let id: String = chars.join("");
            return Some(Token::Identifier(id));
        }

        // pipe delimited
        if *top == '|' {
            return self.try_pipe();
        }
        // peculiar
        if is_explicit_sign(top) {
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
        if *top == '.' {
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
        return None;
    }

    fn try_pipe(&mut self) -> Option<<Self as Iterator>::Item> {
        let mut chars = String::new();
        while let Some(c) = self.src.next() {
            println!("c {}", c);
            if c == '|' {
                break;
            }
            if c == '\\' {
                match self.src.next()? {
                    '|' => chars.push('|'),
                    'x' => {
                        let mut hexchars = String::new();
                        while let Some(c) = self.src.next() {
                            match c {
                                'A'..='F' | 'a'..='f' | '0'..='9' => hexchars.push(c),
                                ';' => {
                                    let u = u32::from_str_radix(&hexchars, 16).unwrap();
                                    chars.push(char::from_u32(u)?);
                                    break;
                                }
                                x => token_error!("unexpected hex char '{}'", x),
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

    fn try_number_radix(&mut self, radix: u32) -> Option<<Self as Iterator>::Item> {
        let num: String = self.src.by_ref().take_while(|c| !is_delimiter(c)).collect();
        if let Ok(n) = u32::from_str_radix(&num, radix) {
            return Some(Token::Number(n as f64));
        }
        token_error!("\"{}\" is not a valid base '{}' number", num, radix);
    }

    fn try_number_real(&mut self, sign: f64) -> Option<<Self as Iterator>::Item> {
        let num: String = self.src.by_ref().take_while(|c| !is_delimiter(c)).collect();
        if let Ok(n) = num.parse::<f64>() {
            return Some(Token::Number(sign * n));
        }
        token_error!("\"{}\" is not a valid number", num);
    }

    fn try_hash(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        // BEGIN hash
        if *top == '#' {
            let top = self.src.next()?;
            // BEGIN booleans
            if top == 't' {
                let it = self.src.by_ref().take_while(|c| !is_delimiter(c));
                let rest: String = it.take_while(|c| !c.is_whitespace()).collect();
                if !rest.is_empty() && rest != "rue" {
                    token_error!("unknown token '#t{}'", rest)
                }
                return Some(Token::Bool(true));
            }

            if top == 'f' {
                let it = self.src.by_ref().take_while(|c| !is_delimiter(c));
                let rest: String = it.take_while(|c| !c.is_whitespace()).collect();
                if !rest.is_empty() && rest != "alse" {
                    token_error!("unknown token '#f{}'", rest)
                }
                return Some(Token::Bool(false));
            }
            // END booleans

            // BEGIN numbers
            match top {
                'b' => return self.try_number_radix(2),
                'x' => return self.try_number_radix(16),
                'o' => return self.try_number_radix(8),
                'd' => return self.try_number_real(1.0),
                _ => {}
            }
            // END numbers

            match top {
                '(' => return Some(Token::Vector),
                'u' => {
                    let args = self
                        .src
                        .by_ref()
                        .take_while(|x| !is_delimiter(x))
                        .collect::<String>();
                    let delim = self.src.next();
                    if &args == "8" && delim == Some('(') {
                        return Some(Token::ByteVec);
                    }
                    token_error!("unknown reader expression '#u{}'", args);
                }
                _ => {}
            }

            if let Some(t) = self.try_char(&top) {
                return Some(t);
            }

            self.top = Some(top);
        }
        return None;
    }

    fn try_signed(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        match top {
            '+' => self.try_number_real(1.0),
            '-' => self.try_number_real(-1.0),
            '0'..='9' => {
                if let Some(Token::Number(n)) = self.try_number_real(1.0) {
                    let new = top.to_digit(10).unwrap();
                    let n = n + (new as f64) * 10.0f64.powf(n.log10().floor() + 1.0);

                    return Some(Token::Number(n));
                } else {
                    unreachable!()
                }
            }
            _ => None,
        }
    }

    fn try_string(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        if *top != '"' {
            return None;
        }
        let mut s = String::new();

        while let Some(c) = self.src.next() {
            match c {
                '"' => return Some(Token::String(s)),
                '\\' => {
                    let n = self.src.next()?;
                    match n {
                        '"' => s.push('"'),
                        _ => unimplemented!("String escapes are not implemented"),
                    }
                }
                c => s.push(c),
            }
        }
        None
    }

    fn try_delimiters(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        match top {
            '(' => Some(Token::Popen),
            ')' => Some(Token::Pclose),
            '\'' => Some(Token::Quote),
            ',' => {
                let c = self.src.next();
                if let Some('@') = c {
                    Some(Token::Splice)
                } else {
                    self.top = c;
                    Some(Token::Unquote)
                }
            }
            _ => None,
        }
    }

    fn try_char(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        if *top != '\\' {
            return None;
        }

        let rest: String = self.src.by_ref().take_while(|x| !is_delimiter(x)).collect();
        match rest.as_str() {
            "alarm" => return Some(Token::Char('\x07')),
            "backspace" => return Some(Token::Char('\x08')),
            "delete" => return Some(Token::Char('\x7f')),
            "escape" => return Some(Token::Char('\x1b')),
            "newline" => return Some(Token::Char('\n')),
            "null" => return Some(Token::Char('\0')),
            "return" => return Some(Token::Char('\r')),
            "space" => return Some(Token::Char(' ')),
            "tab" => return Some(Token::Char('\t')),
            s => {
                if s.len() == 1 {
                    return Some(Token::Char(s.chars().next().unwrap()));
                } else if s.len() == 0 {
                    token_error!("expected char found nothing");
                } else {
                    let mut cs = s.chars();
                    if cs.next().unwrap() == 'x' {
                        // hexcode
                        let rest: String = cs.collect();
                        if let Ok(u) = u32::from_str_radix(&rest, 16) {
                            if let Some(c) = char::from_u32(u) {
                                return Some(Token::Char(c));
                            }
                        }
                        token_error!("unrecognized hexcode '{}'", rest);
                    } else {
                        token_error!("unexpected char '{}'", s);
                    }
                }
            }
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let top = self.top.or_else(|| self.src.next())?;

        self.try_identifier(&top)
            .or_else(|| self.try_hash(&top))
            .or_else(|| self.try_signed(&top))
            .or_else(|| self.try_string(&top))
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
    fn tokenize_pipe_id() {
        let ts = vec![
            ("|two words|", "two words"),
            ("|two\\x20;words|", "two words"),
        ];

        for (src, dst) in ts {
            let expect = Token::Identifier(dst.to_string());
            let mut stream = tokenize(src);
            println!("{}", src);
            assert_eq!(Some(expect), stream.next())
        }
    }

    #[test]
    fn tokenize_identifiers() {
        let ts = vec![
            ("a34kTMNs", "a34kTMNs"),
            ("lambda", "lambda"),
            ("list->vector", "list->vector"),
            ("q", "q"),
            ("V17a", "V17a"),
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
    fn tokenize_strings() {
        let ts = vec![
            ("\"the quick brown fox\"", "the quick brown fox"),
            ("\" \\\"quoted\"", " \"quoted"),
        ];
        for (inp, expect) in ts {
            let expect = Token::String(expect.to_string());
            let mut stream = tokenize(inp);
            assert_eq!(Some(expect), stream.next())
        }
    }

    #[test]
    fn tokenize_chars() {
        let ts = vec![
            ("#\\a", 'a'),
            ("#\\newline", '\n'),
            ("#\\xCEBB", '\u{CEBB}'),
        ];
        for (inp, expect) in ts {
            let expect = Token::Char(expect);
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
