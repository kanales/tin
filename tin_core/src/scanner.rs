use std::str::Chars;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
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

impl Token {
    pub fn closes(&self, c: char) -> bool {
        if let (true, Token::Pclose) = (c == '(', self) {
            true
        } else {
            false
        }
    }
}

pub struct TokenStream<'a> {
    src: Chars<'a>,
    top: Option<char>,
}

pub fn is_initial(c: &char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '!' | '$' | '&' | '*' => true,
        '/' | ':' | '<' | '=' => true,
        '>' | '?' | '^' | '_' | '~' => true,
        _ => false,
    }
}

pub fn is_explicit_sign(c: &char) -> bool {
    *c == '+' || *c == '-'
}
pub fn is_subsequent(c: &char) -> bool {
    if c.is_digit(10) || is_initial(c) || is_explicit_sign(c) {
        return true;
    }

    match c {
        '.' | '@' => true,
        _ => false,
    }
}
pub fn is_sign_subsequent(c: &char) -> bool {
    is_initial(c) || is_explicit_sign(c) || *c == '@'
}

pub fn is_delimiter(c: &char) -> bool {
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
    fn collect_while<F>(&mut self, p: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut acc = String::new();
        for c in self.src.by_ref() {
            if p(c) {
                acc.push(c);
            } else {
                self.top = Some(c);
                break;
            }
        }
        return acc;
    }

    fn try_identifier(&mut self, top: &char) -> Option<<Self as Iterator>::Item> {
        // BEGIN identifiers
        if is_initial(top) {
            let mut chars = self.collect_while(|x| is_subsequent(&x));
            chars.insert(0, *top);
            return Some(Token::Identifier(chars));
        }

        // pipe delimited
        if *top == '|' {
            return self.try_pipe();
        }
        // peculiar
        if is_explicit_sign(top) {
            let mut chars = top.to_string();
            if let Some(c) = self.src.next() {
                if is_sign_subsequent(&c) {
                    chars.push(c);
                    let sub = self.collect_while(|c| is_subsequent(&c));
                    chars.push_str(&sub);
                } else if c == '.' {
                    chars.push(c);
                    if let Some(c) = self.src.next() {
                        if is_delimiter(&c) {
                            self.top = Some(c);
                        } else {
                            if is_dot_subsequent(&c) {
                                chars.push(c);
                            }
                            let sub = self.collect_while(|c| is_subsequent(&c));
                            chars.push_str(&sub);
                        }
                    }
                    return Some(Token::Identifier(chars));
                } else {
                    self.top = Some(c);
                }
                return Some(Token::Identifier(chars));
            }
            return Some(Token::Identifier(chars));
        }

        if *top == '.' {
            let mut chars = String::from(".");
            if let Some(c) = self.src.next() {
                if is_delimiter(&c) {
                    self.top = Some(c);
                    return Some(Token::Dot);
                }
                if is_dot_subsequent(&c) {
                    chars.push(c);
                }
                let sub = self.collect_while(|c| is_subsequent(&c));
                chars.push_str(&sub);
            }
            return Some(Token::Identifier(chars));
        }
        // END identifiers
        return None;
    }

    fn try_pipe(&mut self) -> Option<<Self as Iterator>::Item> {
        let mut chars = String::new();
        while let Some(c) = self.src.next() {
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

    fn not_delimiter(&mut self) -> String {
        let mut acc = String::new();
        for c in self.src.by_ref() {
            if is_delimiter(&c) {
                self.top = Some(c);
                return acc;
            }
            acc.push(c);
        }
        acc
    }

    fn try_number_radix(&mut self, radix: u32) -> Option<<Self as Iterator>::Item> {
        let num: String = self.not_delimiter();
        if num.is_empty() {
            return None;
        }

        if let Ok(n) = u32::from_str_radix(&num, radix) {
            return Some(Token::Number(n as f64));
        }
        token_error!("\"{}\" is not a valid base '{}' number", num, radix);
    }

    fn try_number_real(&mut self, sign: f64) -> Option<<Self as Iterator>::Item> {
        let num: String = self.not_delimiter();
        if num.is_empty() {
            return None;
        }

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
                let it = self.src.by_ref();
                let rest: String = it.take_while(|c| !c.is_whitespace()).collect();
                if !rest.is_empty() && rest != "rue" {
                    token_error!("unknown token '#t{}'", rest)
                }
                return Some(Token::Bool(true));
            }

            if top == 'f' {
                let it = self.src.by_ref();
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
                    let mut args = String::new();
                    while let Some(c) = self.src.next() {
                        if is_delimiter(&c) {
                            self.top = Some(c);
                            break;
                        }
                        args.push(c);
                    }

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
            '0'..='9' => match self.try_number_real(1.0) {
                Some(Token::Number(n)) => {
                    let new = top.to_digit(10).unwrap();
                    let n = n + (new as f64) * 10.0f64.powf(n.log10().floor() + 1.0);

                    Some(Token::Number(n))
                }
                None => Some(Token::Number(top.to_digit(10).unwrap().into())),
                x => unreachable!("hint: `TokenStream::try_number_real` returned `{:?}`.", x),
            },
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
            '`' => Some(Token::Quasi),
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
        let mut it = self.src.by_ref().skip_while(|c| c.is_whitespace());
        let top = self
            .top
            .take()
            .into_iter()
            .skip_while(|c| c.is_whitespace())
            .next()
            .or_else(|| it.next())?;

        self.try_identifier(&top)
            .or_else(|| self.try_hash(&top))
            .or_else(|| self.try_signed(&top))
            .or_else(|| self.try_string(&top))
            .or_else(|| self.try_delimiters(&top))
    }
}

pub fn is_dot_subsequent(c: &char) -> bool {
    is_sign_subsequent(c) || *c == '.'
}

pub fn tokenize(s: &str) -> TokenStream {
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
            ("42", 42.0),
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
            assert_eq!(stream.next(), Some(expect));
        }
    }

    #[test]
    fn tokenize_multiple() {
        let input = "1 2(a b)";
        let expect = vec![
            Token::Number(1.0),
            Token::Number(2.0),
            Token::Popen,
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Pclose,
        ];

        let tokens: Vec<_> = tokenize(input).collect();
        assert_eq!(tokens, expect);
    }
}
