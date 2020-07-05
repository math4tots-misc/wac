#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Name(&'a str),
    Int(i64),
    Float(f64),
    NormalString(&'a str),
    RawString(&'a str),
    Newline,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Comma,
    Dollar,
    Plus,
    Minus,
    Star,
    Slash,
    Slash2,
    Percent,
    Lt,
    Le,
    Gt,
    Ge,
    Ne,
    Eq2,
    Eq,
    EOF,
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub main: usize,
    pub start: usize,
    pub end: usize,
}
impl Span {
    pub fn join(self, other: Self) -> Self {
        Self {
            main: self.main,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }
    pub fn upto(self, other: Self) -> Self {
        Self {
            main: self.main,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.start),
        }
    }
}
impl From<[usize; 2]> for Span {
    fn from(a: [usize; 2]) -> Self {
        Self {
            main: a[0],
            start: a[0],
            end: a[1],
        }
    }
}
impl From<[usize; 3]> for Span {
    fn from(a: [usize; 3]) -> Self {
        Self {
            main: a[0],
            start: a[1],
            end: a[2],
        }
    }
}
#[derive(Debug)]
pub enum LexError {
    Unrecognized {
        /// the index into the string where we encountered the
        /// unrecognized token
        pos: usize,

        /// the start of the unrecognized token
        text: String,
    },
    BadRawStringQuote {
        /// the index where we expected to see the quote char
        pos: usize,

        /// the actual character that was seen instead of ' or "
        actual: char,
    },
    BadEndState {
        /// string describing the end state encountered
        state: String,
    },
    UnterminatedStringLiteral {
        /// index into the string marking the start of the
        /// unterminated strin gliteral
        start: usize,
    },
    MismatchedGroupingSymbols {
        pos: usize,
        open: char,
        close: char,
    },
    UnterminatedGroupingSymbol {
        open: char,
    },
}

pub fn lex(s: &str) -> Result<Vec<(Token, Span)>, LexError> {
    let mut ret = Vec::new();
    let mut chars = Chars::new(s);
    let mut state = State::Normal;
    let mut grouping_stack = Vec::new();
    while let Some((c, i)) = chars.next() {
        match state {
            State::Normal => match c {
                '\n' => match grouping_stack.last() {
                    Some('{') | None => {
                        ret.push((Token::Newline, [i, i + 1].into()));
                    }
                    _ => {}
                },
                _ if c.is_whitespace() => {}
                '"' | '\'' => {
                    state = State::NormalString(i, c);
                }
                'r' if [Some('#'), Some('\''), Some('"')].contains(&chars.peek()) => {
                    state = State::RawStringPrefix(i, 0);
                }
                _ if c.is_ascii_digit() => {
                    chars.put_back(c);
                    state = State::Digits(i);
                }
                _ if c == '_' || c.is_alphanumeric() => {
                    chars.put_back(c);
                    state = State::Name(i);
                }
                '#' => {
                    state = State::Comment;
                }
                '(' => {
                    grouping_stack.push(c);
                    ret.push((Token::LParen, [i, i + 1].into()));
                }
                ')' => {
                    match grouping_stack.pop() {
                        Some('(') => {}
                        other => {
                            return Err(LexError::MismatchedGroupingSymbols {
                                pos: i,
                                open: other.unwrap_or('x'),
                                close: c,
                            })
                        }
                    }
                    ret.push((Token::RParen, [i, i + 1].into()));
                }
                '[' => {
                    grouping_stack.push(c);
                    ret.push((Token::LBracket, [i, i + 1].into()));
                }
                ']' => {
                    match grouping_stack.pop() {
                        Some('[') => {}
                        other => {
                            return Err(LexError::MismatchedGroupingSymbols {
                                pos: i,
                                open: other.unwrap_or('x'),
                                close: c,
                            })
                        }
                    }
                    ret.push((Token::RBracket, [i, i + 1].into()));
                }
                '{' => {
                    grouping_stack.push(c);
                    ret.push((Token::LBrace, [i, i + 1].into()));
                }
                '}' => {
                    match grouping_stack.pop() {
                        Some('{') => {}
                        other => {
                            return Err(LexError::MismatchedGroupingSymbols {
                                pos: i,
                                open: other.unwrap_or('x'),
                                close: c,
                            })
                        }
                    }
                    ret.push((Token::RBrace, [i, i + 1].into()));
                }
                ';' => ret.push((Token::Semicolon, [i, i + 1].into())),
                ':' => ret.push((Token::Colon, [i, i + 1].into())),
                ',' => ret.push((Token::Comma, [i, i + 1].into())),
                '$' => ret.push((Token::Dollar, [i, i + 1].into())),
                '+' => ret.push((Token::Plus, [i, i + 1].into())),
                '-' => ret.push((Token::Minus, [i, i + 1].into())),
                '*' => ret.push((Token::Star, [i, i + 1].into())),
                '%' => ret.push((Token::Percent, [i, i + 1].into())),
                '=' | '!' | '<' | '>' | '/' => state = State::Combine(c),
                _ => {
                    return Err(LexError::Unrecognized {
                        pos: i,
                        text: format!("{}", c),
                    });
                }
            },
            State::Comment => {
                if c == '\n' {
                    state = State::Normal;
                }
            }
            State::Combine(first) => {
                match (first, c) {
                    ('=', '=') => ret.push((Token::Eq2, [i - 1, i + 1].into())),
                    ('/', '/') => ret.push((Token::Slash2, [i - 1, i + 1].into())),
                    ('!', '=') => ret.push((Token::Ne, [i - 1, i + 1].into())),
                    ('<', '=') => ret.push((Token::Le, [i - 1, i + 1].into())),
                    ('>', '=') => ret.push((Token::Ge, [i - 1, i + 1].into())),
                    _ => {
                        // the first character on its own can be a token,
                        // but doesn't combine with the second one
                        chars.put_back(c);
                        let tok = match first {
                            '=' => Token::Eq,
                            '<' => Token::Lt,
                            '>' => Token::Gt,
                            '/' => Token::Slash,
                            _ => {
                                return Err(LexError::Unrecognized {
                                    pos: i,
                                    text: format!("{}{}", first, c),
                                })
                            }
                        };
                        ret.push((tok, [i - 1, i].into()));
                    }
                }
                state = State::Normal;
            }
            State::Digits(start) => match c {
                '.' => {
                    state = State::DigitsAfterDot(start);
                }
                _ if c.is_ascii_digit() => {}
                _ => {
                    let value: i64 = s[start..i].parse().unwrap();
                    ret.push((Token::Int(value), [start, i].into()));

                    chars.put_back(c);
                    state = State::Normal;
                }
            },
            State::DigitsAfterDot(start) => {
                if !c.is_ascii_digit() {
                    let value: f64 = s[start..i].parse().unwrap();
                    ret.push((Token::Float(value), [start, i].into()));

                    chars.put_back(c);
                    state = State::Normal;
                }
            }
            State::Name(start) => {
                if !(c == '_' || c.is_alphanumeric()) {
                    let name = &s[start..i];
                    ret.push((Token::Name(name), [start, i].into()));

                    chars.put_back(c);
                    state = State::Normal;
                }
            }
            State::NormalString(start, quote) => match c {
                '\\' => {
                    state = State::NormalStringEscape(start, quote);
                }
                _ if c == quote => {
                    ret.push((Token::NormalString(&s[start + 1..i]), [start, i + 1].into()));
                    state = State::Normal;
                }
                _ => {}
            },
            State::NormalStringEscape(start, quote) => {
                state = State::NormalString(start, quote);
            }
            State::RawStringPrefix(start, ref mut hash_len) => match c {
                '#' => *hash_len += 1,
                '\'' | '"' => {
                    state = State::RawStringBody(start, *hash_len, c);
                }
                _ => {
                    return Err(LexError::BadRawStringQuote { pos: i, actual: c });
                }
            },
            State::RawStringBody(start, hash_len, quote) => {
                if c == quote {
                    state = State::RawStringSuffix(start, hash_len, quote, 0);
                }
            }
            State::RawStringSuffix(start, hash_len, quote, ref mut seen_so_far) => {
                if hash_len == *seen_so_far {
                    ret.push((
                        Token::RawString(&s[start + 2 + hash_len..i - 1 - *seen_so_far]),
                        [start, i].into(),
                    ));
                    chars.put_back(c);
                    state = State::Normal;
                } else if c == '#' {
                    *seen_so_far += 1;
                } else {
                    state = State::RawStringBody(start, hash_len, quote);
                }
            }
        }
    }
    match state {
        State::Normal => {
            ret.push((Token::EOF, [chars.pos, chars.pos].into()));
        }
        State::RawStringBody(start, _hash_len, _quote) => {
            return Err(LexError::UnterminatedStringLiteral { start });
        }
        _ => {
            return Err(LexError::BadEndState {
                state: format!("{:?}", state),
            });
        }
    }
    if let Some(ch) = grouping_stack.pop() {
        return Err(LexError::UnterminatedGroupingSymbol { open: ch });
    }
    Ok(ret)
}

#[derive(Debug)]
enum State {
    Normal,
    Comment,
    Combine(char),
    Digits(usize),
    DigitsAfterDot(usize),
    Name(usize),
    NormalString(usize, char),
    NormalStringEscape(usize, char),
    RawStringPrefix(usize, usize),
    RawStringBody(usize, usize, char),
    RawStringSuffix(usize, usize, char, usize),
}

struct Chars<'a> {
    chars_exhausted: bool,
    pos: usize,
    lookahead: Option<char>,
    chars: std::str::Chars<'a>,
}

impl<'a> Chars<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            chars_exhausted: false,
            pos: 0,
            lookahead: None,
            chars: s.chars(),
        }
    }
    fn peek(&mut self) -> Option<char> {
        match self.lookahead {
            Some(ch) => Some(ch),
            None => {
                self.lookahead = self.chars.next();
                self.lookahead
            }
        }
    }
    fn next(&mut self) -> Option<(char, usize)> {
        let opt_ch = match self.lookahead {
            Some(ch) => {
                self.lookahead = None;
                Some(ch)
            }
            None => {
                if self.chars_exhausted {
                    None
                } else {
                    match self.chars.next() {
                        Some(ch) => Some(ch),
                        None => {
                            self.chars_exhausted = true;
                            // This way, from the point of view of the main
                            // loop, the source always ends with a newline
                            // character
                            // (we use newline instead of just a space to
                            // ensure comments are properly terminated)
                            Some('\n')
                        }
                    }
                }
            }
        };
        let pos = self.pos;
        if let Some(ch) = opt_ch {
            self.pos += ch.len_utf8();
            Some((ch, pos))
        } else {
            None
        }
    }
    fn put_back(&mut self, ch: char) {
        if self.lookahead.is_some() {
            panic!("put_back more than 1 char not allowd");
        }
        self.lookahead = Some(ch);
        self.pos -= ch.len_utf8();
    }
}

#[cfg(test)]
mod tests {
    use super::LexError;
    use super::Token;

    fn lex(s: &str) -> Result<Vec<Token>, LexError> {
        Ok(super::lex(s)?.into_iter().map(|pair| pair.0).collect())
    }

    #[test]
    fn sample() {
        let toks = lex(r####"1 2.3, () hello "normal string" r"a" r#"raw string"#"####).unwrap();
        assert_eq!(
            toks,
            vec![
                Token::Int(1),
                Token::Float(2.3),
                Token::Comma,
                Token::LParen,
                Token::RParen,
                Token::Name("hello"),
                Token::NormalString("normal string"),
                Token::RawString("a"),
                Token::RawString("raw string"),
                Token::Newline,
                Token::EOF,
            ],
        );
    }
}
