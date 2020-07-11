use std::collections::VecDeque;

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
    Exclamation,
    Caret,
    Ampersand,
    VerticalBar,
    Arrow,
    Dot,
    Dot2,
    Lt,
    Le,
    Gt,
    Ge,
    Ne,
    Eq2,
    Eq,
    Lt2,
    Gt2,
    EOF,
}
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub main: usize,
    pub start: usize,
    pub end: usize,
    pub lineno: usize,
}
impl Span {
    pub fn new(main: usize, start: usize, end: usize, lineno: usize) -> Self {
        Self {
            main,
            start,
            end,
            lineno,
        }
    }
    pub fn join(self, other: Self) -> Self {
        Self {
            main: self.main,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
            lineno: self.lineno,
        }
    }
    pub fn upto(self, other: Self) -> Self {
        Self {
            main: self.main,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.start),
            lineno: self.lineno,
        }
    }
}
#[derive(Debug)]
pub enum LexError {
    Unrecognized {
        /// the location in the string where we encountered the
        /// unrecognized token
        span: Span,

        /// the start of the unrecognized token
        text: String,
    },
    BadRawStringQuote {
        /// the location where we expected to see the quote char
        span: Span,

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
        span: Span,
    },
    MismatchedGroupingSymbols {
        span: Span,
        open: char,
        close: char,
    },
    UnterminatedGroupingSymbol {
        open: char,
    },
}

impl LexError {
    pub fn span(&self) -> Span {
        match self {
            LexError::Unrecognized {
                /// the index into the string where we encountered the
                /// unrecognized token
                span,

                /// the start of the unrecognized token
                    text: _,
            } => *span,
            LexError::BadRawStringQuote {
                /// the location where we expected to see the quote char
                span,

                /// the actual character that was seen instead of ' or "
                    actual: _,
            } => *span,
            LexError::BadEndState {
                /// string describing the end state encountered
                    state: _,
            } => dummy_span(),
            LexError::UnterminatedStringLiteral {
                /// index into the string marking the start of the
                /// unterminated strin gliteral
                span,
            } => *span,
            LexError::MismatchedGroupingSymbols {
                span,
                open: _,
                close: _,
            } => *span,
            LexError::UnterminatedGroupingSymbol { open: _ } => dummy_span(),
        }
    }
}

fn dummy_span() -> Span {
    Span {
        main: 0,
        start: 0,
        end: 0,
        lineno: 0,
    }
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
                        ret.push((Token::Newline, chars.span(i, i + 1)));
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
                    ret.push((Token::LParen, chars.span(i, i + 1)));
                }
                ')' => {
                    match grouping_stack.pop() {
                        Some('(') => {}
                        other => {
                            return Err(LexError::MismatchedGroupingSymbols {
                                span: chars.span(i, i + 1),
                                open: other.unwrap_or('x'),
                                close: c,
                            })
                        }
                    }
                    ret.push((Token::RParen, chars.span(i, i + 1)));
                }
                '[' => {
                    grouping_stack.push(c);
                    ret.push((Token::LBracket, chars.span(i, i + 1)));
                }
                ']' => {
                    match grouping_stack.pop() {
                        Some('[') => {}
                        other => {
                            return Err(LexError::MismatchedGroupingSymbols {
                                span: chars.span(i, i + 1),
                                open: other.unwrap_or('x'),
                                close: c,
                            })
                        }
                    }
                    ret.push((Token::RBracket, chars.span(i, i + 1)));
                }
                '{' => {
                    grouping_stack.push(c);
                    ret.push((Token::LBrace, chars.span(i, i + 1)));
                }
                '}' => {
                    match grouping_stack.pop() {
                        Some('{') => {}
                        other => {
                            return Err(LexError::MismatchedGroupingSymbols {
                                span: chars.span(i, i + 1),
                                open: other.unwrap_or('x'),
                                close: c,
                            })
                        }
                    }
                    ret.push((Token::RBrace, chars.span(i, i + 1)));
                }
                ';' => ret.push((Token::Semicolon, chars.span(i, i + 1))),
                ':' => ret.push((Token::Colon, chars.span(i, i + 1))),
                ',' => ret.push((Token::Comma, chars.span(i, i + 1))),
                '$' => ret.push((Token::Dollar, chars.span(i, i + 1))),
                '+' => ret.push((Token::Plus, chars.span(i, i + 1))),
                '-' => ret.push((Token::Minus, chars.span(i, i + 1))),
                '*' => ret.push((Token::Star, chars.span(i, i + 1))),
                '%' => ret.push((Token::Percent, chars.span(i, i + 1))),
                '^' => ret.push((Token::Caret, chars.span(i, i + 1))),
                '&' => ret.push((Token::Ampersand, chars.span(i, i + 1))),
                '|' => ret.push((Token::VerticalBar, chars.span(i, i + 1))),
                '=' | '!' | '<' | '>' | '/' | '.' => state = State::Combine(c),
                _ => {
                    return Err(LexError::Unrecognized {
                        span: chars.span(i, i + 1),
                        text: format!("{}", c),
                    });
                }
            },
            State::Comment => {
                if c == '\n' {
                    chars.put_back(c);
                    state = State::Normal;
                }
            }
            State::Combine(first) => {
                match (first, c) {
                    ('=', '=') => ret.push((Token::Eq2, chars.span(i - 1, i + 1))),
                    ('/', '/') => ret.push((Token::Slash2, chars.span(i - 1, i + 1))),
                    ('!', '=') => ret.push((Token::Ne, chars.span(i - 1, i + 1))),
                    ('<', '=') => ret.push((Token::Le, chars.span(i - 1, i + 1))),
                    ('>', '=') => ret.push((Token::Ge, chars.span(i - 1, i + 1))),
                    ('<', '<') => ret.push((Token::Lt2, chars.span(i - 1, i + 1))),
                    ('>', '>') => ret.push((Token::Gt2, chars.span(i - 1, i + 1))),
                    ('.', '.') => ret.push((Token::Dot2, chars.span(i - 1, i + 1))),
                    ('=', '>') => ret.push((Token::Arrow, chars.span(i - 1, i + 1))),
                    _ => {
                        // the first character on its own can be a token,
                        // but doesn't combine with the second one
                        chars.put_back(c);
                        let tok = match first {
                            '=' => Token::Eq,
                            '<' => Token::Lt,
                            '>' => Token::Gt,
                            '/' => Token::Slash,
                            '!' => Token::Exclamation,
                            '.' => Token::Dot,
                            _ => {
                                return Err(LexError::Unrecognized {
                                    span: chars.span(i, i + 2),
                                    text: format!("{}{}", first, c),
                                })
                            }
                        };
                        ret.push((tok, chars.span(i - 1, i)));
                    }
                }
                state = State::Normal;
            }
            State::Digits(start) => match c {
                '.' if chars.peek() != Some('.') => {
                    state = State::DigitsAfterDot(start);
                }
                _ if c.is_ascii_digit() => {}
                'x' => {
                    state = State::HexDigits(start);
                }
                'b' => {
                    state = State::BinaryDigits(start);
                }
                _ => {
                    let value: i64 = s[start..i].parse().unwrap();
                    ret.push((Token::Int(value), chars.span(start, i)));

                    chars.put_back(c);
                    state = State::Normal;
                }
            },
            State::DigitsAfterDot(start) => {
                if !c.is_ascii_digit() {
                    let value: f64 = s[start..i].parse().unwrap();
                    ret.push((Token::Float(value), chars.span(start, i)));

                    chars.put_back(c);
                    state = State::Normal;
                }
            }
            State::BinaryDigits(start) => match c {
                _ if c == '0' || c == '1' => {}
                _ => {
                    let value = i64::from_str_radix(&s[start + 2..i], 2).unwrap();
                    ret.push((Token::Int(value), chars.span(start, i)));

                    chars.put_back(c);
                    state = State::Normal;
                }
            },
            State::HexDigits(start) => match c {
                _ if c.is_ascii_hexdigit() => {}
                _ => {
                    let value = i64::from_str_radix(&s[start + 2..i], 16).unwrap();
                    ret.push((Token::Int(value), chars.span(start, i)));

                    chars.put_back(c);
                    state = State::Normal;
                }
            },
            State::Name(start) => {
                if !(c == '_' || c.is_alphanumeric()) {
                    let name = &s[start..i];
                    ret.push((Token::Name(name), chars.span(start, i)));

                    chars.put_back(c);
                    state = State::Normal;
                }
            }
            State::NormalString(start, quote) => match c {
                '\\' => {
                    state = State::NormalStringEscape(start, quote);
                }
                _ if c == quote => {
                    ret.push((
                        Token::NormalString(&s[start + 1..i]),
                        chars.span(start, i + 1),
                    ));
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
                    return Err(LexError::BadRawStringQuote {
                        span: chars.span(i, i + 1),
                        actual: c,
                    });
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
                        chars.span(start, i),
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
            ret.push((Token::EOF, chars.span(chars.pos, chars.pos)));
        }
        State::RawStringBody(start, _hash_len, _quote) => {
            return Err(LexError::UnterminatedStringLiteral {
                span: chars.span(start, start + 1),
            });
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
    BinaryDigits(usize),
    HexDigits(usize),
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
    lookahead: VecDeque<char>,
    chars: std::str::Chars<'a>,
    lineno: usize,
}

impl<'a> Chars<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            chars_exhausted: false,
            pos: 0,
            lookahead: VecDeque::new(),
            chars: s.chars(),
            lineno: 1,
        }
    }
    fn peek(&mut self) -> Option<char> {
        match self.lookahead.front() {
            Some(ch) => Some(*ch),
            None => {
                if self.chars_exhausted {
                    None
                } else {
                    match self.chars.next() {
                        Some(c) => {
                            self.lookahead.push_back(c);
                            Some(c)
                        }
                        None => {
                            self.chars_exhausted = true;
                            // This way, from the point of view of the main
                            // loop, the source always ends with a newline
                            // character
                            // (we use newline instead of just a space to
                            // ensure comments are properly terminated)
                            self.lookahead.push_back('\n');
                            None
                        }
                    }
                }
            }
        }
    }
    fn next(&mut self) -> Option<(char, usize)> {
        self.peek(); // 'prime' lookahead
        if let Some(ch) = self.lookahead.pop_front() {
            let pos = self.pos;
            self.pos += ch.len_utf8();
            if ch == '\n' {
                self.lineno += 1;
            }
            Some((ch, pos))
        } else {
            None
        }
    }
    fn put_back(&mut self, ch: char) {
        self.lookahead.push_front(ch);
        self.pos -= ch.len_utf8();
        if ch == '\n' {
            self.lineno -= 1;
        }
    }
    fn span(&self, main: usize, end: usize) -> Span {
        Span {
            main,
            start: main,
            end,
            lineno: self.lineno,
        }
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
