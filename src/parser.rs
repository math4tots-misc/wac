//! Relatively language agnostic tools for parsing
//! for grammer stuff, see parsef.rs
use crate::lex;
use crate::LexError;
use crate::Span;
use crate::Token;
use std::rc::Rc;

pub struct Parser<'a> {
    i: usize,
    tokens_and_spans: Vec<(Token<'a>, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Result<Self, LexError> {
        let tokens_and_spans = lex(s)?;
        Ok(Self {
            i: 0,
            tokens_and_spans,
        })
    }
    pub fn peek(&self) -> Token<'a> {
        self.tokens_and_spans[self.i].0
    }
    pub fn span(&self) -> Span {
        self.tokens_and_spans[self.i].1
    }
    pub fn gettok(&mut self) -> Token<'a> {
        let token = self.peek();
        self.i += 1;
        token
    }
    pub fn at<'b, P: Into<Pattern<'b>>>(&self, p: P) -> bool {
        p.into().matches(self.peek())
    }
    pub fn at_name(&self, name: &str) -> bool {
        if let Token::Name(s) = self.peek() {
            s == name
        } else {
            false
        }
    }
    pub fn consume<'b, P: Into<Pattern<'b>>>(&mut self, p: P) -> bool {
        if self.at(p) {
            self.gettok();
            true
        } else {
            false
        }
    }
    pub fn expect<'b, P: Into<Pattern<'b>>>(&mut self, p: P) -> Result<Token<'a>, ParseError> {
        let p = p.into();
        if self.at(p) {
            Ok(self.gettok())
        } else {
            Err(ParseError::InvalidToken {
                span: self.span(),
                expected: format!("{:?}", p),
                got: format!("{:?}", self.peek()),
            })
        }
    }
    pub fn expect_name(&mut self) -> Result<Rc<str>, ParseError> {
        match self.peek() {
            Token::Name(name) => {
                self.gettok();
                Ok(name.into())
            }
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "Name".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
    pub fn expect_string(&mut self) -> Result<Rc<str>, ParseError> {
        match self.peek() {
            Token::RawString(s) => {
                self.gettok();
                Ok(s.into())
            }
            Token::NormalString(s) => {
                self.gettok();
                Ok(resolve_escapes(s)?)
            }
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "String".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
}

fn resolve_escapes(s: &str) -> Result<Rc<str>, ParseError> {
    enum State {
        Normal,
        Escape,
    }
    let mut ret = String::new();
    let mut state = State::Normal;
    for c in s.chars() {
        match state {
            State::Normal => {
                if c == '\\' {
                    state = State::Escape;
                } else {
                    ret.push(c);
                }
            }
            State::Escape => {
                match c {
                    'n' => ret.push('\n'),
                    'r' => ret.push('\r'),
                    't' => ret.push('\t'),
                    '0' => ret.push('\0'),
                    '"' => ret.push('"'),
                    '\'' => ret.push('\''),
                    '\\' => ret.push('\\'),
                    _ => return Err(ParseError::InvalidEscape(c)),
                }
                state = State::Normal;
            }
        }
    }
    Ok(ret.into())
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Pattern<'a> {
    Exact(Token<'a>),
    Name,
    Int,
    Float,
    String,
}

impl<'a> Pattern<'a> {
    fn matches<'b>(&self, t: Token<'b>) -> bool {
        match self {
            Pattern::Exact(p) => t == *p,
            Pattern::Name => match t {
                Token::Name(_) => true,
                _ => false,
            },
            Pattern::Int => match t {
                Token::Int(_) => true,
                _ => false,
            },
            Pattern::Float => match t {
                Token::Float(_) => true,
                _ => false,
            },
            Pattern::String => match t {
                Token::NormalString(_) | Token::RawString(_) => true,
                _ => false,
            },
        }
    }
}

impl<'a> From<Token<'a>> for Pattern<'a> {
    fn from(t: Token<'a>) -> Self {
        Self::Exact(t)
    }
}

#[derive(Debug)]
pub enum ParseError {
    InvalidToken {
        span: Span,
        expected: String,
        got: String,
    },
    InvalidEscape(char),
    NoSuchIntrinsic(Span, Rc<str>),
}
