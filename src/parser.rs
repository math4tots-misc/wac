//! Relatively language agnostic tools for parsing
//! for grammer stuff, see parsef.rs
use crate::lex;
use crate::LexError;
use crate::SSpan;
use crate::Source;
use crate::Span;
use crate::Token;
use crate::Type;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Parser<'a> {
    source: Rc<Source>,
    i: usize,
    tokens_and_spans: Vec<(Token<'a>, Span)>,
    user_type_map: &'a Option<HashMap<Rc<str>, Type>>,
}

impl<'a> Parser<'a> {
    pub fn new(
        source: &'a Rc<Source>,
        user_type_map: &'a Option<HashMap<Rc<str>, Type>>,
    ) -> Result<Self, LexError> {
        let tokens_and_spans = lex(&source.data)?;
        Ok(Self {
            source: source.clone(),
            i: 0,
            tokens_and_spans,
            user_type_map,
        })
    }
    pub fn peek(&self) -> Token<'a> {
        self.tokens_and_spans[self.i].0
    }
    pub fn span(&self) -> SSpan {
        let span = self.tokens_and_spans[self.i].1;
        SSpan {
            source: self.source.clone(),
            span: span,
        }
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
                let span = self.span();
                self.gettok();
                Ok(resolve_escapes(s, span)?)
            }
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "String".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
    pub fn get_user_defined_type(
        &mut self,
        span: &SSpan,
        name: &Rc<str>,
    ) -> Result<Type, ParseError> {
        match self.user_type_map {
            Some(map) => match map.get(name) {
                Some(r) => Ok(*r),
                None => Err(ParseError::InvalidToken {
                    span: span.clone(),
                    expected: "enum or record".into(),
                    got: "user defined type not found".into(),
                }),
            },
            None => {
                // If no map is provided, return a dummy type
                Ok(Type::Enum(0))
            }
        }
    }
}

fn resolve_escapes(s: &str, span: SSpan) -> Result<Rc<str>, ParseError> {
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
                    _ => return Err(ParseError::InvalidEscape(span, c)),
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
        span: SSpan,
        expected: String,
        got: String,
    },
    InvalidEscape(SSpan, char),
    NoSuchIntrinsic(SSpan, Rc<str>),
}

impl ParseError {
    pub fn span(&self) -> SSpan {
        match self {
            Self::InvalidToken { span, .. } => span.clone(),
            Self::InvalidEscape(span, ..) => span.clone(),
            Self::NoSuchIntrinsic(span, ..) => span.clone(),
        }
    }

    pub fn format(&self) -> String {
        match self {
            Self::InvalidToken {
                span,
                expected,
                got,
            } => format!("{}Expected {}, but got {}\n", span.format(), expected, got),
            Self::InvalidEscape(span, ch) => format!("{}Invalid escape char {}", span.format(), ch),
            Self::NoSuchIntrinsic(span, name) => {
                format!("{}No such intrinsic: {}", span.format(), name)
            }
        }
    }
}
