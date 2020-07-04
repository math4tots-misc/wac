use crate::LexError;
use crate::ParseError;
use crate::Span;
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    Lex(Rc<str>, LexError),
    Parse(Rc<str>, ParseError),
    Wabt(wabt::Error),
    Wasmer(wr::error::Error),
    Type {
        span: Span,
        expected: String,
        got: String,
    },
}

impl From<wabt::Error> for Error {
    fn from(e: wabt::Error) -> Self {
        Self::Wabt(e)
    }
}

impl From<wr::error::Error> for Error {
    fn from(e: wr::error::Error) -> Self {
        Self::Wasmer(e)
    }
}

impl From<wr::error::ResolveError> for Error {
    fn from(e: wr::error::ResolveError) -> Self {
        Self::Wasmer(e.into())
    }
}

impl From<wr::error::RuntimeError> for Error {
    fn from(e: wr::error::RuntimeError) -> Self {
        Self::Wasmer(e.into())
    }
}
