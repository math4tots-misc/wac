use crate::LexError;
use crate::ParseError;
use crate::Span;
use std::fmt;
use std::rc::Rc;

pub struct Source {
    pub name: Rc<str>,
    pub data: Rc<str>,
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Source({})", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct SSpan {
    pub source: Rc<Source>,
    pub span: Span,
}

impl SSpan {
    pub(crate) fn join(&self, other: &Self) -> Self {
        // TODO: check self.source and other.source
        // are the same pointer
        Self {
            source: self.source.clone(),
            span: self.span.join(other.span),
        }
    }
    pub(crate) fn upto(&self, other: &Self) -> Self {
        // TODO: check self.source and other.source
        // are the same pointer
        Self {
            source: self.source.clone(),
            span: self.span.upto(other.span),
        }
    }
    pub fn format(&self) -> String {
        let i = self.span.main;
        let lineno = self.lineno();
        let lstart = self.source.data[..i]
            .rfind('\n')
            .map(|j| j + 1)
            .unwrap_or(0);
        let lend = self.source.data[i..]
            .find('\n')
            .map(|j| i + j)
            .unwrap_or(self.source.data.len());
        let line = &self.source.data[lstart..lend];
        format!(
            "in {} on line {}\n{}\n{}*\n",
            self.source.name,
            lineno,
            line,
            " ".repeat(i - lstart)
        )
    }
    pub fn lineno(&self) -> usize {
        assert_eq!(self.span.lineno, self.source.data[..self.span.main].matches('\n').count() + 1);
        self.span.lineno
    }
}

#[derive(Debug)]
pub enum Error {
    Lex(SSpan, LexError),
    Parse(ParseError),
    Wabt(wabt::Error),
    Wasmer(wr::error::Error),
    Type {
        span: SSpan,
        expected: String,
        got: String,
    },
    ConflictingDefinitions {
        span1: SSpan,
        span2: SSpan,
        name: Rc<str>,
    },
}

impl Error {
    pub fn from_lex(source: Rc<Source>, le: LexError) -> Self {
        let span = SSpan {
            source,
            span: le.span(),
        };
        Self::Lex(span, le)
    }

    pub fn format(&self) -> String {
        match self {
            Self::Lex(span, er) => format!("{}{:?}", span.format(), er),
            Self::Parse(er) => er.format(),
            Self::Type {
                span,
                expected,
                got,
            } => format!("{}Expected {} but got {}", span.format(), expected, got),
            Self::ConflictingDefinitions { span1, span2, name } => format!(
                "{}{}Conflicting definitions for {}",
                span1.format(),
                span2.format(),
                name
            ),
            Self::Wabt(_) | Self::Wasmer(_) => format!("{:?}", self),
        }
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
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
