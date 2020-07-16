use crate::Span;

pub struct Error {
    pub span: Vec<Span>,
    pub message: String,
}

impl Error {
    pub fn format(&self) -> String {
        let mut ret = String::new();
        for span in &self.span {
            ret.push_str(&span.format());
        }
        ret.push_str(&self.message);
        ret
    }
}

impl From<wabt::Error> for Error {
    fn from(e: wabt::Error) -> Self {
        Self {
            span: vec![],
            message: format!("{:?}", e),
        }
    }
}

impl From<wr::error::Error> for Error {
    fn from(e: wr::error::Error) -> Self {
        Self {
            span: vec![],
            message: format!("{:?}", e),
        }
    }
}

impl From<wr::error::ResolveError> for Error {
    fn from(e: wr::error::ResolveError) -> Self {
        Self {
            span: vec![],
            message: format!("{:?}", e),
        }
    }
}

impl From<wr::error::RuntimeError> for Error {
    fn from(e: wr::error::RuntimeError) -> Self {
        Self {
            span: vec![],
            message: format!("{:?}", e),
        }
    }
}
