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
pub struct Span {
    pub source: Rc<Source>,
    pub main: usize,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn join(&self, other: &Self) -> Self {
        // TODO: check self.source and other.source
        // are the same pointer
        Self {
            source: self.source.clone(),
            main: self.main,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }
    pub fn upto(&self, other: &Self) -> Self {
        // TODO: check self.source and other.source
        // are the same pointer
        Self {
            source: self.source.clone(),
            main: self.main,
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.start),
        }
    }
    pub fn format(&self) -> String {
        let i = self.main;
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
        // TODO: fix self.span.lineno
        // assert_eq!(
        //     self.span.lineno,
        //     self.source.data[..self.span.main].matches('\n').count()
        //         + 1
        //         + if self.source.data[self.span.main..].chars().next() == Some('\n') {
        //             1
        //         } else {
        //             0
        //         },
        // );
        // self.span.lineno
        self.source.data[..self.main].matches('\n').count() + 1
    }
}
