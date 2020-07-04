use std::cell::RefCell;
use std::rc::Rc;

pub enum Part {
    Sink(Rc<Sink>),
    String(String),
}

pub struct Sink {
    parts: RefCell<Vec<Part>>,
}

impl Sink {
    pub fn new() -> Rc<Self> {
        Rc::new(Self {
            parts: RefCell::new(Vec::new()),
        })
    }
    pub fn spawn(&self) -> Rc<Sink> {
        let sink = Sink::new();
        self.parts.borrow_mut().push(Part::Sink(sink.clone()));
        sink
    }
    pub fn writeln<S: AsRef<str>>(&self, s: S) {
        self.parts
            .borrow_mut()
            .push(Part::String(format!("{}\n", s.as_ref())));
    }
    pub fn write<S: Into<String>>(&self, s: S) {
        self.parts.borrow_mut().push(Part::String(s.into()));
    }
    pub fn write_escaped_bytes<B: AsRef<[u8]>>(&self, bb: B) {
        let mut string = String::new();
        for b in bb.as_ref() {
            string.push_str(&format!("\\{:0>2X}", b));
        }
        self.parts.borrow_mut().push(Part::String(string));
    }
    pub fn get(&self) -> String {
        let mut string = String::new();
        for part in self.parts.borrow().iter() {
            match part {
                Part::Sink(sink) => {
                    string.push_str(&sink.get());
                }
                Part::String(s) => {
                    string.push_str(&s);
                }
            }
        }
        string
    }
}
