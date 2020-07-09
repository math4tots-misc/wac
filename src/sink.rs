use crate::llir::*;
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

/// methods that correspond to webassembly constructs
impl Sink {
    pub fn data_directive(&self, ptr: WasmPtr, data: &[u8]) {
        self.write(format!("(data (i32.const {}) \"", ptr));
        for byte in data {
            self.write(format!("\\{:0>2X}", byte));
        }
        self.writeln("\")");
    }

    pub fn i32_sub(&self) {
        self.writeln("i32.sub");
    }

    pub fn i32_const(&self, value: i32) {
        self.writeln(format!("i32.const {}", value))
    }

    pub fn i64_const(&self, value: i64) {
        self.writeln(format!("i64.const {}", value))
    }

    pub fn f32_const(&self, value: f32) {
        self.writeln(format!("f32.const {}", value))
    }

    pub fn f64_const(&self, value: f64) {
        self.writeln(format!("f64.const {}", value))
    }

    pub fn i32_reinterpret_f32(&self) {
        self.writeln("i32.reinterpret_f32")
    }

    pub fn call(&self, name: &str) {
        self.writeln(format!("call {}", name))
    }

    pub fn local_get(&self, name: &str) {
        self.writeln(format!("local.get {}", name))
    }

    pub fn local_set(&self, name: &str) {
        self.writeln(format!("local.set {}", name))
    }

    pub fn local_tee(&self, name: &str) {
        self.writeln(format!("local.tee {}", name))
    }

    pub fn global_get(&self, name: &str) {
        self.writeln(format!("global.get {}", name))
    }

    pub fn global_set(&self, name: &str) {
        self.writeln(format!("global.set {}", name))
    }

    pub fn global_tee(&self, name: &str) {
        self.writeln(format!("global.tee {}", name))
    }
}
