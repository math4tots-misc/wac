use crate::llir::*;
use crate::Scope;
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

    pub fn memory_directive(&self, page_count: usize) {
        self.writeln(format!("(memory $rt_mem {})", page_count));
    }

    pub fn global(&self, type_: WasmType, name: &str, value: i64) {
        self.writeln(format!(
            "(global {} {} ({}.const {}))",
            name, type_, type_, value
        ));
    }

    pub fn global_mut(&self, type_: WasmType, name: &str, value: i64) {
        self.writeln(format!(
            "(global {} (mut {}) ({}.const {}))",
            name, type_, type_, value
        ));
    }

    pub fn i32_add(&self) {
        self.writeln("i32.add");
    }

    pub fn i32_div_s(&self) {
        self.writeln("i32.div_s");
    }

    pub fn f32_div(&self) {
        self.writeln("f32.div");
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

    pub fn drop_(&self) {
        self.writeln("drop");
    }

    pub fn unreachable(&self) {
        self.writeln("unreachable");
    }

    pub fn f32_convert_i32_s(&self) {
        self.writeln("f32.convert_i32_s")
    }

    pub fn i32_reinterpret_f32(&self) {
        self.writeln("i32.reinterpret_f32")
    }

    pub fn i32_eqz(&self) {
        self.writeln("i32.eqz")
    }

    pub fn i32_trunc_f32_s(&self) {
        self.writeln("i32.trunc_f32_s")
    }

    pub fn i64_extend_i32_u(&self) {
        self.writeln("i64.extend_i32_u")
    }

    pub fn call(&self, name: &str) {
        self.writeln(format!("call {}", name))
    }

    pub(crate) fn var_get(&self, scope: Scope, name: &str) {
        match scope {
            Scope::Global => self.global_get(name),
            Scope::Local => self.local_get(name),
        }
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

    pub fn start_block(&self, break_label: u32, type_: Option<WasmType>) {
        self.writeln(format!(
            "(block $lbl_{}{}",
            break_label,
            match type_ {
                Some(type_) => format!(" (result {})", type_),
                None => "".to_owned(),
            },
        ));
    }

    pub fn end_block(&self) {
        self.writeln(")");
    }

    pub fn start_loop(&self, break_label: u32, continue_label: u32) {
        self.writeln(format!(
            "(block $lbl_{} (loop $lbl_{}",
            break_label, continue_label
        ));
    }

    pub fn br(&self, label: u32) {
        self.writeln(format!("br $lbl_{}", label));
    }

    pub fn br_if(&self, label: u32) {
        self.writeln(format!("br_if $lbl_{}", label));
    }

    pub fn end_loop(&self) {
        self.writeln("))")
    }
}
