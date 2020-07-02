use crate::LLExpr;
use crate::LLFile;
use crate::LLFunction;
use crate::LLFunctionImport;
use crate::LLFunctionType;
use crate::LLImport;
use crate::LLType;
use crate::LLValueType;
use crate::LLVisibility;
use crate::ParseError;
use crate::Span;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::Infallible;
use std::convert::TryInto;
use std::rc::Rc;

/// compiles wac source to wat (webassembly text format)
/// A module is made of zero or more files
/// A module may be compiled completely independently of all
/// dependencies
/// However, the tradeoff for this is that you need to know the
/// full type signature of every function that you pull from your
/// dependencies
pub fn compile(files: Vec<&LLFile>) -> Result<String, CompileError> {
    let (imports, functions) = {
        let mut imports = Vec::new();
        let mut functions = Vec::new();
        for file in files {
            for imp in &file.imports {
                imports.push(imp);
            }
            for func in &file.functions {
                functions.push(func);
            }
        }
        (imports, functions)
    };
    let mut out = Out::new();
    for import in imports {
        match import {
            LLImport::Function(LLFunctionImport {
                span,
                module_name,
                function_name,
                imported_name,
                type_,
            }) => {
                // add this imported function to the global scope
                out.add_global(imported_name.clone(), Value::Function(*span, type_.clone()))?;
                // actually emit the required import code
                out.imports.writeln(format!(
                    "(import \"{}\" \"{}\" (func $f_{} {}))",
                    module_name,
                    function_name,
                    imported_name,
                    translate_function_type(type_),
                ));
            }
        }
    }
    for func in &functions {
        let LLFunction {
            span,
            visibility,
            name,
            parameters,
            return_type,
            locals: _,
            body: _,
        } = func;
        // add this function to the global scope
        out.add_global(
            name.clone(),
            Value::Function(
                *span,
                LLFunctionType {
                    parameters: parameters.iter().map(|pair| pair.1.clone()).collect(),
                    return_type: return_type.clone(),
                },
            ),
        )?;
        // If this function has public visibility, we need to export it
        match visibility {
            LLVisibility::Public => {
                out.exports
                    .writeln(format!("(export \"{}\" (func $f_{}))", name, name));
            }
            LLVisibility::Private => {}
        }
    }
    for func in &functions {
        let LLFunction {
            span,
            visibility: _,
            name,
            parameters,
            return_type,
            locals,
            body,
        } = func;
        let sink = out.functions.spawn();
        sink.write(format!("(func $f_{}", name));
        out.locals.clear();
        for param in parameters {
            let (name, type_) = param;
            out.add_local(name.clone(), Value::Local(*span, type_.clone()))?;
            sink.write(format!(" (param $l_{} {})", name, translate_type(type_)));
        }
        sink.write(format!(" (result {})", translate_type(return_type)));
        for local in locals {
            let (name, type_) = local;
            out.add_local(name.clone(), Value::Local(*span, type_.clone()))?;
            sink.write(format!(" (local $l_{} {})", name, translate_type(type_)));
        }
        sink.writeln("");
        let body_sink = sink.spawn();
        translate_expr(&mut out, body_sink, body, Some(return_type))?;
        sink.writeln(")");
    }
    Ok(out.get())
}

fn translate_expr(out: &mut Out, sink: Rc<Sink>, expr: &LLExpr, expected: Option<&LLType>) -> Result<(), CompileError> {
    panic!("TODO")
}

fn translate_function_type(ft: &LLFunctionType) -> String {
    let mut s = String::new();
    for param in &ft.parameters {
        s.push_str(&format!(" (param {})", translate_type(param)));
    }
    s.push_str(&format!(" (result {})", translate_type(&ft.return_type)));
    s
}

fn translate_type(t: &LLType) -> &str {
    match t.to_value_type() {
        LLValueType::I32 => "i32",
        LLValueType::I64 => "i64",
        LLValueType::F32 => "f32",
        LLValueType::F64 => "f64",
    }
}

enum Value {
    Function(Span, LLFunctionType),
    Local(Span, LLType),
}

impl Value {
    fn span(&self) -> Span {
        match self {
            Value::Function(span, _) => span.clone(),
            Value::Local(span, _) => span.clone(),
        }
    }
}

struct Out {
    main: Rc<Sink>,
    imports: Rc<Sink>,
    functions: Rc<Sink>,
    exports: Rc<Sink>,

    /// essentially the global scope
    globals: HashMap<Rc<str>, Value>,

    /// For local variables
    locals: HashMap<Rc<str>, Value>,
}

impl Out {
    fn new() -> Self {
        let main = Sink::new();
        main.writeln("(module");
        let imports = main.spawn();
        let functions = main.spawn();
        let exports = main.spawn();
        main.writeln(")");
        Self {
            main,
            imports,
            functions,
            exports,
            globals: HashMap::new(),
            locals: HashMap::new(),
        }
    }
    fn get(self) -> String {
        self.main.get()
    }
    fn add_global(&mut self, key: Rc<str>, value: Value) -> Result<(), CompileError> {
        if let Some(old_value) = self.globals.get(&key) {
            let old = old_value.span();
            let new = value.span();
            return Err(CompileError::DuplicateDefinition { key, old, new });
        }
        self.globals.insert(key, value);
        Ok(())
    }
    fn add_local(&mut self, key: Rc<str>, value: Value) -> Result<(), CompileError> {
        if let Some(old_value) = self.locals.get(&key) {
            let old = old_value.span();
            let new = value.span();
            return Err(CompileError::DuplicateDefinition { key, old, new });
        }
        self.locals.insert(key, value);
        Ok(())
    }
}

enum Part {
    Sink(Rc<Sink>),
    String(String),
}

struct Sink {
    parts: RefCell<Vec<Part>>,
}

impl Sink {
    fn new() -> Rc<Self> {
        Rc::new(Self {
            parts: RefCell::new(Vec::new()),
        })
    }
    fn spawn(&self) -> Rc<Sink> {
        let sink = Sink::new();
        self.parts.borrow_mut().push(Part::Sink(sink.clone()));
        sink
    }
    fn writeln<S: AsRef<str>>(&self, s: S) {
        self.parts
            .borrow_mut()
            .push(Part::String(format!("{}\n", s.as_ref())));
    }
    fn write<S: Into<String>>(&self, s: S) {
        self.parts.borrow_mut().push(Part::String(s.into()));
    }
    fn escaped_bytes<B: AsRef<[u8]>>(&self, bb: B) {
        let mut string = String::new();
        for b in bb.as_ref() {
            string.push_str(&format!("{:0>2X}", b));
        }
        self.parts.borrow_mut().push(Part::String(string));
    }
    fn get(&self) -> String {
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

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    DuplicateDefinition { key: Rc<str>, old: Span, new: Span },
}

impl From<ParseError> for CompileError {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
}

impl From<Infallible> for CompileError {
    fn from(_: Infallible) -> Self {
        panic!("Infallible")
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn formatting() {
        assert_eq!(&format!("{:0>2X}", 1), "01",);
        assert_eq!(&format!("{:0>2X}", 15), "0F",);
        assert_eq!(&format!("{:0>2X}", 0xAB), "AB",);
    }
}
