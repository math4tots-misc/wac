use crate::parse;
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
use std::rc::Rc;

const PAGE_SIZE: usize = 65536;

// all compile time allocations will align to 8 bytes
const DEFAULT_ALIGN_BYTES: usize = 8;

const PRELUDE_STR: &'static str = include_str!("prelude.wac");

// const TAG_INVALID: u32 = 0;
const TAG_I32: u32 = 1;
// const TAG_I64: u32 = 2;
const TAG_F32: u32 = 3;
// const TAG_F64: u32 = 4;
const TAG_STRING: u32 = 5;
// const TAG_LIST: u32 = 6;
// const TAG_ID: u32 = 7;

pub fn compile<N: Into<Rc<str>>, D: AsRef<str>>(
    name_data_pairs: Vec<(N, D)>,
) -> Result<String, CompileError> {
    let mut files = Vec::new();
    for (name, data) in name_data_pairs {
        let name = name.into();
        files.push(match parse(name.clone(), data.as_ref()) {
            Ok(result) => result,
            Err(error) => {
                return Err(CompileError::Parse(name, error).into());
            }
        });
    }
    files.push(match parse("<prelude>".into(), PRELUDE_STR) {
        Ok(result) => result,
        Err(error) => return Err(CompileError::Parse("<prelude>".into(), error).into()),
    });
    let file_refs: Vec<_> = files.iter().collect();
    compile_files(file_refs)
}

/// compiles wac source to wat (webassembly text format)
/// A module is made of zero or more files
/// A module may be compiled completely independently of all
/// dependencies
/// However, the tradeoff for this is that you need to know the
/// full type signature of every function that you pull from your
/// dependencies
fn compile_files(files: Vec<&LLFile>) -> Result<String, CompileError> {
    let mut out = Out::new();
    for file in &files {
        out.filename = file.name.clone();
        for import in &file.imports {
            match import {
                LLImport::Function(LLFunctionImport {
                    span,
                    module_name,
                    function_name,
                    imported_name,
                    type_,
                }) => {
                    // add this imported function to the global scope
                    out.add_global(imported_name.clone(), *span, Value::Function(type_.clone()))?;
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
        for func in &file.functions {
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
                *span,
                Value::Function(LLFunctionType {
                    parameters: parameters.iter().map(|pair| pair.1.clone()).collect(),
                    return_type: return_type.clone(),
                }),
            )?;
            // If this function has public visibility, we need to export it
            match visibility {
                LLVisibility::Public => {
                    out.exports
                        .writeln(format!("(export \"f_{}\" (func $f_{}))", name, name));
                }
                LLVisibility::Private => {}
            }
        }
    }
    for file in &files {
        out.filename = file.name.clone();
        for func in &file.functions {
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
                out.add_local(name.clone(), *span, Value::Local(type_.clone()))?;
                sink.write(format!(" (param $l_{} {})", name, translate_type(type_)));
            }
            sink.write(format!(" (result {})", translate_type(return_type)));
            for local in locals {
                let (name, type_) = local;
                out.add_local(name.clone(), *span, Value::Local(type_.clone()))?;
                sink.write(format!(" (local $l_{} {})", name, translate_type(type_)));
            }
            sink.writeln("");
            let body_sink = sink.spawn();
            translate_expr(&mut out, &body_sink, body, Some(return_type))?;
            sink.writeln(")");
        }
    }
    let out = out.get();
    // println!("out -> {}", out);
    Ok(out)
}

fn translate_expr(
    out: &mut Out,
    sink: &Rc<Sink>,
    expr: &LLExpr,
    expected: Option<&LLType>,
) -> Result<(), CompileError> {
    match expr {
        LLExpr::Int(span, value) => {
            match expected {
                Some(LLType::I32) => {
                    sink.writeln(format!("(i32.const {})", value));
                }
                Some(LLType::I64) => {
                    sink.writeln(format!("(i64.const {})", value));
                }
                Some(LLType::Id) => {
                    write_tagged_data(sink, TAG_I32, *value as i32 as u32);
                }
                Some(expected) => {
                    return Err(CompileError::Type {
                        filename: out.filename.clone(),
                        span: *span,
                        expected: format!("{:?}", expected),
                        got: "Int".into(),
                    })
                }
                None => {
                    // the value is not used, write nothing
                }
            }
        }
        LLExpr::Float(span, value) => {
            match expected {
                Some(LLType::F32) => {
                    sink.writeln(format!("(f32.const {})", value));
                }
                Some(LLType::F64) => {
                    sink.writeln(format!("(f64.const {})", value));
                }
                Some(LLType::Id) => {
                    write_tagged_float(sink, *value as f32);
                }
                Some(expected) => {
                    return Err(CompileError::Type {
                        filename: out.filename.clone(),
                        span: *span,
                        expected: format!("{:?}", expected),
                        got: "Float".into(),
                    })
                }
                None => {
                    // the value is not used, write nothing
                }
            }
        }
        LLExpr::String(span, string) => {
            match expected {
                Some(LLType::String) => {
                    let ptr = out.intern(string);
                    sink.writeln(format!("(i32.const {})", ptr));
                }
                Some(LLType::Id) => {
                    let ptr = out.intern(string);
                    write_tagged_data(sink, TAG_STRING, ptr);
                }
                Some(expected) => {
                    return Err(CompileError::Type {
                        filename: out.filename.clone(),
                        span: *span,
                        expected: format!("{:?}", expected),
                        got: "String".into(),
                    })
                }
                None => {
                    // the value is not used, write nothing
                }
            }
        }
        LLExpr::Block(span, exprs, last) => {
            for expr in exprs {
                translate_expr(out, sink, expr, None)?;
            }
            if let Some(last) = last {
                translate_expr(out, sink, last, expected)?;
            } else {
                match expected {
                    Some(LLType::Void) => sink.writeln("(i32.const 0)"),
                    Some(LLType::I32) => sink.writeln("(i32.const 0)"),
                    Some(LLType::I64) => sink.writeln("(i64.const 0)"),
                    Some(LLType::F32) => sink.writeln("(f32.const 0)"),
                    Some(LLType::F64) => sink.writeln("(f64.const 0)"),
                    Some(expected) => {
                        return Err(CompileError::Type {
                            filename: out.filename.clone(),
                            span: *span,
                            expected: format!("{:?}", expected),
                            got: "empty-block".into(),
                        });
                    }
                    None => {}
                }
            }
        }
        LLExpr::List(span, exprs) => {
            for expr in exprs {
                translate_expr(out, sink, expr, Some(&LLType::Id))?;
            }
            match expected {
                Some(_) => {
                    panic!("TODO: translate_expr List")
                }
                None => {
                    // If it's not used, we should clean up all the values
                    for _ in exprs {
                        drop(out, sink, &LLType::Id);
                    }
                }
            }
        }
        LLExpr::FunctionCall(span, fname, argexprs) => {
            match out.locals.get(fname).or_else(|| out.globals.get(fname)) {
                Some((_def_filename, _def_span, Value::Function(ftype))) => {
                    let ftype = ftype.clone();
                    if ftype.parameters.len() != argexprs.len() {
                        return Err(CompileError::Type {
                            filename: out.filename.clone(),
                            span: *span,
                            expected: format!("{} arguments", ftype.parameters.len()),
                            got: format!("{} arguments", argexprs.len()),
                        });
                    }
                    for i in 0..argexprs.len() {
                        translate_expr(out, sink, &argexprs[i], Some(&ftype.parameters[i]))?;
                    }
                    sink.writeln(format!("call $f_{}", fname));
                    if let Some(expected) = expected {
                        if &ftype.return_type != expected {
                            return Err(CompileError::Type {
                                filename: out.filename.clone(),
                                span: *span,
                                expected: format!("{:?}", expected),
                                got: format!("{:?}", ftype.return_type),
                            });
                        }
                    } else {
                        drop(out, sink, &ftype.return_type);
                    }
                }
                Some((_, _, val)) => {
                    return Err(CompileError::Type {
                        filename: out.filename.clone(),
                        span: *span,
                        expected: "Function".into(),
                        got: format!("{:?}", val),
                    })
                }
                None => {
                    return Err(CompileError::NoSuchName {
                        filename: out.filename.clone(),
                        span: *span,
                        name: fname.clone(),
                    })
                }
            }
        }
        LLExpr::GetVar(span, name) => {
            if let Some(expected) = expected {
                match out.locals.get(name).or_else(|| out.globals.get(name)) {
                    Some((_def_filename, _def_span, Value::Local(type_))) => {
                        if type_ == expected {
                            sink.writeln(format!("(local.get $l_{})", name));
                        } else {
                            return Err(CompileError::Type {
                                filename: out.filename.clone(),
                                span: *span,
                                expected: format!("{:?}", expected),
                                got: format!("{:?}", type_),
                            });
                        }
                    }
                    Some((_def_filename, _def_span, Value::Global(type_))) => {
                        if type_ == expected {
                            sink.writeln(format!("(global.get $g_{})", name));
                        } else {
                            return Err(CompileError::Type {
                                filename: out.filename.clone(),
                                span: *span,
                                expected: format!("{:?}", expected),
                                got: format!("{:?}", type_),
                            });
                        }
                    }
                    Some((_def_filename, _def_span, Value::Function(_))) => {
                        // unclear whether function pointers will be supported
                        return Err(CompileError::Type {
                            filename: out.filename.clone(),
                            span: *span,
                            expected: "Variable".into(),
                            got: "Function".into(),
                        });
                    }
                    None => {
                        return Err(CompileError::NoSuchName {
                            filename: out.filename.clone(),
                            span: *span,
                            name: name.clone(),
                        })
                    }
                }
            }
        }
        LLExpr::SetVar(span, name, valexpr) => {
            if let Some(expected) = expected {
                return Err(CompileError::Type {
                    filename: out.filename.clone(),
                    span: *span,
                    expected: format!("{:?}", expected),
                    got: "void".into(),
                });
            }
            match out.locals.get(name).or_else(|| out.globals.get(name)) {
                Some((_def_filename, _def_span, Value::Local(expected))) => {
                    let expected = expected.clone();
                    translate_expr(out, sink, valexpr, Some(&expected))?;
                    sink.writeln(format!("(local.set $l_{}", name));
                }
                Some((_def_filename, _def_span, Value::Global(expected))) => {
                    let expected = expected.clone();
                    translate_expr(out, sink, valexpr, Some(&expected))?;
                    sink.writeln(format!("(global.set $g_{}", name));
                }
                Some((_def_filename, _def_span, Value::Function(_))) => {
                    // unclear whether function pointers will be supported
                    return Err(CompileError::Type {
                        filename: out.filename.clone(),
                        span: *span,
                        expected: "Variable".into(),
                        got: "Function".into(),
                    });
                }
                None => {
                    return Err(CompileError::NoSuchName {
                        filename: out.filename.clone(),
                        span: *span,
                        name: name.clone(),
                    })
                }
            }
        }
        LLExpr::InlineAsm(span, typed_args, result_type, asm) => {
            for (type_, expr) in typed_args {
                translate_expr(out, sink, expr, Some(type_))?;
            }
            sink.writeln(asm);
            if let Some(expected) = expected {
                if result_type != expected {
                    return Err(CompileError::Type {
                        filename: out.filename.clone(),
                        span: *span,
                        expected: format!("{:?}", expected),
                        got: format!("{:?}", result_type),
                    });
                }
            } else {
                drop(out, sink, result_type);
            }
        }
    }
    Ok(())
}

/// drops a value on the top of the stack, taking care to release references as needed
fn drop(_out: &mut Out, sink: &Rc<Sink>, type_: &LLType) {
    match type_ {
        LLType::Void | LLType::I32 | LLType::I64 | LLType::F32 | LLType::F64 => {
            // for copy types, no other bookkeeping required
            sink.writeln("(drop)");
        }
        LLType::String => {
            sink.writeln("call $f___WAC_drop_str");
            sink.writeln("(drop)"); // drop return value of f___WAC_drop_str
        }
        LLType::List => {
            sink.writeln("call $f___WAC_drop_list");
            sink.writeln("(drop)"); // drop return value of f___WAC_drop_list
        }
        LLType::Id => {
            sink.writeln("call $f___WAC_drop_id");
            sink.writeln("(drop)"); // drop return value of f___WAC_drop_id
        }
    }
}

/// makes a new list from the top len elements from the stack
fn mklist(out: &mut Out, sink: &Rc<Sink>, len: usize) {

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

#[allow(dead_code)]
#[derive(Debug)]
enum Value {
    Function(LLFunctionType),
    Local(LLType),
    Global(LLType),
}

impl Value {}

struct Out {
    filename: Rc<str>,
    main: Rc<Sink>,
    imports: Rc<Sink>,
    memory: Rc<Sink>,
    data: Rc<Sink>,
    functions: Rc<Sink>,
    exports: Rc<Sink>,

    next_free_memory_pos: usize,

    /// maps strings to their interned location in memory
    intern_map: HashMap<Rc<str>, u32>,

    /// essentially the global scope
    globals: HashMap<Rc<str>, (Rc<str>, Span, Value)>,

    /// For local variables
    locals: HashMap<Rc<str>, (Rc<str>, Span, Value)>,
}

impl Out {
    fn new() -> Self {
        let main = Sink::new();
        main.writeln("(module");
        let imports = main.spawn();
        let memory = main.spawn();
        let data = main.spawn();
        data.writeln(r#"(data $rt_mem (i32.const 0) "\00\00\00\00\00\00\00\00")"#);
        let functions = main.spawn();
        let exports = main.spawn();
        main.writeln(")");
        Self {
            filename: "".into(),
            main,
            imports,
            memory,
            data,
            functions,
            exports,

            // we start from 8, because we reserve the first 8 bytes
            // for nullptr
            next_free_memory_pos: 8,

            intern_map: HashMap::new(),

            globals: HashMap::new(),
            locals: HashMap::new(),
        }
    }
    /// reserve the next n bytes and return an index to the beginning
    /// of that chunk
    fn alloc(&mut self, n: usize) -> usize {
        assert_ne!(n, 0);
        // round to smallest multiple of DEFAULT_ALIGN_BYTES greater than or equal to n.
        let n = ((n + (DEFAULT_ALIGN_BYTES - 1)) / DEFAULT_ALIGN_BYTES) * DEFAULT_ALIGN_BYTES;
        let start = self.next_free_memory_pos;
        self.next_free_memory_pos += n;
        start
    }
    /// Ensure the given string is interned, and return its location in memory
    fn intern(&mut self, s: &Rc<str>) -> u32 {
        if !self.intern_map.contains_key(s) {
            let mut buffer = Vec::<u8>::new();

            // reference count
            buffer.extend(&(1i32.to_le_bytes()));

            // size
            buffer.extend(&((s.len() as i32).to_le_bytes()));

            // the actual data itself
            buffer.extend(s.as_bytes());

            let ptr = self.store(&buffer) as u32;

            self.intern_map.insert(s.clone(), ptr);
        }
        *self.intern_map.get(s).unwrap()
    }
    /// Store the given bytes to memory, and return its location
    fn store(&mut self, bytes: &[u8]) -> usize {
        let start = self.alloc(bytes.len());
        self.data.write(format!(r#"(data $rt_mem (i32.const {}) ""#, start));
        self.data.write_escaped_bytes(bytes);
        self.data.writeln(r#"")"#);
        start
    }
    fn get(self) -> String {
        let used_page_count = self.next_free_memory_pos / PAGE_SIZE + 1;
        self.memory.writeln(format!(
            r#"(memory $rt_mem (export "rt_mem") {})"#,
            used_page_count
        ));
        self.data.writeln(format!(
            r#"(global $rt_heap_start i32 (i32.const {}))"#,
            self.next_free_memory_pos
        ));
        self.data.writeln(format!(
            r#"(global $rt_heap_top (mut i32) (i32.const {}))"#,
            self.next_free_memory_pos
        ));
        self.main.get()
    }
    fn add_global(&mut self, key: Rc<str>, span: Span, value: Value) -> Result<(), CompileError> {
        let filename = self.filename.clone();
        if let Some((filename2, span2, _value2)) = self.globals.get(&key) {
            return Err(CompileError::DuplicateDefinition {
                key,
                filename1: filename,
                span1: span,
                filename2: filename2.clone(),
                span2: *span2,
            });
        }
        self.globals.insert(key, (filename, span, value));
        Ok(())
    }
    fn add_local(&mut self, key: Rc<str>, span: Span, value: Value) -> Result<(), CompileError> {
        let filename = self.filename.clone();
        if let Some((filename2, span2, _value2)) = self.locals.get(&key) {
            return Err(CompileError::DuplicateDefinition {
                key,
                filename1: filename,
                span1: span,
                filename2: filename2.clone(),
                span2: *span2,
            });
        }
        self.locals.insert(key, (filename, span, value));
        Ok(())
    }
}

fn write_tagged_data(sink: &Rc<Sink>, tag: u32, data: u32) {
    sink.writeln(format!("(i64.const {})", tag));
    sink.writeln("(i64.shl 32)");
    sink.writeln(format!("(i64.const {})", data));
    sink.writeln("i64.or");
}

fn write_tagged_float(sink: &Rc<Sink>, data: f32) {
    sink.writeln(format!("(i64.const {})", TAG_F32));
    sink.writeln("(i64.shl 32)");
    sink.writeln(format!("(f32.const {})", data));
    sink.writeln("i32.reinterpret_f32");
    sink.writeln("i64.extend_i32_u");
    sink.writeln("i64.or");
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
    fn write_escaped_bytes<B: AsRef<[u8]>>(&self, bb: B) {
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
    Parse(Rc<str>, ParseError),
    DuplicateDefinition {
        key: Rc<str>,
        filename1: Rc<str>,
        filename2: Rc<str>,
        span1: Span,
        span2: Span,
    },
    Type {
        filename: Rc<str>,
        span: Span,
        expected: String,
        got: String,
    },
    NoSuchName {
        filename: Rc<str>,
        span: Span,
        name: Rc<str>,
    },
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
