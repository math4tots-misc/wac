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

/// compiles wac source to wat (webassembly text format)
/// A module is made of zero or more files
/// A module may be compiled completely independently of all
/// dependencies
/// However, the tradeoff for this is that you need to know the
/// full type signature of every function that you pull from your
/// dependencies
pub fn compile(files: Vec<&LLFile>) -> Result<String, CompileError> {
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
                        .writeln(format!("(export \"{}\" (func $f_{}))", name, name));
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
    println!("out -> {}", out);
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
        LLExpr::String(..) => {
            panic!("TODO: translate_expr String");
        }
        LLExpr::Block(span, exprs, last) => {
            for expr in exprs {
                translate_expr(out, sink, expr, None)?;
            }
            if let Some(last) = last {
                translate_expr(out, sink, last, expected)?;
            } else {
                match expected {
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
                    sink.writeln(format!("(call $f_{})", fname));
                    if let Some(expected) = expected {
                        if &ftype.return_type != expected {
                            return Err(CompileError::Type {
                                filename: out.filename.clone(),
                                span: *span,
                                expected: format!("{:?}", expected),
                                got: format!("{:?}", ftype.return_type),
                            })
                        }
                    } else {
                        sink.writeln("drop");
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
                    Some((_, _, v)) => {
                        panic!("TODO: GetVar: {:?}", v);
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
                Some((_, _, v)) => {
                    panic!("TODO: SetVar: {:?}", v);
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
    Ok(())
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
    functions: Rc<Sink>,
    exports: Rc<Sink>,

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
        let functions = main.spawn();
        let exports = main.spawn();
        main.writeln(")");
        Self {
            filename: "".into(),
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
    #[allow(dead_code)]
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
