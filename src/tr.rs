use crate::ir::*;
use crate::parse_file;
use crate::Binop;
use crate::Error;
use crate::Parser;
use crate::SSpan;
use crate::Sink;
use crate::Source;
use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

/// Number of bytes at start of memory that's reserved
/// Compile-time constants stored in memory start from this location
pub const RESERVED_BYTES: usize = 2048;

/// translates a list of (filename, wac-code) pairs into
/// a wat webassembly module
pub fn translate(mut sources: Vec<(Rc<str>, Rc<str>)>) -> Result<String, Error> {
    let prelude_name: Rc<str> = "[prelude]".into();
    let prelude_str: Rc<str> = include_str!("prelude.wac").into();
    sources.insert(0, (prelude_name, prelude_str));
    let mut files = Vec::new();
    for (filename, data) in sources {
        let source = Rc::new(Source {
            name: filename.clone(),
            data: data.clone(),
        });
        let mut parser = match Parser::new(&source) {
            Ok(parser) => parser,
            Err(error) => return Err(Error::from_lex(source.clone(), error)),
        };
        let file = parse_file(&mut parser)?;
        files.push((filename, file));
    }
    let mut out = Out::new();
    let mut functions = HashMap::new();

    // collect all function signatures
    for (_filename, file) in &files {
        for imp in &file.imports {
            match imp {
                Import::Function(FunctionImport { alias, type_, .. }) => {
                    functions.insert(alias.clone(), type_.clone());
                }
            }
        }
        for func in &file.functions {
            functions.insert(func.name.clone(), func.type_().clone());
        }
    }
    let mut gscope = GlobalScope::new(functions);

    // translate all global variables
    // NOTE: global variables that appear before cannot refer to
    // global variables that appear later
    // NOTE: it kinda sucks that the behavior of the code will depend on the
    // order in which you provide the files
    for (_filename, file) in &files {
        for gvar in &file.globalvars {
            let mut lscope = LocalScope::new(&gscope);
            let type_ = if let Some(t) = gvar.type_ {
                t
            } else {
                guess_type(&mut lscope, &gvar.init)?
            };
            let init_sink = out.start.spawn();
            translate_expr(&mut out, &init_sink, &mut lscope, Some(type_), &gvar.init)?;
            let info = gscope.decl(gvar.span.clone(), gvar.name.clone(), type_)?;
            init_sink.writeln(format!("global.set {}", info.wasm_name));
            out.gvars.writeln(format!(
                "(global {} (mut {}) ({}.const 0))",
                info.wasm_name,
                translate_type(info.type_),
                translate_type(info.type_),
            ));
        }
    }

    // translate the functions
    for (_filename, file) in files {
        for imp in file.imports {
            translate_import(&out, imp);
        }
        for func in file.functions {
            translate_func(&mut out, &gscope, func)?;
        }
    }
    Ok(out.get())
}

struct GlobalScope {
    functions: HashMap<Rc<str>, FunctionType>,
    varmap: HashMap<Rc<str>, Rc<GlobalVarInfo>>,
    decls: Vec<Rc<GlobalVarInfo>>,
}

impl GlobalScope {
    fn new(functions: HashMap<Rc<str>, FunctionType>) -> Self {
        Self {
            functions,
            varmap: HashMap::new(),
            decls: vec![],
        }
    }

    fn decl(
        &mut self,
        span: SSpan,
        name: Rc<str>,
        type_: Type,
    ) -> Result<Rc<GlobalVarInfo>, Error> {
        if let Some(info) = self.varmap.get(&name) {
            return Err(Error::ConflictingDefinitions {
                span1: info.span.clone(),
                span2: span,
                name,
            });
        }
        let wasm_name = format!("$g_{}", name).into();
        let info = Rc::new(GlobalVarInfo {
            span,
            original_name: name.clone(),
            type_,
            wasm_name,
        });
        self.decls.push(info.clone());
        self.varmap.insert(name.clone(), info.clone());
        Ok(info)
    }
}

/// global variable declaration
struct GlobalVarInfo {
    #[allow(dead_code)]
    span: SSpan,
    #[allow(dead_code)]
    original_name: Rc<str>,
    type_: Type,
    wasm_name: Rc<str>,
}

/// local variable declaration
struct LocalVarInfo {
    #[allow(dead_code)]
    span: SSpan,

    /// the programmer provided name for this variable
    original_name: Rc<str>,

    type_: Type,

    wasm_name: Rc<str>,
}

struct LocalScope<'a> {
    g: &'a GlobalScope,
    locals: Vec<HashMap<Rc<str>, Rc<LocalVarInfo>>>,
    nlabels: usize,
    continue_labels: Vec<u32>,
    break_labels: Vec<u32>,
    decls: Vec<Rc<LocalVarInfo>>,
}

impl<'a> LocalScope<'a> {
    fn new(g: &'a GlobalScope) -> Self {
        Self {
            g,
            locals: vec![HashMap::new()],
            nlabels: 0,
            continue_labels: vec![],
            break_labels: vec![],
            decls: vec![],
        }
    }
    fn push(&mut self) {
        self.locals.push(HashMap::new());
    }
    fn pop(&mut self) {
        self.locals.pop().unwrap();
    }
    fn decl(&mut self, span: SSpan, original_name: Rc<str>, type_: Type) -> Rc<LocalVarInfo> {
        let id = self.decls.len();
        let wasm_name = format!("$l_{}_{}", id, original_name).into();
        let info = Rc::new(LocalVarInfo {
            span,
            original_name,
            type_,
            wasm_name,
        });
        self.decls.push(info.clone());
        self.locals
            .last_mut()
            .unwrap()
            .insert(info.original_name.clone(), info.clone());
        info
    }
    fn get(&self, name: &Rc<str>) -> Option<ScopeEntry> {
        for map in self.locals.iter().rev() {
            match map.get(name) {
                Some(t) => return Some(ScopeEntry::Local(t.clone())),
                None => {}
            }
        }
        match self.g.varmap.get(name) {
            Some(t) => Some(ScopeEntry::Global(t.clone())),
            None => None,
        }
    }
    fn get_or_err(&self, span: SSpan, name: &Rc<str>) -> Result<ScopeEntry, Error> {
        match self.get(name) {
            Some(e) => Ok(e),
            None => Err(Error::Type {
                span,
                expected: format!("Variable {}", name),
                got: "NotFound".into(),
            }),
        }
    }
    fn getf(&self, name: &Rc<str>) -> Option<FunctionType> {
        self.g.functions.get(name).cloned()
    }
    fn getf_or_err(&self, span: SSpan, name: &Rc<str>) -> Result<FunctionType, Error> {
        match self.getf(name) {
            Some(e) => Ok(e),
            None => Err(Error::Type {
                span,
                expected: format!("Function {}", name),
                got: "NotFound".into(),
            }),
        }
    }
    fn new_label_id(&mut self) -> u32 {
        let id = self.nlabels as u32;
        self.nlabels += 1;
        id
    }
}

enum ScopeEntry {
    Local(Rc<LocalVarInfo>),
    Global(Rc<GlobalVarInfo>),
}

fn translate_func_type(ft: &FunctionType) -> String {
    let mut ret = String::new();
    let FunctionType {
        return_type,
        parameter_types,
    } = ft;
    for pt in parameter_types {
        ret.push_str(&format!(" (param {})", translate_type(*pt)));
    }
    if let Some(rt) = return_type {
        ret.push_str(&format!(" (result {})", translate_type(*rt)));
    }
    ret
}

fn translate_type(t: Type) -> &'static str {
    match t.wasm() {
        WasmType::I32 => "i32",
        WasmType::I64 => "i64",
        WasmType::F32 => "f32",
        WasmType::F64 => "f64",
    }
}

fn translate_import(out: &Out, imp: Import) {
    match imp {
        Import::Function(FunctionImport {
            span: _,
            module_name,
            function_name,
            alias,
            type_,
        }) => {
            out.imports.writeln(format!(
                r#"(import "{}" "{}" (func $f_{} {}))"#,
                module_name,
                function_name,
                alias,
                translate_func_type(&type_),
            ));
        }
    }
}

fn translate_func(out: &mut Out, gscope: &GlobalScope, func: Function) -> Result<(), Error> {
    let mut lscope = LocalScope::new(gscope);

    match func.visibility {
        Visibility::Public => {
            out.exports.writeln(format!(
                r#"(export "f_{}" (func $f_{}))"#,
                func.name, func.name
            ));
        }
        Visibility::Private => {}
    }

    let sink = out.funcs.spawn();
    sink.writeln(format!("(func $f_{}", func.name));

    for parameter in &func.parameters {
        let info = lscope.decl(func.span.clone(), parameter.0.clone(), parameter.1);
        sink.writeln(format!(
            " (param {} {})",
            info.wasm_name,
            translate_type(info.type_)
        ));
    }
    if let Some(return_type) = func.return_type {
        sink.writeln(format!(" (result {})", translate_type(return_type)));
    }
    // we won't know what locals we have until we finish translate_expr on the body
    let locals_sink = sink.spawn();
    translate_expr(out, &sink, &mut lscope, func.return_type, &func.body)?;
    sink.writeln(")");

    // declare all the local variables (skipping parameters)
    for info in lscope.decls.into_iter().skip(func.parameters.len()) {
        locals_sink.writeln(format!(
            " (local {} {})",
            info.wasm_name,
            translate_type(info.type_)
        ));
    }
    Ok(())
}

fn translate_expr(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: Option<Type>,
    expr: &Expr,
) -> Result<(), Error> {
    match expr {
        Expr::Bool(span, x) => {
            match etype {
                Some(Type::Bool) => {
                    sink.writeln(format!("(i32.const {})", if *x { 1 } else { 0 }));
                }
                Some(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "Bool".into(),
                    })
                }
                None => {
                    // no-op value is dropped
                }
            }
        }
        Expr::Int(span, x) => {
            match etype {
                Some(Type::I32) => {
                    sink.writeln(format!("(i32.const {})", x));
                }
                Some(Type::I64) => {
                    sink.writeln(format!("(i64.const {})", x));
                }
                Some(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "Int".into(),
                    })
                }
                None => {
                    // no-op value is dropped
                }
            }
        }
        Expr::Float(span, x) => {
            match etype {
                Some(Type::F32) => {
                    sink.writeln(format!("(f32.const {})", x));
                }
                Some(Type::F64) => {
                    sink.writeln(format!("(f64.const {})", x));
                }
                Some(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "Float".into(),
                    })
                }
                None => {
                    // no-op value is dropped
                }
            }
        }
        Expr::Block(span, exprs) => {
            if let Some(last) = exprs.last() {
                lscope.push();

                for expr in &exprs[..exprs.len() - 1] {
                    translate_expr(out, sink, lscope, None, expr)?;
                }
                translate_expr(out, sink, lscope, etype, last)?;

                lscope.pop();
            } else {
                match etype {
                    None => {}
                    Some(t) => {
                        return Err(Error::Type {
                            span: span.clone(),
                            expected: format!("{:?}", t),
                            got: "Void (empty-block)".into(),
                        })
                    }
                }
            }
        }
        Expr::GetVar(span, name) => {
            let entry = lscope.get_or_err(span.clone(), name)?;
            match entry {
                ScopeEntry::Local(info) => {
                    match etype {
                        Some(etype) if etype == info.type_ => {
                            sink.writeln(format!("local.get {}", info.wasm_name));
                        }
                        Some(etype) => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{:?}", etype),
                                got: format!("{:?} (localvar)", info.type_),
                            })
                        }
                        None => {
                            // we already checked this variable exists,
                            // if we don't use the return value,
                            // there's nothing we need to do here
                        }
                    }
                }
                ScopeEntry::Global(info) => {
                    match etype {
                        Some(etype) if etype == info.type_ => {
                            sink.writeln(format!("global.get {}", info.wasm_name));
                        }
                        Some(etype) => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{:?}", etype),
                                got: format!("{:?} (globalvar)", info.type_),
                            })
                        }
                        None => {
                            // we already checked this variable exists,
                            // if we don't use the return value,
                            // there's nothing we need to do here
                        }
                    }
                }
            }
        }
        Expr::SetVar(span, name, setexpr) => {
            let entry = lscope.get_or_err(span.clone(), name)?;
            if let Some(etype) = etype {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{:?}", etype),
                    got: "Void (setvar)".into(),
                });
            }
            match entry {
                ScopeEntry::Local(info) => match etype {
                    Some(etype) => {
                        return Err(Error::Type {
                            span: span.clone(),
                            expected: format!("{:?}", etype),
                            got: "Void (local.setvar)".into(),
                        })
                    }
                    None => {
                        translate_expr(out, sink, lscope, Some(info.type_), setexpr)?;
                        sink.writeln(format!("local.set {}", info.wasm_name));
                    }
                },
                ScopeEntry::Global(info) => match etype {
                    Some(etype) => {
                        return Err(Error::Type {
                            span: span.clone(),
                            expected: format!("{:?}", etype),
                            got: "Void (global.setvar)".into(),
                        })
                    }
                    None => {
                        translate_expr(out, sink, lscope, Some(info.type_), setexpr)?;
                        sink.writeln(format!("global.set {}", info.wasm_name));
                    }
                },
            }
        }
        Expr::DeclVar(span, name, type_, setexpr) => {
            let type_ = match type_ {
                Some(t) => *t,
                None => guess_type(lscope, setexpr)?,
            };
            let info = lscope.decl(span.clone(), name.clone(), type_);
            if let Some(etype) = etype {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{:?}", etype),
                    got: "Void (declvar)".into(),
                });
            }
            translate_expr(out, sink, lscope, Some(type_), setexpr)?;
            sink.writeln(format!("local.set {}", info.wasm_name));
        }
        Expr::FunctionCall(span, fname, argexprs) => {
            let ftype = lscope.getf_or_err(span.clone(), fname)?;
            if argexprs.len() != ftype.parameter_types.len() {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{} args", ftype.parameter_types.len()),
                    got: format!("{} args", argexprs.len()),
                });
            }
            for (argexpr, ptype) in argexprs.iter().zip(ftype.parameter_types) {
                translate_expr(out, sink, lscope, Some(ptype), argexpr)?;
            }
            sink.writeln(format!("call $f_{}", fname));
            auto_cast(sink, span, lscope, ftype.return_type, etype)?;
        }
        Expr::If(_span, cond, body, other) => {
            sink.writeln("(if ");
            if let Some(etype) = etype {
                sink.writeln(format!(" (result {})", translate_type(etype)));
            }
            translate_expr(out, sink, lscope, Some(Type::Bool), cond)?;
            sink.writeln("(then");
            translate_expr(out, sink, lscope, etype, body)?;
            sink.writeln(")");
            sink.writeln("(else");
            translate_expr(out, sink, lscope, etype, other)?;
            sink.writeln(")");
            sink.writeln(")");
        }
        Expr::While(_span, cond, body) => {
            let break_label = lscope.new_label_id();
            let continue_label = lscope.new_label_id();
            lscope.break_labels.push(break_label);
            lscope.continue_labels.push(continue_label);

            sink.writeln(format!(
                "(block $lbl_{} (loop $lbl_{}",
                break_label, continue_label
            ));
            translate_expr(out, sink, lscope, Some(Type::Bool), cond)?;
            sink.writeln("i32.eqz");
            sink.writeln(format!("br_if $lbl_{}", break_label));
            translate_expr(out, sink, lscope, None, body)?;
            sink.writeln(format!("br $lbl_{}", continue_label));
            sink.writeln("))");

            lscope.break_labels.pop();
            lscope.continue_labels.pop();
        }
        Expr::LessThan(span, left, right) => {
            op_cmp(out, sink, lscope, etype, span, "lt", left, right)?;
        }
        Expr::Add(span, left, right) => {
            op_arith_binop(out, sink, lscope, etype, span, "add", left, right)?;
        }
        Expr::Binop(span, op, left, right) => match op {
            Binop::Less => op_cmp(out, sink, lscope, etype, span, "lt", left, right)?,
            Binop::LessOrEqual => op_cmp(out, sink, lscope, etype, span, "le", left, right)?,
            Binop::Greater => op_cmp(out, sink, lscope, etype, span, "gt", left, right)?,
            Binop::GreaterOrEqual => op_cmp(out, sink, lscope, etype, span, "ge", left, right)?,
            Binop::Add => op_arith_binop(out, sink, lscope, etype, span, "add", left, right)?,
            Binop::Subtract => op_arith_binop(out, sink, lscope, etype, span, "sub", left, right)?,
            Binop::Multiply => op_arith_binop(out, sink, lscope, etype, span, "mul", left, right)?,
            _ => panic!("TODO: translate_expr binop {:?}", op),
        },
        Expr::Unop(span, op, expr) => {
            match op {
                Unop::Plus | Unop::Minus => {
                    let gtype = guess_type(lscope, expr)?;
                    match gtype {
                        Type::F32 | Type::F64 | Type::I32 | Type::I64 => {
                            translate_expr(out, sink, lscope, Some(gtype), expr)?;
                            match op {
                                Unop::Plus => {}
                                Unop::Minus => {
                                    match gtype {
                                        Type::F32 | Type::F64 => {
                                            sink.writeln(format!("({}.neg ", translate_type(gtype)));
                                        }
                                        Type::I32 | Type::I64 => {
                                            sink.writeln(format!("{}.const -1", translate_type(gtype)));
                                            sink.writeln(format!("{}.mul", translate_type(gtype)));
                                        }
                                        _ => panic!("Unop gtype {:?}", gtype)
                                    }
                                }
                                _ => panic!("Unop {:?}", op),
                            }
                        }
                        _ => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: "numeric".into(),
                                got: format!("{:?}", gtype),
                            })
                        }
                    }
                    auto_cast(sink, span, lscope, Some(gtype), etype)?
                }
                Unop::Not => {
                    translate_expr(out, sink, lscope, Some(Type::Bool), expr)?;
                    sink.writeln("i32.eqz");
                }
            }
        }
        Expr::CString(span, value) => match etype {
            Some(Type::I32) => {
                let ptr = out.intern_cstr(value);
                sink.writeln(format!("i32.const {}", ptr));
            }
            Some(etype) => {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{:?}", etype),
                    got: "i32 (cstr)".into(),
                })
            }
            None => {}
        },
        Expr::Asm(span, args, type_, asm_code) => {
            for arg in args {
                let argtype = guess_type(lscope, arg)?;
                translate_expr(out, sink, lscope, Some(argtype), arg)?;
            }
            sink.writeln(asm_code);
            auto_cast(sink, span, lscope, *type_, etype)?;
        }
    }
    Ok(())
}

/// drops the TOS given that TOS is the provided type
fn drop(_lscope: &mut LocalScope, sink: &Rc<Sink>, type_: Type) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
            sink.writeln("drop");
        }
    }
}

/// util for comparison operators (e.g. LessThan, GreaterThan, etc)
///   * both arguments are always same type
///   * guesses types based on first arg
///   * always returns bool (i32)
///   * signed and unsigend versions for ints (with *_s/_u suffix)
fn op_cmp(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: Option<Type>,
    span: &SSpan,
    opname: &str,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    let gtype = guess_type(lscope, left)?;
    translate_expr(out, sink, lscope, Some(gtype), left)?;
    translate_expr(out, sink, lscope, Some(gtype), right)?;
    match gtype {
        Type::Bool | Type::I32 | Type::I64 => {
            sink.writeln(format!("{}.{}_s", translate_type(gtype), opname));
        }
        Type::F32 | Type::F64 => {
            sink.writeln(format!("{}.{}", translate_type(gtype), opname));
        }
    }
    auto_cast(sink, span, lscope, Some(gtype), etype)?;
    Ok(())
}

/// util for binary arithmetic operators (e.g. Add, Subtract, etc)
///   * both arguments are always same type
///   * guesses types based on first arg
///   * always returns argument type
///   * not split by sign
fn op_arith_binop(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: Option<Type>,
    span: &SSpan,
    opname: &str,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    let gtype = guess_type(lscope, left)?;
    translate_expr(out, sink, lscope, Some(gtype), left)?;
    translate_expr(out, sink, lscope, Some(gtype), right)?;
    match gtype {
        Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
            sink.writeln(format!("{}.{}", translate_type(gtype), opname));
        }
        _ => {
            return Err(Error::Type {
                span: span.clone(),
                expected: "numeric value".into(),
                got: format!("{:?}", gtype),
            });
        }
    }
    auto_cast(sink, span, lscope, Some(gtype), etype)?;
    Ok(())
}

/// perform a cast of TOS from src to dst for when implicitly needed
fn auto_cast(sink: &Rc<Sink>, span: &SSpan, lscope: &mut LocalScope, src: Option<Type>, dst: Option<Type>) -> Result<(), Error> {
    match (src, dst) {
        (Some(src), Some(dst)) if src == dst => {}
        (None, None) => {}
        (Some(src), None) => {
            drop(lscope, sink, src);
        }
        (Some(src), Some(dst)) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: format!("{:?}", dst),
                got: format!("{:?}", src),
            });
        }
        (None, Some(dst)) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: format!("{:?}", dst),
                got: "Void".into(),
            });
        }
    }
    Ok(())
}

/// tries to guess the type of an expression that must return some value
/// returning void will cause an error to be returned
fn guess_type(lscope: &mut LocalScope, expr: &Expr) -> Result<Type, Error> {
    match expr {
        Expr::Bool(..) => Ok(Type::Bool),
        Expr::Int(..) => Ok(Type::I32),
        Expr::Float(..) => Ok(Type::F32),
        Expr::GetVar(span, name) => match lscope.get_or_err(span.clone(), name)? {
            ScopeEntry::Local(info) => Ok(info.type_),
            ScopeEntry::Global(info) => Ok(info.type_),
        },
        Expr::SetVar(span, ..) => Err(Error::Type {
            span: span.clone(),
            expected: "any-value".into(),
            got: "Void (setvar)".into(),
        }),
        Expr::DeclVar(span, ..) => Err(Error::Type {
            span: span.clone(),
            expected: "any-value".into(),
            got: "Void (declvar)".into(),
        }),
        Expr::Block(span, exprs) => match exprs.last() {
            Some(last) => guess_type(lscope, last),
            None => {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: "any-value".into(),
                    got: "Void (empty-block)".into(),
                })
            }
        },
        Expr::FunctionCall(span, name, _) => {
            match lscope.getf_or_err(span.clone(), name)?.return_type {
                Some(t) => Ok(t),
                None => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "any-value".into(),
                        got: "Void (void-returning-function)".into(),
                    })
                }
            }
        }
        Expr::If(_, _, body, _) => guess_type(lscope, body),
        Expr::While(span, ..) => Err(Error::Type {
            span: span.clone(),
            expected: "any-value".into(),
            got: "Void (while)".into(),
        }),
        Expr::LessThan(..) => {
            // Should return a bool
            Ok(Type::I32)
        }
        Expr::Add(_, left, _) => guess_type(lscope, left),
        Expr::Binop(_span, op, left, _) => match op {
            Binop::Add | Binop::Subtract | Binop::Multiply => guess_type(lscope, left),
            Binop::Divide => Ok(Type::F32),
            Binop::TruncDivide | Binop::Remainder => Ok(Type::I32),
            Binop::BitwiseAnd
            | Binop::BitwiseOr
            | Binop::BitwiseXor
            | Binop::ShiftLeft
            | Binop::ShiftRight => Ok(Type::I32),
            Binop::Less
            | Binop::LessOrEqual
            | Binop::Greater
            | Binop::GreaterOrEqual
            | Binop::Equal
            | Binop::NotEqual => Ok(Type::Bool),
        },
        Expr::Unop(_span, op, expr) => match op {
            Unop::Minus | Unop::Plus => guess_type(lscope, expr),
            Unop::Not => Ok(Type::Bool),
        }
        Expr::CString(..) => {
            // Should return a pointer
            Ok(Type::I32)
        }
        Expr::Asm(span, _, type_, _) => match type_ {
            Some(t) => Ok(*t),
            None => {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: "any-value".into(),
                    got: "Void (void-asm-expr)".into(),
                })
            }
        },
    }
}

struct Out {
    main: Rc<Sink>,
    imports: Rc<Sink>,
    memory: Rc<Sink>,
    data: Rc<Sink>,
    gvars: Rc<Sink>,
    funcs: Rc<Sink>,
    start: Rc<Sink>,
    exports: Rc<Sink>,

    data_len: Cell<usize>,
    intern_map: HashMap<Rc<str>, u32>,
}

impl Out {
    fn new() -> Self {
        let main = Sink::new();
        let imports = main.spawn();
        let memory = main.spawn();
        let data = main.spawn();
        let gvars = main.spawn();
        let funcs = main.spawn();
        main.writeln("(func $__rt_start");
        let start = main.spawn();
        main.writeln(")");
        main.writeln("(start $__rt_start)");
        let exports = main.spawn();
        Self {
            main,
            imports,
            memory,
            data,
            gvars,
            funcs,
            start,
            exports,
            data_len: Cell::new(RESERVED_BYTES),
            intern_map: HashMap::new(),
        }
    }

    fn get(self) -> String {
        self.memory.writeln("(memory $rt_mem 1)");
        self.gvars.writeln(format!(
            "(global $rt_heap_start i32 (i32.const {}))",
            self.data_len.get()
        ));
        self.main.get()
    }

    fn data(&self, data: &[u8]) -> u32 {
        // data is reserved with 16-byte alignment
        let reserve_len = (data.len() + 16 - 1) / 16 * 16;
        let ptr = self.data_len.get();
        self.data_len.set(reserve_len + ptr);
        self.data.write(format!("(data (i32.const {}) \"", ptr));
        for byte in data {
            self.data.write(format!("\\{:0>2X}", byte));
        }
        self.data.writeln("\")");
        ptr as u32
    }

    fn intern_cstr(&mut self, s: &Rc<str>) -> u32 {
        if !self.intern_map.contains_key(s) {
            let mut buffer = s.as_bytes().to_vec();
            buffer.push(0);
            let ptr = self.data(&buffer);
            self.intern_map.insert(s.clone(), ptr);
        }
        *self.intern_map.get(s).unwrap()
    }
}
