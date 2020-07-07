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
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

mod ops;

pub const PAGE_SIZE: usize = 65536;

/// Number of bytes at start of memory that's reserved
/// Compile-time constants stored in memory start from this location
pub const RESERVED_BYTES: usize = 2048;

/// translates a list of (filename, wac-code) pairs into
/// a wat webassembly module
pub fn translate(mut sources: Vec<(Rc<str>, Rc<str>)>) -> Result<String, Error> {
    let prelude = vec![
        ("[prelude:lang]".into(), crate::prelude::LANG.into()),
        ("[prelude:malloc]".into(), crate::prelude::MALLOC.into()),
        ("[prelude:str]".into(), crate::prelude::STR.into()),
        ("[prelude:id]".into(), crate::prelude::ID.into()),
        ("[prelude:list]".into(), crate::prelude::LIST.into()),
        ("[prelude:type]".into(), crate::prelude::TYPE.into()),
    ];

    sources.splice(0..0, prelude);
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

    // some universal constants
    // we provide these both as 'const' and as wasm globals because
    // from inside wac normally, constants may be preferrable,
    // but from inside asm blocks, it's not possible to use const values
    out.gvars
        .writeln(format!("(global $rt_tag_i32  i32 (i32.const {}))", TAG_I32));
    out.gvars
        .writeln(format!("(global $rt_tag_i64  i32 (i32.const {}))", TAG_I64));
    out.gvars
        .writeln(format!("(global $rt_tag_f32  i32 (i32.const {}))", TAG_F32));
    out.gvars
        .writeln(format!("(global $rt_tag_f64  i32 (i32.const {}))", TAG_F64));
    out.gvars.writeln(format!(
        "(global $rt_tag_bool i32 (i32.const {}))",
        TAG_BOOL
    ));
    out.gvars.writeln(format!(
        "(global $rt_tag_type i32 (i32.const {}))",
        TAG_TYPE
    ));
    out.gvars.writeln(format!(
        "(global $rt_tag_str  i32 (i32.const {}))",
        TAG_STRING
    ));
    out.gvars.writeln(format!(
        "(global $rt_tag_list i32 (i32.const {}))",
        TAG_LIST
    ));
    out.gvars
        .writeln(format!("(global $rt_tag_id   i32 (i32.const {}))", TAG_ID));

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

    // [just take the first span in prelude:lang imports, and use that
    // for any builtin thing with no good corresponding location in wac source]
    let void_span = files[0].1.imports[0].span().clone();

    // prepare the special type constants
    // these could be in the source directly, but it would make it harder to keep
    // both the rust and wac code in sync.
    gscope.decl_const(void_span.clone(), "i32".into(), ConstValue::Type(Type::I32))?;
    gscope.decl_const(void_span.clone(), "i64".into(), ConstValue::Type(Type::I64))?;
    gscope.decl_const(void_span.clone(), "f32".into(), ConstValue::Type(Type::F32))?;
    gscope.decl_const(void_span.clone(), "f64".into(), ConstValue::Type(Type::F64))?;
    gscope.decl_const(
        void_span.clone(),
        "bool".into(),
        ConstValue::Type(Type::Bool),
    )?;
    gscope.decl_const(
        void_span.clone(),
        "type".into(),
        ConstValue::Type(Type::Type),
    )?;
    gscope.decl_const(
        void_span.clone(),
        "str".into(),
        ConstValue::Type(Type::String),
    )?;
    gscope.decl_const(
        void_span.clone(),
        "list".into(),
        ConstValue::Type(Type::List),
    )?;
    gscope.decl_const(void_span.clone(), "id".into(), ConstValue::Type(Type::Id))?;

    // prepare all constants
    for (_filename, file) in &files {
        for c in &file.constants {
            gscope.decl_const(c.span.clone(), c.name.clone(), c.value.clone())?;
        }
    }

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
            translate_expr(
                &mut out,
                &init_sink,
                &mut lscope,
                ReturnType::Value(type_),
                &gvar.init,
            )?;
            let info = gscope.decl_gvar(gvar.span.clone(), gvar.name.clone(), type_)?;
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
    varmap: HashMap<Rc<str>, ScopeEntry>,
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

    fn decl_const(
        &mut self,
        span: SSpan,
        name: Rc<str>,
        cval: ConstValue,
    ) -> Result<Rc<ConstantInfo>, Error> {
        if let Some(info) = self.varmap.get(&name) {
            return Err(Error::ConflictingDefinitions {
                span1: info.span().clone(),
                span2: span,
                name,
            });
        }
        let info = Rc::new(ConstantInfo {
            span,
            name: name.clone(),
            value: cval,
        });
        self.varmap
            .insert(name.clone(), ScopeEntry::Constant(info.clone()));
        Ok(info)
    }

    fn decl_gvar(
        &mut self,
        span: SSpan,
        name: Rc<str>,
        type_: Type,
    ) -> Result<Rc<GlobalVarInfo>, Error> {
        if let Some(info) = self.varmap.get(&name) {
            return Err(Error::ConflictingDefinitions {
                span1: info.span().clone(),
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
        self.varmap
            .insert(name.clone(), ScopeEntry::Global(info.clone()));
        Ok(info)
    }
}

struct ConstantInfo {
    span: SSpan,
    #[allow(dead_code)]
    name: Rc<str>,
    value: ConstValue,
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

    /// local variables not directly created by the end-user
    /// but by the system as needed
    helper_locals: HashMap<Rc<str>, Type>,
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
            helper_locals: HashMap::new(),
        }
    }
    fn helper(&mut self, name: &str, type_: Type) {
        assert!(name.starts_with("$rt_"));
        if let Some(old_type) = self.helper_locals.get(name) {
            assert_eq!(*old_type, type_);
        }
        self.helper_locals.insert(name.into(), type_);
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
            Some(t) => Some(t.clone()),
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

#[derive(Clone)]
enum ScopeEntry {
    Local(Rc<LocalVarInfo>),
    Global(Rc<GlobalVarInfo>),
    Constant(Rc<ConstantInfo>),
}

impl ScopeEntry {
    fn span(&self) -> &SSpan {
        match self {
            ScopeEntry::Local(info) => &info.span,
            ScopeEntry::Global(info) => &info.span,
            ScopeEntry::Constant(info) => &info.span,
        }
    }

    fn type_(&self) -> Type {
        match self {
            ScopeEntry::Local(info) => info.type_,
            ScopeEntry::Global(info) => info.type_,
            ScopeEntry::Constant(info) => info.value.type_(),
        }
    }
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
    match return_type {
        ReturnType::Value(rt) => {
            ret.push_str(&format!(" (result {})", translate_type(*rt)));
        }
        ReturnType::Void | ReturnType::NoReturn => {}
    }
    ret
}

fn translate_type(t: Type) -> &'static str {
    translate_wasm_type(t.wasm())
}

fn translate_wasm_type(wt: WasmType) -> &'static str {
    match wt {
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
    match func.return_type {
        ReturnType::Value(return_type) => {
            sink.writeln(format!(" (result {})", translate_type(return_type)));
        }
        ReturnType::NoReturn | ReturnType::Void => {}
    }
    // we won't know what locals we have until we finish translate_expr on the body
    let locals_sink = sink.spawn();
    let locals_init = sink.spawn();
    translate_expr(out, &sink, &mut lscope, func.return_type, &func.body)?;
    let epilogue = sink.spawn();
    sink.writeln(")");

    // special local variables used by some operations
    // temporary variable for duplicating values on TOS
    let mut helper_locals: Vec<(_, _)> = lscope.helper_locals.into_iter().collect();
    helper_locals.sort_by(|a, b| a.0.cmp(&b.0));
    for (wasm_name, type_) in helper_locals {
        locals_sink.writeln(format!("(local {} {})", wasm_name, translate_type(type_)));
        release_var(&epilogue, Scope::Local, &wasm_name, type_);
    }

    // declare all the local variables (skipping parameters)
    for info in lscope.decls.iter().skip(func.parameters.len()) {
        locals_sink.writeln(format!(
            " (local {} {})",
            info.wasm_name,
            translate_type(info.type_)
        ));
        locals_init.writeln(format!(
            "(local.set {} ({}.const 0))",
            info.wasm_name,
            translate_type(info.type_)
        ));
    }

    // Make sure to release all local variables, even the parameters
    for info in lscope.decls {
        release_var(&epilogue, Scope::Local, &info.wasm_name, info.type_);
    }
    Ok(())
}

fn translate_expr(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    expr: &Expr,
) -> Result<(), Error> {
    match expr {
        Expr::Bool(span, x) => {
            match etype {
                ReturnType::Value(Type::Bool) => {
                    sink.writeln(format!("(i32.const {})", if *x { 1 } else { 0 }));
                }
                ReturnType::Value(Type::Id) => {
                    sink.writeln(format!("(i32.const {})", if *x { 1 } else { 0 }));
                    cast_to_id(sink, TAG_BOOL);
                }
                ReturnType::Value(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "bool".into(),
                    })
                }
                ReturnType::Void => {
                    // no-op value is dropped
                }
                ReturnType::NoReturn => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "noreturn".into(),
                        got: "bool".into(),
                    })
                }
            }
        }
        Expr::Int(span, x) => {
            match etype {
                ReturnType::Value(Type::I32) => {
                    sink.writeln(format!("(i32.const {})", x));
                }
                ReturnType::Value(Type::I64) => {
                    sink.writeln(format!("(i64.const {})", x));
                }
                ReturnType::Value(Type::F32) => {
                    sink.writeln(format!("(f32.const {})", x));
                }
                ReturnType::Value(Type::F64) => {
                    sink.writeln(format!("(f64.const {})", x));
                }
                ReturnType::Value(Type::Id) => {
                    sink.writeln(format!("(i32.const {})", x));
                    cast_to_id(sink, TAG_I32);
                }
                ReturnType::Value(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "int".into(),
                    })
                }
                ReturnType::Void => {
                    // no-op value is dropped
                }
                ReturnType::NoReturn => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "noreturn".into(),
                        got: "int".into(),
                    })
                }
            }
        }
        Expr::Float(span, x) => {
            match etype {
                ReturnType::Value(Type::F32) => {
                    sink.writeln(format!("(f32.const {})", x));
                }
                ReturnType::Value(Type::F64) => {
                    sink.writeln(format!("(f64.const {})", x));
                }
                ReturnType::Value(Type::Id) => {
                    sink.writeln(format!("(f32.const {})", x));
                    sink.writeln("i32.reinterpret_f32");
                    cast_to_id(sink, TAG_F32);
                }
                ReturnType::Value(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "Float".into(),
                    })
                }
                ReturnType::Void => {
                    // no-op value is dropped
                }
                ReturnType::NoReturn => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "noreturn".into(),
                        got: "float".into(),
                    })
                }
            }
        }
        Expr::String(span, value) => {
            match etype {
                ReturnType::Value(t) => {
                    let ptr = out.intern_str(value);
                    sink.writeln(format!("(i32.const {})", ptr));
                    retain(lscope, sink, Type::String, DropPolicy::Keep);
                    auto_cast(
                        sink,
                        span,
                        lscope,
                        ReturnType::Value(Type::String),
                        ReturnType::Value(t),
                    )?;
                }
                ReturnType::Void => {
                    // no-op value is dropped
                }
                ReturnType::NoReturn => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "noreturn".into(),
                        got: "str".into(),
                    })
                }
            }
        }
        Expr::List(span, exprs) => {
            sink.writeln("call $f___new_list");
            for expr in exprs {
                raw_dup(lscope, sink, WasmType::I32);
                translate_expr(out, sink, lscope, ReturnType::Value(Type::Id), expr)?;
                sink.writeln("call $f___list_push_raw_no_retain");
            }
            auto_cast(sink, span, lscope, ReturnType::Value(Type::List), etype)?;
        }
        Expr::Block(span, exprs) => {
            if let Some(last) = exprs.last() {
                lscope.push();

                for expr in &exprs[..exprs.len() - 1] {
                    translate_expr(out, sink, lscope, ReturnType::Void, expr)?;
                }
                translate_expr(out, sink, lscope, etype, last)?;

                lscope.pop();
            } else {
                match etype {
                    ReturnType::Void => {}
                    ReturnType::Value(t) => {
                        return Err(Error::Type {
                            span: span.clone(),
                            expected: format!("{:?}", t),
                            got: "Void (empty-block)".into(),
                        })
                    }
                    ReturnType::NoReturn => {
                        return Err(Error::Type {
                            span: span.clone(),
                            expected: "noreturn".into(),
                            got: "void (empty-block)".into(),
                        })
                    }
                }
            }
        }
        Expr::GetVar(span, name) => {
            let entry = lscope.get_or_err(span.clone(), name)?;
            let gtype = entry.type_();
            match entry {
                ScopeEntry::Local(info) => {
                    sink.writeln(format!("local.get {}", info.wasm_name));
                    retain(lscope, sink, info.type_, DropPolicy::Keep);
                }
                ScopeEntry::Global(info) => {
                    sink.writeln(format!("global.get {}", info.wasm_name));
                    retain(lscope, sink, info.type_, DropPolicy::Keep);
                }
                ScopeEntry::Constant(info) => match &info.value {
                    ConstValue::I32(x) => {
                        sink.writeln(format!("i32.const {}", x));
                    }
                    ConstValue::Type(t) => {
                        sink.writeln(format!("i32.const {}", t.tag()));
                    }
                },
            }
            auto_cast(sink, span, lscope, ReturnType::Value(gtype), etype)?;
        }
        Expr::SetVar(span, name, setexpr) => {
            let entry = lscope.get_or_err(span.clone(), name)?;

            // There's no need to retain here, because anything that's currently
            // on the stack already has a retain on it. By popping from the
            // stack, we're transferring the retain on the stack into the
            // variable itself.
            //
            // We do however have to release the old value.
            match entry {
                ScopeEntry::Local(info) => {
                    translate_expr(out, sink, lscope, ReturnType::Value(info.type_), setexpr)?;
                    release_var(sink, Scope::Local, &info.wasm_name, info.type_);
                    sink.writeln(format!("local.set {}", info.wasm_name));
                }
                ScopeEntry::Global(info) => {
                    translate_expr(out, sink, lscope, ReturnType::Value(info.type_), setexpr)?;
                    release_var(sink, Scope::Global, &info.wasm_name, info.type_);
                    sink.writeln(format!("global.set {}", info.wasm_name));
                }
                ScopeEntry::Constant(_) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "variable (for setvar)".into(),
                        got: "constant".into(),
                    })
                }
            }
            auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
        }
        Expr::DeclVar(span, name, type_, setexpr) => {
            let type_ = match type_ {
                Some(t) => *t,
                None => guess_type(lscope, setexpr)?,
            };
            let info = lscope.decl(span.clone(), name.clone(), type_);
            // There's no need to retain here, because anything that's currently
            // on the stack already have a retain on them. By popping from the
            // stack, we're transferring the retain on the stack into the
            // variable itself.
            translate_expr(out, sink, lscope, ReturnType::Value(type_), setexpr)?;
            sink.writeln(format!("local.set {}", info.wasm_name));
            auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
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
                translate_expr(out, sink, lscope, ReturnType::Value(ptype), argexpr)?;
            }
            sink.writeln(format!("call $f_{}", fname));
            auto_cast(sink, span, lscope, ftype.return_type, etype)?;
        }
        Expr::If(_span, pairs, other) => {
            for (cond, body) in pairs {
                translate_expr(out, sink, lscope, ReturnType::Value(Type::Bool), cond)?;
                sink.writeln("if");
                match etype {
                    ReturnType::Value(etype) => {
                        sink.writeln(format!(" (result {})", translate_type(etype)));
                    }
                    ReturnType::NoReturn | ReturnType::Void => {}
                }
                translate_expr(out, sink, lscope, etype, body)?;
                sink.writeln("else");
            }

            translate_expr(out, sink, lscope, etype, other)?;

            for _ in pairs {
                sink.writeln("end");
            }
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
            translate_expr(out, sink, lscope, ReturnType::Value(Type::Bool), cond)?;
            sink.writeln("i32.eqz");
            sink.writeln(format!("br_if $lbl_{}", break_label));
            translate_expr(out, sink, lscope, ReturnType::Void, body)?;
            sink.writeln(format!("br $lbl_{}", continue_label));
            sink.writeln("))");

            lscope.break_labels.pop();
            lscope.continue_labels.pop();
        }
        Expr::Binop(span, op, left, right) => match op {
            Binop::Less => op_cmp(out, sink, lscope, etype, span, "lt", left, right)?,
            Binop::LessOrEqual => op_cmp(out, sink, lscope, etype, span, "le", left, right)?,
            Binop::Greater => op_cmp(out, sink, lscope, etype, span, "gt", left, right)?,
            Binop::GreaterOrEqual => op_cmp(out, sink, lscope, etype, span, "ge", left, right)?,
            Binop::Is => {
                let left_type = guess_type(lscope, left)?;
                let right_type = guess_type(lscope, right)?;
                let gtype = common_type(lscope, span, left_type, right_type)?;

                // The 'eq' instruction will remove the values from the stack,
                // but they may not actually release the values (if they are supposed
                // to be smart pointers)
                //
                // to ensure it is done properly, we need to explicitly call
                // release() before we actually run the instruction
                translate_expr(out, sink, lscope, ReturnType::Value(gtype), left)?;
                release(lscope, sink, gtype, DropPolicy::Keep);
                translate_expr(out, sink, lscope, ReturnType::Value(gtype), right)?;
                release(lscope, sink, gtype, DropPolicy::Keep);
                sink.writeln(match gtype.wasm() {
                    WasmType::I32 => "i32.eq",
                    WasmType::I64 => "i64.eq",
                    WasmType::F32 => "f32.eq",
                    WasmType::F64 => "f64.eq",
                });
                auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
            }
            Binop::IsNot => {
                let left_type = guess_type(lscope, left)?;
                let right_type = guess_type(lscope, right)?;
                let gtype = common_type(lscope, span, left_type, right_type)?;

                // The 'ne' instruction will remove the values from the stack,
                // but they may not actually release the values (if they are supposed
                // to be smart pointers)
                //
                // to ensure it is done properly, we need to explicitly call
                // release() before we actually run the instruction
                translate_expr(out, sink, lscope, ReturnType::Value(gtype), left)?;
                release(lscope, sink, gtype, DropPolicy::Keep);
                translate_expr(out, sink, lscope, ReturnType::Value(gtype), right)?;
                release(lscope, sink, gtype, DropPolicy::Keep);
                sink.writeln(match gtype.wasm() {
                    WasmType::I32 => "i32.ne",
                    WasmType::I64 => "i64.ne",
                    WasmType::F32 => "f32.ne",
                    WasmType::F64 => "f64.ne",
                });
                auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
            }
            Binop::Add | Binop::Subtract | Binop::Multiply | Binop::Divide | Binop::TruncDivide => handle_binop(
                *op,
                out,
                sink,
                lscope,
                etype,
                span,
                left,
                right,
            )?,
            Binop::BitwiseAnd => {
                op_bitwise_binop(out, sink, lscope, etype, span, "and", left, right)?
            }
            Binop::BitwiseOr => {
                op_bitwise_binop(out, sink, lscope, etype, span, "or", left, right)?
            }
            Binop::BitwiseXor => {
                op_bitwise_binop(out, sink, lscope, etype, span, "xor", left, right)?
            }
            Binop::ShiftLeft => {
                op_bitwise_binop(out, sink, lscope, etype, span, "shl", left, right)?
            }
            Binop::ShiftRight => {
                op_bitwise_binop(out, sink, lscope, etype, span, "shr_u", left, right)?
            }
            _ => panic!("TODO: translate_expr binop {:?}", op),
        },
        Expr::Unop(span, op, expr) => match op {
            Unop::Plus | Unop::Minus => {
                let gtype = guess_type(lscope, expr)?;
                match gtype {
                    Type::F32 | Type::F64 | Type::I32 | Type::I64 => {
                        translate_expr(out, sink, lscope, ReturnType::Value(gtype), expr)?;
                        match op {
                            Unop::Plus => {}
                            Unop::Minus => match gtype {
                                Type::F32 | Type::F64 => {
                                    sink.writeln(format!("({}.neg ", translate_type(gtype)));
                                }
                                Type::I32 | Type::I64 => {
                                    sink.writeln(format!("{}.const -1", translate_type(gtype)));
                                    sink.writeln(format!("{}.mul", translate_type(gtype)));
                                }
                                _ => panic!("Unop gtype {:?}", gtype),
                            },
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
                auto_cast(sink, span, lscope, ReturnType::Value(gtype), etype)?
            }
            Unop::Not => {
                translate_expr(out, sink, lscope, ReturnType::Value(Type::Bool), expr)?;
                sink.writeln("i32.eqz");
            }
        },
        Expr::AssertType(_span, type_, expr) => {
            translate_expr(out, sink, lscope, ReturnType::Value(*type_), expr)?;
        }
        Expr::CString(span, value) => match etype {
            ReturnType::Value(Type::I32) => {
                let ptr = out.intern_cstr(value);
                sink.writeln(format!("i32.const {}", ptr));
            }
            ReturnType::Value(etype) => {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{:?}", etype),
                    got: "i32 (cstr)".into(),
                })
            }
            ReturnType::Void => {}
            ReturnType::NoReturn => {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: "noreturn".into(),
                    got: "i32 (cstr)".into(),
                })
            }
        },
        Expr::Asm(span, args, type_, asm_code) => {
            for arg in args {
                let argtype = guess_type(lscope, arg)?;
                translate_expr(out, sink, lscope, ReturnType::Value(argtype), arg)?;
            }
            sink.writeln(asm_code);
            auto_cast(sink, span, lscope, *type_, etype)?;
        }
    }
    Ok(())
}

/// drops the TOS given that TOS is the provided type
/// the drop parameter determines if the value will be consumed/dropped or not
fn release(lscope: &mut LocalScope, sink: &Rc<Sink>, type_: Type, dp: DropPolicy) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Type => match dp {
            DropPolicy::Drop => sink.writeln("drop"),
            DropPolicy::Keep => {}
        },
        Type::String => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.writeln("call $f___WAC_str_release");
        }
        Type::List => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.writeln("call $f___WAC_list_release");
        }
        Type::Id => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I64),
            }
            sink.writeln("call $f___WAC_id_release");
        }
    }
}

/// releases a reference in a var
/// overall, should leave the stack unchanged
fn release_var(sink: &Rc<Sink>, scope: Scope, wasm_name: &Rc<str>, type_: Type) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Type => {}
        Type::String => {
            sink.writeln(format!("{}.get {}", scope, wasm_name));
            sink.writeln("call $f___WAC_str_release");
        }
        Type::List => {
            sink.writeln(format!("{}.get {}", scope, wasm_name));
            sink.writeln("call $f___WAC_list_release");
        }
        Type::Id => {
            sink.writeln(format!("{}.get {}", scope, wasm_name));
            sink.writeln("call $f___WAC_id_release");
        }
    }
}

enum DropPolicy {
    Drop,
    Keep,
}

enum Scope {
    Local,
    Global,
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scope::Local => write!(f, "local"),
            Scope::Global => write!(f, "global"),
        }
    }
}

/// retains the TOS value given the provided type
/// the drop parameter determines if the value will be consumed/dropped or not
fn retain(lscope: &mut LocalScope, sink: &Rc<Sink>, type_: Type, dp: DropPolicy) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Type => match dp {
            DropPolicy::Drop => sink.writeln("drop"),
            DropPolicy::Keep => {}
        },
        Type::String => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.writeln("call $f___WAC_str_retain");
        }
        Type::List => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.writeln("call $f___WAC_list_retain");
        }
        Type::Id => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I64),
            }
            sink.writeln("call $f___WAC_id_retain");
        }
    }
}

/// Duplicates TOS
/// Requires a WasmType -- this function does not take into account
/// any sort of reference counting
fn raw_dup(lscope: &mut LocalScope, sink: &Rc<Sink>, wasm_type: WasmType) {
    let t = translate_wasm_type(wasm_type);
    let tmpvar = format!("$rt_tmp_dup_{}", t);
    lscope.helper(&tmpvar, wasm_type.wac());
    sink.writeln(format!("local.tee {}", tmpvar));
    sink.writeln(format!("local.get {}", tmpvar));
}

/// Return the most specific shared type between the two types
/// Returns an error if no such type exists
fn common_type(_lscope: &mut LocalScope, span: &SSpan, a: Type, b: Type) -> Result<Type, Error> {
    match (a, b) {
        _ if a == b => Ok(a),
        (Type::I32, Type::F32) | (Type::F32, Type::I32) => Ok(Type::F32),
        _ => Err(Error::Type {
            span: span.clone(),
            expected: format!("{:?}", a),
            got: format!("{:?}", b),
        }),
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
    etype: ReturnType,
    span: &SSpan,
    opname: &str,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    let gtype = guess_type(lscope, left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(gtype), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(gtype), right)?;
    match gtype {
        Type::Bool | Type::I32 | Type::I64 => {
            sink.writeln(format!("{}.{}_s", translate_type(gtype), opname));
        }
        Type::F32 | Type::F64 => {
            sink.writeln(format!("{}.{}", translate_type(gtype), opname));
        }
        Type::Type => panic!("TODO: Type comparisons not yet supported"),
        Type::String => panic!("TODO: String comparisons not yet supported"),
        Type::List => panic!("TODO: List comparisons not yet supported"),
        Type::Id => panic!("TODO: Id comparisons not yet supported"),
    }
    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
    Ok(())
}

fn handle_binop(
    op: Binop,
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    let cases = ops::cases_for_binop(op);
    let ltype = guess_type(lscope, left)?;
    let rtype = guess_type(lscope, right)?;
    for cs in cases {
        if ltype == cs.lhs && rtype == cs.rhs {
            return (cs.handler)(out, sink, lscope, etype, span, left, right);
        }
    }
    Err(Error::Type {
        span: span.clone(),
        expected: format!("{:?} arguments", op),
        got: "no matching implementation for given types".into(),
    })
}

fn guess_binop_type(
    op: Binop,
    lscope: &mut LocalScope,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<Type, Error> {
    let cases = ops::cases_for_binop(op);
    let type_set: HashSet<_> = cases.iter().map(|c| c.type_).collect();
    if type_set.len() == 1 {
        return Ok(cases[0].type_);
    }
    let ltype = guess_type(lscope, left)?;
    let rtype = guess_type(lscope, right)?;
    for cs in cases {
        if ltype == cs.lhs && rtype == cs.rhs {
            return Ok(cs.type_);
        }
    }
    Err(Error::Type {
        span: span.clone(),
        expected: format!("{:?} arguments", op),
        got: "no matching implementation for given types".into(),
    })
}

/// util for binary bitwise operators
///   * both arguments are always i32
///   * always returns i32
fn op_bitwise_binop(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    opname: &str,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), right)?;
    sink.writeln(format!("i32.{}", opname));
    auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
    Ok(())
}

/// adds opcodes to convert an i32 type to an 'id'
fn cast_to_id(sink: &Rc<Sink>, tag: i32) {
    sink.writeln("i64.extend_i32_u");
    sink.writeln(format!("i64.const {}", tag));
    sink.writeln("i64.const 32");
    sink.writeln("i64.shl");
    sink.writeln("i64.or");
}

/// perform a cast of TOS from src to dst for when implicitly needed
fn auto_cast(
    sink: &Rc<Sink>,
    span: &SSpan,
    lscope: &mut LocalScope,
    src: ReturnType,
    dst: ReturnType,
) -> Result<(), Error> {
    match (src, dst) {
        (ReturnType::Value(src), ReturnType::Value(dst)) if src == dst => {}
        (ReturnType::Void, ReturnType::Void) => {}
        (ReturnType::NoReturn, ReturnType::NoReturn) => {}
        (ReturnType::NoReturn, _) => {
            // if we're ever provided with a noreturn,
            // there's nothing really to do except let wasm know
            sink.writeln("unreachable");
        }
        (ReturnType::Value(Type::I32), ReturnType::Value(Type::F32)) => {
            sink.writeln("f32.convert_i32_s");
        }
        (ReturnType::Value(Type::I32), ReturnType::Value(Type::Id)) => {
            cast_to_id(sink, TAG_I32);
        }
        (ReturnType::Value(Type::F32), ReturnType::Value(Type::Id)) => {
            sink.writeln("i32.reinterpret_f32");
            cast_to_id(sink, TAG_F32);
        }
        (ReturnType::Value(Type::Bool), ReturnType::Value(Type::Id)) => {
            cast_to_id(sink, TAG_BOOL);
        }
        (ReturnType::Value(Type::String), ReturnType::Value(Type::Id)) => {
            cast_to_id(sink, TAG_STRING);
        }
        (ReturnType::Value(Type::List), ReturnType::Value(Type::Id)) => {
            cast_to_id(sink, TAG_LIST);
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::I32)) => {
            sink.writeln("call $f___WAC_raw_id_to_i32");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::F32)) => {
            sink.writeln("call $f___WAC_raw_id_to_f32");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::Bool)) => {
            sink.writeln("call $f___WAC_raw_id_to_bool");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::String)) => {
            sink.writeln("call $f___WAC_raw_id_to_str");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::List)) => {
            sink.writeln("call $f___WAC_raw_id_to_list");
        }
        (ReturnType::Value(src), ReturnType::Void) => {
            release(lscope, sink, src, DropPolicy::Drop);
        }
        (ReturnType::Value(src), ReturnType::Value(dst)) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: format!("{:?}", dst),
                got: format!("{:?}", src),
            });
        }
        (ReturnType::Value(src), ReturnType::NoReturn) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: "noreturn".into(),
                got: format!("{:?}", src),
            });
        }
        (ReturnType::Void, ReturnType::NoReturn) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: "noreturn".into(),
                got: "void".into(),
            });
        }
        (ReturnType::Void, ReturnType::Value(dst)) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: format!("{:?}", dst),
                got: "void".into(),
            });
        }
    }
    Ok(())
}

/// perform a cast of TOS from src to dst for when explicitly requested
/// "stronger" than auto_cast
fn explicit_cast(
    sink: &Rc<Sink>,
    span: &SSpan,
    lscope: &mut LocalScope,
    src: ReturnType,
    dst: ReturnType,
) -> Result<(), Error> {
    match (src, dst) {
        (ReturnType::Value(Type::F32), ReturnType::Value(Type::I32)) => {
            sink.writeln("i32.trunc_f32_s");
        }
        _ => auto_cast(sink, span, lscope, src, dst)?,
    }
    Ok(())
}

/// tries to guess the type of an expression that must return some value
/// returning void will cause an error to be returned
fn guess_type(lscope: &mut LocalScope, expr: &Expr) -> Result<Type, Error> {
    match guess_return_type(lscope, expr)? {
        ReturnType::Value(t) => Ok(t),
        ReturnType::Void => Err(Error::Type {
            span: expr.span().clone(),
            expected: "assignable type".into(),
            got: "void (variables cannot be void)".into(),
        }),
        ReturnType::NoReturn => {
            // I'm not sure if returning an error is actually the correct thing
            // to do here
            Err(Error::Type {
                span: expr.span().clone(),
                expected: "assignable type".into(),
                got: "noreturn (variables cannot be noreturn)".into(),
            })
        }
    }
}

// return the best fitting type that fits the union of the two return types
fn best_union_return_type(a: ReturnType, b: ReturnType) -> ReturnType {
    match (a, b) {
        // If we see Void anywhere, the overall return must be void
        (ReturnType::Void, _) | (_, ReturnType::Void) => ReturnType::Void,

        // NoReturn is a recessive, if we see one, always return the other type
        (ReturnType::NoReturn, _) => b,
        (_, ReturnType::NoReturn) => a,

        (ReturnType::Value(a), ReturnType::Value(b)) => ReturnType::Value(match (a, b) {
            _ if a == b => a,

            // special case for int/floats -- ints can be used as floats
            // if needed
            (Type::I32, Type::F32) | (Type::F32, Type::I32) => Type::F32,

            // in all other cases, just use the id type
            _ => Type::Id,
        }),
    }
}

/// tries to guess the type of an expression that must return some value
/// returning void will cause an error to be returned
fn guess_return_type(lscope: &mut LocalScope, expr: &Expr) -> Result<ReturnType, Error> {
    match expr {
        Expr::Bool(..) => Ok(ReturnType::Value(Type::Bool)),
        Expr::Int(..) => Ok(ReturnType::Value(Type::I32)),
        Expr::Float(..) => Ok(ReturnType::Value(Type::F32)),
        Expr::String(..) => Ok(ReturnType::Value(Type::String)),
        Expr::List(..) => Ok(ReturnType::Value(Type::List)),
        Expr::GetVar(span, name) => match lscope.get_or_err(span.clone(), name)? {
            ScopeEntry::Local(info) => Ok(ReturnType::Value(info.type_)),
            ScopeEntry::Global(info) => Ok(ReturnType::Value(info.type_)),
            ScopeEntry::Constant(info) => Ok(ReturnType::Value(info.value.type_())),
        },
        Expr::SetVar(..) => Ok(ReturnType::Void),
        Expr::DeclVar(..) => Ok(ReturnType::Void),
        Expr::Block(_, exprs) => match exprs.last() {
            Some(last) => {
                // TODO: check the body for the noreturn type
                guess_return_type(lscope, last)
            }
            None => Ok(ReturnType::Void),
        },
        Expr::FunctionCall(span, name, _) => {
            Ok(lscope.getf_or_err(span.clone(), name)?.return_type)
        }
        Expr::If(_, pairs, other) => {
            let mut ret = ReturnType::NoReturn;
            for (_, body) in pairs {
                ret = best_union_return_type(ret, guess_return_type(lscope, body)?);
            }
            ret = best_union_return_type(ret, guess_return_type(lscope, other)?);
            Ok(ret)
        }
        Expr::While(..) => Ok(ReturnType::Void),
        Expr::Binop(span, op, left, right) => match op {
            Binop::Add => {
                Ok(ReturnType::Value(guess_binop_type(*op, lscope, span, left, right)?))
            }
            Binop::Subtract | Binop::Multiply => {
                let a = guess_type(lscope, left)?;
                let b = guess_type(lscope, right)?;
                Ok(ReturnType::Value(common_type(lscope, span, a, b)?))
            }
            Binop::Divide => Ok(ReturnType::Value(Type::F32)),
            Binop::TruncDivide | Binop::Remainder => Ok(ReturnType::Value(Type::I32)),
            Binop::BitwiseAnd
            | Binop::BitwiseOr
            | Binop::BitwiseXor
            | Binop::ShiftLeft
            | Binop::ShiftRight => Ok(ReturnType::Value(Type::I32)),
            Binop::Less
            | Binop::LessOrEqual
            | Binop::Greater
            | Binop::GreaterOrEqual
            | Binop::Equal
            | Binop::NotEqual
            | Binop::Is
            | Binop::IsNot => Ok(ReturnType::Value(Type::Bool)),
        },
        Expr::Unop(_span, op, expr) => match op {
            Unop::Minus | Unop::Plus => Ok(ReturnType::Value(guess_type(lscope, expr)?)),
            Unop::Not => Ok(ReturnType::Value(Type::Bool)),
        },
        Expr::AssertType(_, type_, _) => Ok(ReturnType::Value(*type_)),
        Expr::CString(..) => {
            // Should return a pointer
            Ok(ReturnType::Value(Type::I32))
        }
        Expr::Asm(_, _, type_, _) => Ok(type_.clone()),
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
    intern_cstr_map: HashMap<Rc<str>, u32>,
    intern_str_map: HashMap<Rc<str>, u32>,
}

impl Out {
    fn new() -> Self {
        let main = Sink::new();
        let imports = main.spawn();
        let memory = main.spawn();
        let data = main.spawn();
        let gvars = main.spawn();
        let funcs = main.spawn();
        main.write(crate::wfs::CODE);
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
            intern_cstr_map: HashMap::new(),
            intern_str_map: HashMap::new(),
        }
    }

    fn get(self) -> String {
        let len = self.data_len.get();
        let page_len = (len + (PAGE_SIZE - 1)) / PAGE_SIZE;
        self.memory
            .writeln(format!("(memory $rt_mem {})", page_len));
        self.gvars
            .writeln(format!("(global $rt_heap_start i32 (i32.const {}))", len,));
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
        if !self.intern_cstr_map.contains_key(s) {
            let mut buffer = s.as_bytes().to_vec();
            buffer.push(0);
            let ptr = self.data(&buffer);
            self.intern_cstr_map.insert(s.clone(), ptr);
        }
        *self.intern_cstr_map.get(s).unwrap()
    }

    fn intern_str(&mut self, s: &Rc<str>) -> u32 {
        if !self.intern_str_map.contains_key(s) {
            let mut buffer = Vec::<u8>::new();
            // refcnt
            buffer.extend(&1i32.to_le_bytes());
            // len
            buffer.extend(&(s.len() as i32).to_le_bytes());
            // utf8
            buffer.extend(s.as_bytes().to_vec());
            let ptr = self.data(&buffer);
            self.intern_str_map.insert(s.clone(), ptr);
        }
        *self.intern_str_map.get(s).unwrap()
    }
}
