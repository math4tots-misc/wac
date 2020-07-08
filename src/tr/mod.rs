//! logic for translating wac sources to a single wat/webassembly source
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
// use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

mod guess;
mod typ;
mod expr;
mod retain;
mod scope;
mod out;
mod func;

use guess::*;
use expr::*;
use typ::*;
use retain::*;
use scope::*;
use out::*;
use func::*;

pub const PAGE_SIZE: usize = 65536;

/// Number of bytes at start of memory that's reserved
/// Compile-time constants stored in memory start from this location
/// Of course, right after RESERVED_BYTES comes the stack.
pub const RESERVED_BYTES: usize = 2048;

/// the location in memory where the stack starts
/// For now, it's the first thing after RESERVED_BYTES
pub const STACK_START: usize = RESERVED_BYTES;

// the max number of times you can recurse
pub const MAX_STACK_DEPTH: usize = 256;

// the number of bytes to be reserved for the stack to support MAX_STACK_DEPTH
// recursions.
// each function call occupies two pointer values,
//   1. ptr to filename str (i32)
//   2. lineno (i32)
pub const STACK_BYTES: usize = MAX_STACK_DEPTH * 8;

pub const STACK_END: usize = STACK_START + STACK_BYTES;

pub fn translate(sources: Vec<(Rc<str>, Rc<str>)>) -> Result<String, Error> {
    let files = parse_files(sources)?;
    translate_files(files)
}

pub fn parse_files(mut sources: Vec<(Rc<str>, Rc<str>)>) -> Result<Vec<(Rc<str>, File)>, Error> {
    let prelude = vec![
        ("[prelude:lang]".into(), crate::prelude::LANG.into()),
        ("[prelude:malloc]".into(), crate::prelude::MALLOC.into()),
        ("[prelude:str]".into(), crate::prelude::STR.into()),
        ("[prelude:id]".into(), crate::prelude::ID.into()),
        ("[prelude:list]".into(), crate::prelude::LIST.into()),
        ("[prelude:type]".into(), crate::prelude::TYPE.into()),
        ("[prelude:assert]".into(), crate::prelude::ASSERT.into()),
        ("[prelude:panic]".into(), crate::prelude::PANIC.into()),
        ("[prelude:stack]".into(), crate::prelude::STACK.into()),
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

    Ok(files)
}

/// translates a list of (filename, wac-code) pairs into
/// a wat webassembly module
pub fn translate_files(files: Vec<(Rc<str>, File)>) -> Result<String, Error> {
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
            functions.insert(func.name.clone(), func.type_.clone());
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
            let mut lscope = LocalScope::new(&gscope, true);
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
