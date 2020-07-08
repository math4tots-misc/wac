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

mod expr;
mod fcall;
mod func;
mod guess;
mod out;
mod retain;
mod scope;
mod typ;

use expr::*;
use fcall::*;
use func::*;
use guess::*;
use out::*;
use retain::*;
use scope::*;
use typ::*;

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

pub fn parse_files(sources: Vec<(Rc<str>, Rc<str>)>) -> Result<Vec<(Rc<str>, File)>, Error> {
    // This is a terrible hack
    // we parse the the input twice,
    // first to get a list of all user defined types and what they are (i.e.
    // enum or record?)
    // then we do the parse a second time, this time knowing ahead of time
    // what all the user defined types are
    let files = parse_files0(sources.clone(), None)?;

    let mut user_type_map = HashMap::new();
    for file in files {
        for en in file.1.enums {
            let type_ = Type::Enum(en.type_offset);
            user_type_map.insert(en.name, type_);
        }
        for rec in file.1.records {
            let type_ = Type::Record(rec.type_offset);
            user_type_map.insert(rec.name, type_);
        }
    }

    parse_files0(sources, Some(user_type_map))
}

fn parse_files0(
    mut sources: Vec<(Rc<str>, Rc<str>)>,
    user_type_map: Option<HashMap<Rc<str>, Type>>,
) -> Result<Vec<(Rc<str>, File)>, Error> {
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
        ("[prelude:ops]".into(), crate::prelude::OPS.into()),
        ("[prelude:trait]".into(), crate::prelude::TRAIT.into()),
    ];

    sources.splice(0..0, prelude);
    let mut files = Vec::new();
    for (filename, data) in sources {
        let source = Rc::new(Source {
            name: filename.clone(),
            data: data.clone(),
        });
        let mut parser = match Parser::new(&source, &user_type_map) {
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

    let mut functions = HashMap::<Rc<str>, FunctionEntry>::new();
    let mut traits_by_id = Vec::<Rc<TraitInfo>>::new();

    // collect all function signatures
    for (_filename, file) in &files {
        for imp in &file.imports {
            match imp {
                Import::Function(FunctionImport { alias, type_, .. }) => {
                    let entry = FunctionEntry::Function(Rc::new(FunctionInfo {
                        name: alias.clone(),
                        type_: type_.clone(),
                    }));
                    functions.insert(alias.clone(), entry);
                }
            }
        }
        for func in &file.functions {
            let entry = FunctionEntry::Function(Rc::new(FunctionInfo {
                name: func.name.clone(),
                type_: func.type_.clone(),
            }));
            functions.insert(func.name.clone(), entry);
        }
        for trait_ in &file.traits {
            let id = traits_by_id.len() as i32;
            let info = Rc::new(TraitInfo {
                id,
                type_: trait_.type_.clone(),
                name: trait_.name.clone(),
            });
            traits_by_id.push(info.clone());
            let entry = FunctionEntry::Trait(info);
            functions.insert(trait_.name.clone(), entry);
        }
    }
    let mut gscope = GlobalScope::new(functions, traits_by_id);

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
        for imp in file.impls {
            translate_impl(&mut out, &mut gscope, imp)?;
        }
    }

    // write out the itables for each type
    {
        // for now, let's only assume builtin types
        // in the future though, this will need to be adjusted
        // to account for user defined types
        let ntypes = TAG_ID + 1;

        // the meta itable maps each type tag to their itables' start and end ptrs
        let meta_itable_size = (ntypes * 8) as usize;
        let mut meta_itable_bytes = Vec::<u8>::new();
        meta_itable_bytes.resize_with(meta_itable_size, || 0);

        let mut impls_vec: Vec<(Type, HashMap<i32, Rc<ImplInfo>>)> =
            gscope.impls_map.clone().into_iter().collect();
        impls_vec.sort_by(|a, b| a.0.tag().cmp(&b.0.tag()));

        for (type_, map) in impls_vec {
            // For each type, we store an array of pairs
            //   (trait_id, impl_index)
            // print!("type {}, {}: ", type_, type_.tag());

            let mut impl_vec: Vec<(i32, Rc<ImplInfo>)> = map.into_iter().collect();
            impl_vec.sort_by(|a, b| a.0.cmp(&b.0));

            let mut itable_bytes = Vec::<u8>::new();
            for (trait_id, impl_info) in impl_vec {
                // print!("({}[{}], {}) ", trait_id, impl_info.trait_.name, impl_info.index);
                itable_bytes.extend(&trait_id.to_le_bytes());
                itable_bytes.extend(&impl_info.index.to_le_bytes());
            }
            let itable_start: u32 = out.data(&itable_bytes);
            let itable_end = itable_start + (itable_bytes.len() as u32);
            let tag = type_.tag() as usize;

            // println!("[itable: {}, {}]", itable_start, itable_end);

            // add the entry in the meta itable that points to this itable
            // the itable start position
            meta_itable_bytes[tag * 8..tag * 8 + 4].copy_from_slice(&itable_start.to_le_bytes());
            // the itable end position
            meta_itable_bytes[tag * 8 + 4..tag * 8 + 8].copy_from_slice(&itable_end.to_le_bytes());
        }

        // Finally! the meta-itable is built
        let meta_itable_start = out.data(&meta_itable_bytes);
        let meta_itable_end = meta_itable_start + (meta_itable_bytes.len() as u32);

        out.gvars.writeln(format!(
            "(global $rt_meta_itable_start i32 (i32.const {}))",
            meta_itable_start
        ));
        out.gvars.writeln(format!(
            "(global $rt_meta_itable_end   i32 (i32.const {}))",
            meta_itable_end
        ));
    }

    // initialize the wasm table with all the impls so that they
    // can be called with call_indirect
    out.table.writeln("(table $rt_table anyfunc (elem");
    for (i, info) in gscope.impls.iter().enumerate() {
        assert_eq!(i, info.index as usize);
        out.table.writeln(format!("$f_{}", info.fname));
    }
    out.table.writeln("))");
    Ok(out.get())
}
