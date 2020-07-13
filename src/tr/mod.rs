//! logic for translating wac sources to a single wat/webassembly source
use crate::get_enum_value_from_name;
use crate::get_tag_limit;
use crate::ir::*;
use crate::list_all_enum_types_with_members;
use crate::llir::*;
use crate::parse_file;
use crate::set_global_typeinfo;
use crate::Binop;
use crate::Error;
use crate::GlobalTypeInfo;
use crate::Parser;
use crate::SSpan;
use crate::Sink;
use crate::Source;
use crate::TRACE_MODE;
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

pub(crate) use scope::Scope;

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
    let files = parse_files0(sources.clone(), false)?;

    let mut typeinfo = GlobalTypeInfo::new();
    for file in files {
        for en in file.1.enums {
            typeinfo.decl_enum(en.name.clone(), en.members);
        }
        for rec in file.1.records {
            typeinfo.decl_record(rec.name.clone());
        }
    }
    set_global_typeinfo(typeinfo);

    parse_files0(sources, true)
}

fn parse_files0(
    sources: Vec<(Rc<str>, Rc<str>)>,
    strict_about_user_defined_types: bool,
) -> Result<Vec<(Rc<str>, File)>, Error> {
    let mut files = Vec::new();
    for (filename, data) in sources {
        let source = Rc::new(Source {
            name: filename.clone(),
            data: data.clone(),
        });
        let mut parser = match Parser::new(&source, strict_about_user_defined_types) {
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

    // For each record type, store in gscope all the fields it has
    let mut record_fields = HashMap::<Rc<str>, Vec<(Rc<str>, Type)>>::new();
    for (_, file) in &files {
        for record in &file.records {
            record_fields.insert(record.name.clone(), record.fields.clone());
        }
    }
    gscope.record_fields = record_fields;

    // just take the first import declaration we can find
    //
    // [just take the first span in prelude:lang imports, and use that
    // for any builtin thing with no good corresponding location in wac source]
    let void_span = SSpan {
        source: Rc::new(Source {
            name: "[builtin]".into(),
            data: "".into(),
        }),
        span: crate::Span::new(0, 0, 0, 1),
    };

    // prepare the special type constants
    // these could be in the source directly, but it would make it harder to keep
    // both the rust and wac code in sync.
    for type_ in Type::list_builtins() {
        gscope.decl_const(
            void_span.clone(),
            type_.name().into(),
            ConstValue::Type(type_),
        )?;
    }

    // prepare all constants
    for (_filename, file) in &files {
        for c in &file.constants {
            gscope.decl_const(c.span.clone(), c.name.clone(), c.value.clone())?;
        }
        for enum_ in &file.enums {
            let type_ = Type::from_name(&enum_.name).unwrap();
            assert!(type_.is_enum());
            gscope.decl_type(enum_.span.clone(), type_)?;
        }
        for record in &file.records {
            let type_ = Type::from_name(&record.name).unwrap();
            assert!(type_.is_record());
            gscope.decl_type(record.span.clone(), type_)?;
        }
    }

    for (_filename, file) in &files {
        for imp in &file.impls {
            // globally declare this impl
            // this is so that:
            //   * we can catch duplicate definitions
            //   * generate the itable for each type
            //
            // We also have to do this before any expressions are evaluated
            // since, this information is looked up if a trait function is called
            // where the receiver type is known at compile time
            //
            let trait_info = gscope.get_trait(&imp.span, &imp.trait_name)?.clone();
            let fname: Rc<str> = format!("__WAC_{}#{}", imp.receiver_type, &trait_info.name).into();
            gscope.decl_impl(imp.span.clone(), fname, imp.receiver_type, trait_info)?;
        }
    }

    // translate all global variables
    // NOTE: global variables that appear before cannot refer to
    // global variables that appear later
    // NOTE: it kinda sucks that the behavior of the code will depend on the
    // order in which you provide the files
    {
        let mut init_lscope = LocalScope::new(&mut gscope, true);
        for (_filename, file) in &files {
            for gvar in &file.globalvars {
                init_lscope.push();

                let type_ = if let Some(t) = gvar.type_ {
                    t
                } else {
                    guess_type(&mut init_lscope, &gvar.init)?
                };
                let init_sink = out.start.spawn();
                translate_expr(
                    &mut out,
                    &init_sink,
                    &mut init_lscope,
                    ReturnType::Value(type_),
                    &gvar.init,
                )?;
                let info = init_lscope.g.decl_gvar(gvar.span.clone(), gvar.name.clone(), type_)?;
                init_sink.global_set(&info.wasm_name);
                out.gvars.global_mut(info.type_.wasm(), &info.wasm_name, 0);

                init_lscope.pop();
            }
        }
        // declare all helper variables
        for (wasm_name, type_) in init_lscope.helper_locals {
            assert!(type_.primitive());
            out.startlocals.writeln(format!("(local {} {})", wasm_name, translate_type(type_)));
            release_var(&out.start, Scope::Local, &wasm_name, type_);
        }
        // declare all local variables used
        for info in init_lscope.decls {
            out.startlocals.writeln(format!(
                " (local {} {})",
                info.wasm_name,
                translate_type(info.type_)
            ));
            release_var(&out.start, Scope::Local, &info.wasm_name, info.type_);
        }
    }

    // translate the functions
    for (_filename, file) in files {
        for imp in file.imports {
            translate_import(&out, imp);
        }
        for func in file.functions {
            translate_func(&mut out, &mut gscope, func)?;
        }
        for imp in file.impls {
            translate_impl(&mut out, &mut gscope, imp)?;
        }
    }

    // store the string representation of each type
    {
        let ntypes = get_tag_limit();
        let typestr_table_size = (ntypes * 4) as usize;

        // not all parts of this table may be filled in
        // e.g. if there are significantly more enum types
        // than records, or vice versa, there will be 'holes'
        // in the type list
        let mut typestr_table_bytes = Vec::<u8>::new();
        typestr_table_bytes.resize_with(typestr_table_size, || 0);

        for type_ in Type::list() {
            let pos = (type_.tag() * 4) as usize;
            let ptr = out.intern_str(&type_.name()) as WasmPtr;
            typestr_table_bytes[pos..pos + 4].copy_from_slice(&ptr.to_le_bytes());
        }

        let typestr_table_start = out.data(&typestr_table_bytes) as usize;
        let typestr_table_end = typestr_table_start + typestr_table_bytes.len();
        out.gvars.global(
            WasmType::I32,
            "$rt_typestr_table_start",
            typestr_table_start as i64,
        );
        out.gvars.global(
            WasmType::I32,
            "$rt_typestr_table_end",
            typestr_table_end as i64,
        );
    }

    // store enum info
    // each enum info looks like:
    //   [i32 # members][i32 ptr to name of member, ...]
    let enums = list_all_enum_types_with_members();
    let enum_meta_buffer_size = enums.len() * 4 + 4;
    let mut enum_meta_buffer = Vec::<u8>::new();
    enum_meta_buffer.resize_with(enum_meta_buffer_size, || 0);
    for (en, members) in enums {
        let offset = match en {
            Type::Enum(offset) => offset,
            _ => panic!("Not enum? {:?}", en),
        } as usize;
        let mut enum_info_bytes = Vec::<u8>::new();
        enum_info_bytes.extend(&((members.len() * 4) as i32).to_le_bytes());
        for member in members {
            enum_info_bytes.extend(&out.intern_str(&member).to_le_bytes());
        }
        // we store it with a one extra pointer offset because the first
        // 4 bytes are used for the size
        enum_meta_buffer[offset * 4 + 4..offset * 4 + 8]
            .copy_from_slice(&out.data(&enum_info_bytes).to_le_bytes());
    }
    let enum_meta_buffer_start = out.data(&enum_meta_buffer) as usize;
    out.gvars.global(
        WasmType::I32,
        "$rt_enum_meta_buffer_start",
        enum_meta_buffer_start as i64,
    );

    // write out the itables for each type that needs it
    // (this is for dispatching dynamic trait function calls to
    // their matching impls)
    {
        let ntypes = get_tag_limit();

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
            let itable_start: WasmPtr = out.data(&itable_bytes);
            let itable_end = itable_start + (itable_bytes.len() as WasmPtr);
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
        let meta_itable_end = meta_itable_start + (meta_itable_bytes.len() as WasmPtr);

        out.gvars.global(
            WasmType::I32,
            "$rt_meta_itable_start",
            meta_itable_start as i64,
        );
        out.gvars
            .global(WasmType::I32, "$rt_meta_itable_end", meta_itable_end as i64);
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
