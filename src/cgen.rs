use crate::ir::*;
use crate::ByteCount;
use crate::Error;
use std::fmt::Write;

impl Program {
    /// Translate the given program into webassembly
    pub fn wat(self) -> Result<String, Error> {
        gen(self)
    }
}

/// translate a program into webassembly text
fn gen(program: Program) -> Result<String, Error> {
    let mut out = String::new();

    for ext in &program.externs {
        gen_extern(&mut out, ext)?;
    }

    let static_mem_end = program.memory.borrow().get_mem_end();
    let start_page_cnt = std::cmp::max(1, (static_mem_end + PAGE_SIZE - 1) / PAGE_SIZE);

    writeln!(out, "(memory $memory {})", start_page_cnt)?;

    // record where the current heap limit is
    gen_data(&mut out, HEAP_LIMIT_PTR, &static_mem_end.to_le_bytes())?;

    // write out the rest of the data
    let (start_pos, data) = program.memory.borrow().gen();
    gen_data(&mut out, start_pos, &data)?;

    writeln!(out, "(global $rt/static_mem_end i32 (i32.const {}))", static_mem_end)?;
    for gvar in &program.globals {
        gen_global(&mut out, gvar)?;
    }

    for func in &program.funcs {
        gen_func(&mut out, func)?;
    }

    gen_start(&mut out, &program)?;

    writeln!(out, "(start $start)")?;
    writeln!(out, r#"(export "Main" (func $f/Main))"#)?;

    Ok(out)
}

fn gen_data(out: &mut String, start_pos: usize, data: &[u8]) -> Result<(), Error> {
    write!(out, "(data (i32.const {}) \"", start_pos)?;
    for byte in data {
        write!(out, "\\{:02x}", byte)?;
    }
    writeln!(out, "\")")?;
    Ok(())
}

fn gen_global(out: &mut String, gvar: &Global) -> Result<(), Error> {
    // declare global variables
    // they are not actually initialized until 'gen_start'
    writeln!(
        out,
        "(global $g/{} (mut {}) {})",
        gvar.name,
        trtype(&gvar.type_),
        trzeroval(&gvar.type_)
    )?;
    Ok(())
}

/// given a type gives the 'zero value expression' for the associated type
/// this is primarily for initializing variables
fn trzeroval(typ: &Type) -> &str {
    match typ {
        Type::F32 => "(f32.const 0)",
        Type::F64 => "(f64.const 0)",
        Type::I64 => "(i64.const 0)",
        Type::I32 | Type::Bool | Type::Str | Type::Record(_) => "(i32.const 0)",
        Type::Id => "(i64.const 0)",
    }
}

fn gen_start(out: &mut String, program: &Program) -> Result<(), Error> {
    // Initialize global variables
    out.push_str("(func $start\n");
    for local in &program.gvar_init_locals {
        out.push_str(&format!(
            "(local $l/{}/{} {})\n",
            local.id,
            local.name,
            trtype(&local.type_)
        ));
    }
    for gvar in &program.globals {
        gen_expr(out, &gvar.init)?;
        out.push_str(&format!("global.set $g/{}\n", gvar.name));
    }
    // TOOD: release all local variables from gvar_init_locals here
    out.push_str(")\n");
    Ok(())
}

fn gen_extern(out: &mut String, ext: &Extern) -> Result<(), Error> {
    out.push_str(&format!(
        "(import \"{}\" \"{}\" (func $f/{}",
        ext.path.0, ext.path.1, ext.name
    ));
    for param in &ext.type_.parameters {
        out.push_str(&format!(" (param {})", trtype(&param.1)));
    }
    out.push_str(&trrtype(&ext.type_.return_type));
    out.push_str("))\n");
    Ok(())
}

/// translate type
fn trtype(type_: &Type) -> &'static str {
    match type_ {
        Type::Bool => "i32",
        Type::I32 => "i32",
        Type::I64 => "i64",
        Type::F32 => "f32",
        Type::F64 => "f64",
        Type::Str => "i32",
        Type::Record(_) => "i32",
        Type::Id => "i64",
    }
}

/// translate return type
fn trrtype(type_: &ReturnType) -> String {
    match type_ {
        ReturnType::Type(t) => format!(" (result {})", trtype(t)),
        _ => "".into(),
    }
}

fn gen_func(out: &mut String, func: &Func) -> Result<(), Error> {
    out.push_str(&format!("(func $f/{}", func.name));
    for param in func.parameters.borrow().iter() {
        out.push_str(&format!(
            " (param $l/{}/{} {})",
            param.id,
            param.name,
            trtype(&param.type_)
        ));
    }
    out.push_str(&trrtype(&func.type_.return_type));
    out.push_str("\n");

    // declare the local variables, skipping parameters
    for local in func
        .locals
        .borrow()
        .iter()
        .skip(func.parameters.borrow().len())
    {
        out.push_str(&format!(
            "(local $l/{}/{} {})\n",
            local.id,
            local.name,
            trtype(&local.type_)
        ));
    }

    out.push_str("(block $ret");
    out.push_str(&trrtype(&func.type_.return_type));
    out.push_str("\n");

    gen_stmt(out, func.body.borrow().as_ref().unwrap())?;

    out.push_str(")\n");
    // release all local variables here (including parameters)
    for local in func.locals.borrow().iter() {
        release_var(out, &Variable::Local(local.clone()))?;
    }
    out.push_str(")\n");
    Ok(())
}

fn gen_stmt(out: &mut String, stmt: &Stmt) -> Result<(), Error> {
    match &stmt.data {
        StmtData::Block(stmts) => {
            for stmt in stmts {
                gen_stmt(out, stmt)?;
            }
        }
        StmtData::Return(expr) => {
            gen_expr(out, expr)?;
            out.push_str("br $ret\n");
        }
        StmtData::Expr(expr) => {
            gen_expr(out, expr)?;
            assert_eq!(expr.type_, ReturnType::Void);
        }
    }
    Ok(())
}

fn gen_expr(out: &mut String, expr: &Expr) -> Result<(), Error> {
    match &expr.data {
        ExprData::Void => {}
        ExprData::Bool(b) => writeln!(out, "i32.const {}", if *b { 1 } else { 0 })?,
        ExprData::I32(x) => writeln!(out, "i32.const {}", x)?,
        ExprData::I64(x) => writeln!(out, "i64.const {}", x)?,
        ExprData::F32(x) => writeln!(out, "f32.const {}", x)?,
        ExprData::F64(x) => writeln!(out, "f64.const {}", x)?,
        ExprData::Str(ptr) => {
            writeln!(out, "i32.const {}", ptr.get())?;
            writeln!(out, "call $f/__retain")?;
            writeln!(out, "i32.const {}", ptr.get())?;
        }
        ExprData::GetVar(x) => match x.type_() {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                writeln!(out, "{}.get {}", x.wasm_kind(), x.wasm_name())?;
            }
            Type::Str | Type::Record(_) => {
                writeln!(out, "{}.get {}", x.wasm_kind(), x.wasm_name())?;
                writeln!(out, "call $f/__retain")?;
                writeln!(out, "{}.get {}", x.wasm_kind(), x.wasm_name())?;
            }
            Type::Id => panic!("TODO: gen_expr id GetVar (retain)"),
        },
        ExprData::SetVar(x, expr) => match x.type_() {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                gen_expr(out, expr)?;
                writeln!(out, "{}.set {}", x.wasm_kind(), x.wasm_name())?;
            }
            Type::Str | Type::Record(_) => {
                // save the old value on the stack (for release later)
                writeln!(out, "{}.get {}", x.wasm_kind(), x.wasm_name())?;

                gen_expr(out, expr)?;
                writeln!(out, "{}.tee {}", x.wasm_kind(), x.wasm_name())?;

                // retain the new value
                writeln!(out, "call $f/__retain")?;

                // release the old value
                writeln!(out, "call $f/__release")?;
            }
            Type::Id => panic!("TODO: gen_expr id SetVar (retain + release)"),
        },
        ExprData::AugVar(x, op, expr) => match x.type_() {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                out.push_str(&format!("{}.get {}\n", x.wasm_kind(), x.wasm_name()));
                gen_expr(out, expr)?;
                out.push_str(&format!("{}\n", op));
                out.push_str(&format!("{}.set {}\n", x.wasm_kind(), x.wasm_name()));
            }
            Type::Str | Type::Record(_) => {
                panic!("TODO: gen_expr record AugLocal (retain + release)")
            }
            Type::Id => panic!("TODO: gen_expr id AugVar (retain + release)"),
        },
        ExprData::CallFunc(func, args) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            writeln!(out, "call $f/{}", func.name)?;
        }
        ExprData::CallExtern(ext, args) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            writeln!(out, "call $f/{}", ext.name)?;
        }
        ExprData::Op(op, args) => {
            for arg in args {
                gen_expr(out, arg)?;

                // these are primitive wasm operations,
                // so if any of the arguments here are non-primitive,
                // we should take care to drop them
                // this should mostly never happen, except for the
                // Is/IsNot operands
                release_tos(out, arg.type_.value().unwrap(), DropPolicy::Keep)?;
            }
            writeln!(out, "{}", op)?;
        }
        ExprData::DropPrimitive(x) => {
            gen_expr(out, x)?;
            writeln!(out, "drop")?;
        }
        ExprData::Asm(args, _, code) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            writeln!(out, "{}", code)?;
        }
        ExprData::Raw(var) => {
            writeln!(out, "{}.get {}", var.wasm_kind(), var.wasm_name())?;
            match var.type_() {
                Type::F32 => writeln!(out, "i32.reinterpret_f32")?,
                Type::F64 => writeln!(out, "i64.reinterpret_f64")?,
                _ => {}
            }
        }
        ExprData::Read(byte_count, addr, offset) => {
            gen_expr(out, addr)?;
            match byte_count {
                ByteCount::N1 => write!(out, "i32.load_u")?,
                ByteCount::N2 => write!(out, "i32.load16_u")?,
                ByteCount::N4 => write!(out, "i32.load")?,
                ByteCount::N8 => write!(out, "i64.load")?,
            }
            if *offset != 0 {
                write!(out, " offset={}", offset)?;
            }
            writeln!(out, "")?;
        }
        ExprData::Write(byte_count, addr, data, offset) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            match byte_count {
                ByteCount::N1 => write!(out, "i32.store8")?,
                ByteCount::N2 => write!(out, "i32.store16")?,
                ByteCount::N4 => write!(out, "i32.store")?,
                ByteCount::N8 => write!(out, "i64.store")?,
            }
            if *offset != 0 {
                write!(out, " offset={}", offset)?;
            }
            writeln!(out, "")?;
        }
    }
    Ok(())
}

fn release_var(out: &mut String, var: &Variable) -> Result<(), Error> {
    let type_ = var.type_();
    match type_.retain_type() {
        RetainType::Primitive => {}
        RetainType::Typed => {
            writeln!(out, "{}.get {}", var.wasm_kind(), var.wasm_name())?;
            writeln!(out, "call $f/__release")?;
        }
        RetainType::Id => panic!("TODO: release_var id"),
    }
    Ok(())
}

enum DropPolicy {
    Keep,
    #[allow(dead_code)]
    Drop,
}

fn release_tos(out: &mut String, type_: &Type, drop_policy: DropPolicy) -> Result<(), Error> {
    match type_.retain_type() {
        RetainType::Primitive => match drop_policy {
            DropPolicy::Keep => {}
            DropPolicy::Drop => {
                writeln!(out, "drop")?;
            }
        },
        RetainType::Typed => {
            match drop_policy {
                DropPolicy::Keep => {
                    writeln!(out, "local.tee $helper/i32")?;
                    writeln!(out, "local.get $helper/i32")?;
                }
                DropPolicy::Drop => {}
            }
            writeln!(out, "call $f/__release")?;
        }
        RetainType::Id => panic!("TODO: release_tos id"),
    }
    Ok(())
}
