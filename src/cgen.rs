use crate::ir::*;
use crate::Error;

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

    out.push_str("(memory $memory 1)\n");

    for gvar in &program.globals {
        gen_global(&mut out, gvar)?;
    }

    for func in &program.funcs {
        gen_func(&mut out, func)?;
    }

    gen_start(&mut out, &program)?;

    out.push_str("(start $start)\n");
    out.push_str(r#"(export "Main" (func $f/Main))"#);
    out.push_str("\n");

    Ok(out)
}

fn gen_global(out: &mut String, gvar: &Global) -> Result<(), Error> {
    // declare global variables
    // they are not actually initialized until 'gen_start'
    out.push_str(&format!(
        "(global $g/{} (mut {}) {})\n",
        gvar.name,
        trtype(&gvar.type_),
        trzeroval(&gvar.type_)
    ));
    Ok(())
}

/// given a type gives the 'zero value expression' for the associated type
/// this is primarily for initializing variables
fn trzeroval(typ: &Type) -> &str {
    match typ {
        Type::F32 => "(f32.const 0)",
        Type::F64 => "(f64.const 0)",
        Type::I64 => "(i64.const 0)",
        Type::I32 | Type::Bool | Type::Record(_) => "(i32.const 0)",
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
        Type::Record(_) => "i32",
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
    // TODO: release all local variables here (including parameters)
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
        ExprData::Bool(b) => out.push_str(&format!("i32.const {}\n", if *b { 1 } else { 0 })),
        ExprData::I32(x) => out.push_str(&format!("i32.const {}\n", x)),
        ExprData::I64(x) => out.push_str(&format!("i64.const {}\n", x)),
        ExprData::F32(x) => out.push_str(&format!("f32.const {}\n", x)),
        ExprData::F64(x) => out.push_str(&format!("f64.const {}\n", x)),
        ExprData::GetLocal(x) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                out.push_str(&format!("local.get $l/{}/{}\n", x.id, x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record GetLocal (retain)"),
        },
        ExprData::SetLocal(x, expr) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                gen_expr(out, expr)?;
                out.push_str(&format!("local.set $l/{}/{}\n", x.id, x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record SetLocal (retain + release)"),
        },
        ExprData::AugLocal(x, op, expr) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                out.push_str(&format!("local.get $l/{}/{}\n", x.id, x.name));
                gen_expr(out, expr)?;
                out.push_str(&format!("{}\n", op));
                out.push_str(&format!("local.set $l/{}/{}\n", x.id, x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record AugLocal (retain + release)"),
        },
        ExprData::GetGlobal(x) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                out.push_str(&format!("global.get $g/{}\n", x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record GetGlobal (retain)"),
        },
        ExprData::SetGlobal(x, expr) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                gen_expr(out, expr)?;
                out.push_str(&format!("global.set $g/{}\n", x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record SetGlobal (retain + release)"),
        },
        ExprData::AugGlobal(x, op, expr) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                out.push_str(&format!("global.get $g/{}\n", x.name));
                gen_expr(out, expr)?;
                out.push_str(&format!("{}\n", op));
                out.push_str(&format!("global.set $g/{}\n", x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record AugGlobal (retain + release)"),
        },
        ExprData::CallFunc(func, args) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            out.push_str(&format!("call $f/{}\n", func.name));
        }
        ExprData::CallExtern(ext, args) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            out.push_str(&format!("call $f/{}\n", ext.name));
        }
        ExprData::Op(op, args) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            out.push_str(&format!("{}\n", op));
        }
        ExprData::DropPrimitive(x) => {
            gen_expr(out, x)?;
            out.push_str("drop\n");
        }
        ExprData::Asm(args, _, code) => {
            for arg in args {
                gen_expr(out, arg)?;
            }
            out.push_str(code);
            out.push('\n');
        }
        ExprData::Read1(addr, offset) => {
            gen_expr(out, addr)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i32.load8_u{}\n", offset));
        }
        ExprData::Read2(addr, offset) => {
            gen_expr(out, addr)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i32.load16_u{}\n", offset));
        }
        ExprData::Read4(addr, offset) => {
            gen_expr(out, addr)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i32.load{}\n", offset));
        }
        ExprData::Read8(addr, offset) => {
            gen_expr(out, addr)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i64.load{}\n", offset));
        }
        ExprData::Write1(addr, data, offset) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i32.store8{}\n", offset));
        }
        ExprData::Write2(addr, data, offset) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i32.store16{}\n", offset));
        }
        ExprData::Write4(addr, data, offset) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i32.store{}\n", offset));
        }
        ExprData::Write8(addr, data, offset) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            let offset = if *offset != 0 {
                format!(" offset={}", offset)
            } else {
                "".to_owned()
            };
            out.push_str(&format!("i64.store{}\n", offset));
        }
    }
    Ok(())
}
