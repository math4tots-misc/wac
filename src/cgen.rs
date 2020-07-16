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

    for func in &program.funcs {
        gen_func(&mut out, func)?;
    }

    out.push_str(r#"(export "Main" (func $f/Main))"#);
    out.push_str("\n");

    Ok(out)
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
        out.push_str(&format!(" (param $l/{}/{})", param.id, param.name));
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
        out.push_str(&format!("(local $l/{}/{})\n", local.id, local.name));
    }

    out.push_str("(block $ret");
    out.push_str(&trrtype(&func.type_.return_type));
    out.push_str("\n");

    gen_stmt(out, func.body.borrow().as_ref().unwrap())?;

    out.push_str(")\n");
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
                out.push_str(&format!("{}.load $l/{}/{}", x.type_.wasm(), x.id, x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record GetLocal (retain)"),
        },
        ExprData::SetLocal(x, expr) => match x.type_ {
            Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 => {
                gen_expr(out, expr)?;
                out.push_str(&format!("{}.store $l/{}/{}", x.type_.wasm(), x.id, x.name));
            }
            Type::Record(_) => panic!("TODO: gen_expr record SetLocal (retain + release)"),
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
        ExprData::Read1(addr) => {
            gen_expr(out, addr)?;
            out.push_str("i32.load8_u\n");
        }
        ExprData::Read2(addr) => {
            gen_expr(out, addr)?;
            out.push_str("i32.load16_u\n");
        }
        ExprData::Read4(addr) => {
            gen_expr(out, addr)?;
            out.push_str("i32.load\n");
        }
        ExprData::Read8(addr) => {
            gen_expr(out, addr)?;
            out.push_str("i64.load\n");
        }
        ExprData::Write1(addr, data) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            out.push_str("i32.store8\n");
        }
        ExprData::Write2(addr, data) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            out.push_str("i32.store16\n");
        }
        ExprData::Write4(addr, data) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            out.push_str("i32.store\n");
        }
        ExprData::Write8(addr, data) => {
            gen_expr(out, addr)?;
            gen_expr(out, data)?;
            out.push_str("i64.store\n");
        }
    }
    Ok(())
}
