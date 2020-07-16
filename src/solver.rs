use crate::ast::*;
use crate::ir::*;
use crate::scope::*;
use crate::Error;
use std::cell::RefCell;
use std::rc::Rc;

pub fn solve(files: &Vec<File>) -> Result<Program, Error> {
    let mut gscope = GlobalScope::new();

    let mut records_with_ast = Vec::new();
    let mut funcs_with_ast = Vec::new();
    let mut records = Vec::new();
    let mut externs = Vec::new();
    let mut funcs = Vec::new();
    let mut globals = Vec::new();

    // initialize type names in gscope
    for file in files {
        for node in &file.records {
            let rec = Rc::new(Record {
                span: node.span.clone(),
                name: node.name.clone(),
                fields: RefCell::new(vec![]),
            });
            records.push(rec.clone());
            records_with_ast.push((rec.clone(), node));
            gscope.decl(node.name.clone(), Item::Record(rec))?;
        }
    }

    // initialize the fields for records
    for (rec, node) in &records_with_ast {
        let mut fields = Vec::new();
        for (field_name, field_type_expr) in &node.fields {
            let field_type = gscope.resolve_type(field_type_expr)?;
            fields.push((field_name.clone(), field_type));
        }
        *rec.fields.borrow_mut() = fields;
    }

    // initialize extern/function prototypes
    for file in files {
        for node in &file.externs {
            let type_ = gscope.resolve_func_type(&node.type_)?;
            let ext = Rc::new(Extern {
                span: node.span.clone(),
                path: node.path.clone(),
                name: node.name.clone(),
                type_,
            });
            externs.push(ext.clone());
            gscope.decl(node.name.clone(), Item::Extern(ext))?;
        }
        for node in &file.funcs {
            let type_ = gscope.resolve_func_type(&node.type_)?;
            let func = Rc::new(Func {
                span: node.span.clone(),
                name: node.name.clone(),
                type_,
                parameters: RefCell::new(vec![]),
                locals: RefCell::new(vec![]),
                body: RefCell::new(None),
            });
            funcs.push(func.clone());
            funcs_with_ast.push((func.clone(), node));
            gscope.decl(node.name.clone(), Item::Func(func))?;
        }
    }

    // resolve global variables
    let mut gvar_init_lscope = LocalScope::new(&mut gscope, None);
    for file in files {
        for node in &file.globals {
            let span = node.span.clone();
            let name = node.name.clone();
            let (type_, expr) = if let Some(type_) = &node.type_ {
                let type_ = gvar_init_lscope.gscope().resolve_type(type_)?;
                let expr =
                    solve_typed_expr(&mut gvar_init_lscope, &node.init, &type_.clone().into())?;
                (type_, expr)
            } else {
                let expr = solve_value_expr(&mut gvar_init_lscope, &node.init, None)?;
                let type_ = expr.type_.value().cloned().unwrap();
                (type_, expr)
            };
            let global = gvar_init_lscope.gscope().declvar(span, name, type_, expr)?;
            globals.push(global);
        }
    }
    let gvar_init_locals = gvar_init_lscope.locals().clone();

    // resolve functions
    let mut main_found = false;
    for (func, node) in &funcs_with_ast {
        if "Main" == func.name.as_ref() {
            main_found = true;
            if func.type_
                != (FuncType {
                    parameters: vec![],
                    return_type: ReturnType::Void,
                })
            {
                return Err(Error {
                    span: vec![func.span.clone()],
                    message: format!("Expected Main to have type ()void, but got {}", func.type_),
                });
            }
        }
        solve_func(&mut gscope, func, node)?;
    }
    if !main_found {
        return Err(Error {
            span: vec![],
            message: format!("Main function not found"),
        });
    }

    Ok(Program {
        span: files[0].span.clone(),
        externs,
        globals,
        funcs,
        records,
        gvar_init_locals,
    })
}

fn solve_func(gscope: &mut GlobalScope, func: &Rc<Func>, node: &RawFunc) -> Result<(), Error> {
    let mut lscope = LocalScope::new(gscope, Some(func));
    for (param_name, param_type) in &func.type_.parameters {
        let local = lscope.declvar(func.span.clone(), param_name.clone(), param_type.clone())?;
        func.parameters.borrow_mut().push(local);
    }
    let body = solve_stmt(&mut lscope, &node.body)?;
    match &func.type_.return_type {
        ReturnType::Void => {}
        ReturnType::NoReturn => panic!("TODO: figure out how noreturn will work"),
        ReturnType::Type(_) => {
            match &body.return_state {
                ReturnState::AlwaysReturns => {}
                ReturnState::MaybeReturns | ReturnState::NeverReturns => {
                    return Err(Error {
                        span: vec![func.span.clone()],
                        message: format!("Function might not return"),
                    });
                }
                ReturnState::Unreachable => {
                    // TODO: think and determine whether this is ok
                    return Err(Error {
                        span: vec![func.span.clone()],
                        message: format!("Unreachable"),
                    });
                }
            }
        }
    }
    *func.locals.borrow_mut() = lscope.locals().clone();
    *func.body.borrow_mut() = Some(body);
    Ok(())
}

fn solve_stmt(lscope: &mut LocalScope, node: &RawStmt) -> Result<Stmt, Error> {
    match &node.data {
        RawStmtData::Block(nodes) => {
            let mut stmts = Vec::new();
            let mut return_state = ReturnState::NeverReturns;
            for child_node in nodes {
                let stmt = solve_stmt(lscope, child_node)?;
                return_state = return_state.and_then(&stmt.return_state);
                if let ReturnState::Unreachable = return_state {
                    return Err(Error {
                        span: vec![stmt.span.clone()],
                        message: format!("Unreachable statement"),
                    });
                }
                stmts.push(stmt);
            }
            Ok(Stmt {
                span: node.span.clone(),
                return_state,
                data: StmtData::Block(stmts),
            })
        }
        RawStmtData::DeclVar(name, texpr, setexpr) => {
            let setexpr = if let Some(texpr) = texpr {
                solve_typed_expr(lscope, setexpr, &lscope.resolve_type(texpr)?.into())?
            } else {
                solve_value_expr(lscope, setexpr, None)?
            };
            let local = lscope.declvar(
                node.span.clone(),
                name.clone(),
                setexpr.type_.value().cloned().unwrap(),
            )?;
            Ok(Stmt {
                span: node.span.clone(),
                return_state: ReturnState::NeverReturns,
                data: StmtData::Expr(Expr {
                    span: node.span.clone(),
                    type_: ReturnType::Void,
                    data: ExprData::SetLocal(local, setexpr.into()),
                }),
            })
        }
        RawStmtData::Return(enode) => {
            if let Some(rtype) = lscope.return_type().cloned() {
                let expr = solve_typed_expr(lscope, enode, &rtype)?;
                Ok(Stmt {
                    span: node.span.clone(),
                    return_state: ReturnState::AlwaysReturns,
                    data: StmtData::Return(expr),
                })
            } else {
                Err(Error {
                    span: vec![node.span.clone()],
                    message: format!("return is not allowed here"),
                })
            }
        }
        RawStmtData::Expr(enode) => {
            let expr = solve_typed_expr(lscope, enode, &ReturnType::Void)?;
            Ok(Stmt {
                span: node.span.clone(),
                return_state: ReturnState::NeverReturns,
                data: StmtData::Expr(expr),
            })
        }
    }
}

fn solve_value_expr(
    lscope: &mut LocalScope,
    node: &RawExpr,
    hint: Option<&Type>,
) -> Result<Expr, Error> {
    let expr = solve_expr(lscope, node, hint.cloned().map(|h| h.into()).as_ref())?;
    match &expr.type_ {
        ReturnType::Type(_) => {}
        _ => {
            return Err(Error {
                span: vec![node.span.clone()],
                message: format!("Expected value expression but got {}", expr.type_),
            })
        }
    }
    Ok(expr)
}

/// For when we know the exact type the given expression must be
fn solve_typed_expr(
    lscope: &mut LocalScope,
    node: &RawExpr,
    expected_type: &ReturnType,
) -> Result<Expr, Error> {
    let expr = solve_expr(lscope, node, Some(expected_type))?;
    auto_cast(lscope, expr, expected_type)
}

fn auto_cast(
    _lscope: &mut LocalScope,
    expr: Expr,
    expected_type: &ReturnType,
) -> Result<Expr, Error> {
    match (&expr.type_, expected_type) {
        (a, b) if a == b => Ok(expr),
        (ReturnType::Type(Type::Bool), ReturnType::Void)
        | (ReturnType::Type(Type::I32), ReturnType::Void)
        | (ReturnType::Type(Type::I64), ReturnType::Void)
        | (ReturnType::Type(Type::F32), ReturnType::Void)
        | (ReturnType::Type(Type::F64), ReturnType::Void) => Ok(Expr {
            span: expr.span.clone(),
            type_: ReturnType::Void,
            data: ExprData::DropPrimitive(expr.into()),
        }),
        _ => Err(Error {
            span: vec![expr.span.clone()],
            message: format!("Expected {} but got {}", expected_type, expr.type_),
        }),
    }
}

/// For resolving an expression given a type hint
/// the actual expr may ignore the hint.
/// If the given return type is required, or a value type (i.e. non-void/noreturn type)
/// is required, see solve_typed_expr and solve_value_expr
fn solve_expr(
    lscope: &mut LocalScope,
    node: &RawExpr,
    hint: Option<&ReturnType>,
) -> Result<Expr, Error> {
    match &node.data {
        RawExprData::Void => Ok(Expr {
            span: node.span.clone(),
            type_: ReturnType::Void,
            data: ExprData::Void,
        }),
        RawExprData::Bool(x) => Ok(Expr {
            span: node.span.clone(),
            type_: Type::Bool.into(),
            data: ExprData::Bool(*x),
        }),
        RawExprData::Int(x) => match hint.as_ref().and_then(|h| h.value()) {
            Some(Type::I64) => Ok(Expr {
                span: node.span.clone(),
                type_: Type::I64.into(),
                data: ExprData::I64(*x as i64),
            }),
            Some(Type::F32) => Ok(Expr {
                span: node.span.clone(),
                type_: Type::F32.into(),
                data: ExprData::F32(*x as f32),
            }),
            Some(Type::F64) => Ok(Expr {
                span: node.span.clone(),
                type_: Type::F64.into(),
                data: ExprData::F64(*x as f64),
            }),
            _ => Ok(Expr {
                span: node.span.clone(),
                type_: Type::I32.into(),
                data: ExprData::I32(*x as i32),
            }),
        },
        RawExprData::Float(x) => match hint.as_ref().and_then(|h| h.value()) {
            Some(Type::F32) => Ok(Expr {
                span: node.span.clone(),
                type_: Type::F32.into(),
                data: ExprData::F32(*x as f32),
            }),
            _ => Ok(Expr {
                span: node.span.clone(),
                type_: Type::F64.into(),
                data: ExprData::F64(*x as f64),
            }),
        },
        RawExprData::GetVar(name) => match lscope.get(name) {
            Some(Item::Local(local)) => Ok(Expr {
                span: node.span.clone(),
                type_: local.type_.clone().into(),
                data: ExprData::GetLocal(local.clone()),
            }),
            Some(Item::Global(global)) => Ok(Expr {
                span: node.span.clone(),
                type_: global.type_.clone().into(),
                data: ExprData::GetGlobal(global.clone()),
            }),
            Some(item) => Err(Error {
                span: vec![node.span.clone(), item.span().clone()],
                message: format!("{} is not a variable (getvar)", name),
            }),
            _ => Err(Error {
                span: vec![node.span.clone()],
                message: format!("{} not found (getvar)", name),
            }),
        },
        RawExprData::SetVar(name, enode) => match lscope.get(name).cloned() {
            Some(Item::Local(local)) => Ok(Expr {
                span: node.span.clone(),
                type_: local.type_.clone().into(),
                data: ExprData::SetLocal(
                    local.clone(),
                    solve_typed_expr(lscope, enode, &local.type_.clone().into())?.into(),
                ),
            }),
            Some(Item::Global(global)) => Ok(Expr {
                span: node.span.clone(),
                type_: global.type_.clone().into(),
                data: ExprData::SetGlobal(
                    global.clone(),
                    solve_typed_expr(lscope, enode, &global.type_.clone().into())?.into(),
                ),
            }),
            Some(item) => Err(Error {
                span: vec![node.span.clone(), item.span().clone()],
                message: format!("{} is not a variable (setvar)", name),
            }),
            _ => Err(Error {
                span: vec![node.span.clone()],
                message: format!("{} not found (setvar)", name),
            }),
        },
        RawExprData::CallFunc(fname, raw_args) => {
            let func = lscope.get_callable(&node.span, fname)?;
            if func.type_().parameters.len() != raw_args.len() {
                return Err(Error {
                    span: vec![node.span.clone(), func.span().clone()],
                    message: format!(
                        "Expected {} args, but got {}",
                        func.type_().parameters.len(),
                        raw_args.len()
                    ),
                });
            }
            let mut args = Vec::new();
            for (raw_arg, (_, arg_type)) in raw_args.iter().zip(&func.type_().parameters) {
                let arg = solve_typed_expr(lscope, raw_arg, &arg_type.clone().into())?;
                args.push(arg);
            }
            Ok(Expr {
                span: node.span.clone(),
                type_: func.type_().return_type.clone(),
                data: match func {
                    Callable::Func(func) => ExprData::CallFunc(func, args),
                    Callable::Extern(func) => ExprData::CallExtern(func, args),
                },
            })
        }
        RawExprData::Asm(raw_args, type_, code) => {
            let mut args = Vec::new();
            for raw_arg in raw_args {
                args.push(solve_value_expr(lscope, raw_arg, None)?);
            }
            let type_ = lscope.resolve_type(type_)?;
            Ok(Expr {
                span: node.span.clone(),
                type_: type_.clone().into(),
                data: ExprData::Asm(args, type_, code.clone()),
            })
        }
        RawExprData::Char(ch) => Ok(Expr {
            span: node.span.clone(),
            type_: Type::I32.into(),
            data: ExprData::I32(*ch as i32),
        }),
        RawExprData::Read1(addr) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: Type::I32.into(),
                data: ExprData::Read1(addr.into()),
            })
        }
        RawExprData::Read2(addr) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: Type::I32.into(),
                data: ExprData::Read2(addr.into()),
            })
        }
        RawExprData::Read4(addr) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: Type::I32.into(),
                data: ExprData::Read4(addr.into()),
            })
        }
        RawExprData::Read8(addr) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: Type::I64.into(),
                data: ExprData::Read4(addr.into()),
            })
        }
        RawExprData::Write1(addr, data) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            let data = solve_typed_expr(lscope, data, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: ReturnType::Void,
                data: ExprData::Write1(addr.into(), data.into()),
            })
        }
        RawExprData::Write2(addr, data) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            let data = solve_typed_expr(lscope, data, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: ReturnType::Void,
                data: ExprData::Write2(addr.into(), data.into()),
            })
        }
        RawExprData::Write4(addr, data) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            let data = solve_typed_expr(lscope, data, &Type::I32.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: ReturnType::Void,
                data: ExprData::Write4(addr.into(), data.into()),
            })
        }
        RawExprData::Write8(addr, data) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            let data = solve_typed_expr(lscope, data, &Type::I64.into())?;
            Ok(Expr {
                span: node.span.clone(),
                type_: ReturnType::Void,
                data: ExprData::Write8(addr.into(), data.into()),
            })
        }
    }
}
