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
    let memory = gscope.memory().clone();

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

    // initialize global constants
    for file in files {
        for node in &file.constants {
            let hint = if let Some(type_) = &node.type_ {
                Some(gscope.resolve_type(type_)?)
            } else {
                None
            };
            let value = solve_constexpr(&mut gscope, &node.expr, hint)?;
            gscope.declconst(node.span.clone(), node.name.clone(), value)?;
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
        memory,
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

fn solve_constexpr(
    gscope: &mut GlobalScope,
    node: &RawExpr,
    hint: Option<Type>,
) -> Result<ConstVal, Error> {
    match &node.data {
        RawExprData::Int(x) => Ok(ConstVal::I32(*x as i32)),
        RawExprData::GetVar(name) => Ok(gscope.get_constant(&node.span, name)?.value.clone()),
        RawExprData::Binop(op, lhs, rhs) => match op {
            Binop::Add
            | Binop::Subtract
            | Binop::Multiply
            | Binop::Remainder
            | Binop::TruncDivide => {
                let lhs = solve_constexpr(gscope, lhs, hint.clone())?;
                let rhs = solve_constexpr(gscope, rhs, hint)?;
                match (op, &lhs, &rhs) {
                    (Binop::Add, ConstVal::I32(a), ConstVal::I32(b)) => Ok(ConstVal::I32(a + b)),
                    (Binop::Subtract, ConstVal::I32(a), ConstVal::I32(b)) => {
                        Ok(ConstVal::I32(a - b))
                    }
                    (Binop::Multiply, ConstVal::I32(a), ConstVal::I32(b)) => {
                        Ok(ConstVal::I32(a + b))
                    }
                    (Binop::Remainder, ConstVal::I32(a), ConstVal::I32(b)) => {
                        Ok(ConstVal::I32(a % b))
                    }
                    (Binop::TruncDivide, ConstVal::I32(a), ConstVal::I32(b)) => {
                        Ok(ConstVal::I32(a / b))
                    }
                    _ => Err(Error {
                        span: vec![node.span.clone()],
                        message: format!(
                            "Unsupported constexpr binop {:?}, {:?}, {:?}",
                            op, lhs, rhs,
                        ),
                    }),
                }
            }
            _ => Err(Error {
                span: vec![node.span.clone()],
                message: format!("Unsupported constexpr binop {:?}", op),
            }),
        },
        _ => Err(Error {
            span: vec![node.span.clone()],
            message: format!("Expected constexpr"),
        }),
    }
}

fn solve_constexpr_u32(
    gscope: &mut GlobalScope,
    node: &RawExpr,
    hint: Option<Type>,
) -> Result<u32, Error> {
    match solve_constexpr(gscope, node, hint)? {
        ConstVal::I32(x) if x >= 0 => Ok(x as u32),
        _ => Err(Error {
            span: vec![node.span.clone()],
            message: format!("Expected u32 constexpr"),
        }),
    }
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
        RawStmtData::If(raw_pairs, raw_other) => {
            let mut pairs = Vec::<(Expr, Stmt)>::new();
            for (raw_cond, raw_body) in raw_pairs {
                let cond = solve_typed_expr(lscope, raw_cond, &Type::Bool.into())?;
                let body = solve_stmt(lscope, raw_body)?;
                pairs.push((cond, body));
            }
            let other = if let Some(raw_other) = raw_other {
                solve_stmt(lscope, raw_other)?
            } else {
                Stmt {
                    span: node.span.clone(),
                    return_state: ReturnState::NeverReturns,
                    data: StmtData::Block(vec![]),
                }
            };
            let mut return_state = other.return_state.clone();
            for (_, body) in &pairs {
                return_state = body.return_state.clone().or_else(&return_state);
            }
            Ok(Stmt {
                span: node.span.clone(),
                return_state,
                data: StmtData::If(pairs, other.into()),
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
                    data: ExprData::SetVar(Variable::Local(local), setexpr.into()),
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
        (ReturnType::Type(Type::I32), ReturnType::Type(Type::F32)) => Ok(Expr {
            span: expr.span.clone(),
            type_: Type::F32.into(),
            data: ExprData::Op(
                TypedWasmOp {
                    type_: Type::F32.wasm(),
                    op: UntypedWasmOp::convert_i32_s,
                },
                vec![expr],
            ),
        }),
        (ReturnType::Type(Type::I64), ReturnType::Type(Type::F32)) => Ok(Expr {
            span: expr.span.clone(),
            type_: Type::F32.into(),
            data: ExprData::Op(
                TypedWasmOp {
                    type_: Type::F32.wasm(),
                    op: UntypedWasmOp::convert_i64_s,
                },
                vec![expr],
            ),
        }),
        (ReturnType::Type(Type::I64), ReturnType::Type(Type::F64)) => Ok(Expr {
            span: expr.span.clone(),
            type_: Type::F64.into(),
            data: ExprData::Op(
                TypedWasmOp {
                    type_: Type::F64.wasm(),
                    op: UntypedWasmOp::convert_i64_s,
                },
                vec![expr],
            ),
        }),
        (ReturnType::Type(Type::I32), ReturnType::Type(Type::F64)) => Ok(Expr {
            span: expr.span.clone(),
            type_: Type::F64.into(),
            data: ExprData::Op(
                TypedWasmOp {
                    type_: Type::F64.wasm(),
                    op: UntypedWasmOp::convert_i32_s,
                },
                vec![expr],
            ),
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
        RawExprData::Str(string) => {
            lscope.memory().borrow_mut().intern(string);
            Ok(Expr {
                span: node.span.clone(),
                type_: Type::Str.into(),
                data: ExprData::Str(StrPtr {
                    memory: lscope.memory().clone(),
                    string: string.clone(),
                }),
            })
        }
        RawExprData::GetVar(name) => match lscope.get_variable_or_constant(&node.span, name)? {
            VariableOrConstant::Variable(var) => Ok(Expr {
                span: node.span.clone(),
                type_: var.type_().clone().into(),
                data: ExprData::GetVar(var),
            }),
            VariableOrConstant::Constant(cnst) => Ok(Expr {
                span: node.span.clone(),
                type_: cnst.value.type_().clone().into(),
                data: match &cnst.value {
                    ConstVal::I32(value) => ExprData::I32(*value),
                },
            }),
        },
        RawExprData::SetVar(name, enode) => {
            let var = lscope.get_variable(&node.span, name)?;
            Ok(Expr {
                span: node.span.clone(),
                type_: var.type_().clone().into(),
                data: ExprData::SetVar(
                    var.clone(),
                    solve_typed_expr(lscope, enode, &var.type_().clone().into())?.into(),
                ),
            })
        }
        RawExprData::AugVar(name, op, arg) => {
            let var = lscope.get_variable(&node.span, name)?;
            match op {
                Binop::Add | Binop::Subtract => {
                    let type_ = var.type_().clone();
                    match type_ {
                        Type::I32 => Ok(Expr {
                            span: node.span.clone(),
                            type_: ReturnType::Void,
                            data: ExprData::AugVar(
                                var,
                                TypedWasmOp {
                                    op: UntypedWasmOp::from_binop_for_int(*op).unwrap(),
                                    type_: type_.wasm(),
                                },
                                solve_typed_expr(lscope, arg, &type_.into())?.into(),
                            ),
                        }),
                        _ => Err(Error {
                            span: vec![node.span.clone()],
                            message: format!("Aug{:?} not supported for {}", op, type_),
                        }),
                    }
                }
                _ => panic!("Augassign {:?} not yet supported", op),
            }
        }
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
        RawExprData::Unop(op, arg) => {
            let arg = solve_value_expr(lscope, arg, hint.and_then(|t| t.value()))?;
            match (op, arg.type_.value().unwrap()) {
                (Unop::Positive, Type::I32)
                | (Unop::Positive, Type::I64)
                | (Unop::Positive, Type::F32)
                | (Unop::Positive, Type::F64) => Ok(arg),
                (op, type_) => Err(Error {
                    span: vec![node.span.clone()],
                    message: format!("{:?} {} op combo not supported", op, type_),
                }),
            }
        }
        RawExprData::Binop(op, arg1, arg2) => match op {
            Binop::Is | Binop::IsNot => {
                let arg1 = solve_value_expr(lscope, arg1, None)?;
                let arg2 = solve_value_expr(lscope, arg2, Some(arg1.type_.value().unwrap()))?;
                Ok(Expr {
                    span: node.span.clone(),
                    type_: Type::Bool.into(),
                    data: ExprData::Op(
                        TypedWasmOp {
                            op: match op {
                                Binop::Is => UntypedWasmOp::eq,
                                Binop::IsNot => UntypedWasmOp::ne,
                                _ => panic!("impossible binop is/isnot"),
                            },
                            type_: arg1.type_.value().unwrap().wasm(),
                        },
                        vec![arg1, arg2],
                    ),
                })
            }
            Binop::LessThan
            | Binop::LessThanOrEqual
            | Binop::GreaterThan
            | Binop::GreaterThanOrEqual => {
                let arg1 = solve_value_expr(lscope, arg1, None)?;
                let arg2 = solve_value_expr(lscope, arg2, None)?;
                let type_ = match (arg1.type_.value().unwrap(), arg2.type_.value().unwrap()) {
                    (Type::F64, _) | (_, Type::F64) => Type::F64,
                    (Type::F32, _) | (_, Type::F32) => Type::F32,
                    (Type::I64, _) | (_, Type::I64) => Type::I64,
                    _ => Type::I32,
                };
                let arg1 = auto_cast(lscope, arg1, &type_.clone().into())?;
                let arg2 = auto_cast(lscope, arg2, &type_.clone().into())?;
                match type_ {
                    Type::I32 | Type::I64 => Ok(Expr {
                        span: node.span.clone(),
                        type_: Type::Bool.into(),
                        data: ExprData::Op(
                            TypedWasmOp {
                                op: UntypedWasmOp::from_binop_for_int(*op).unwrap(),
                                type_: type_.wasm(),
                            },
                            vec![arg1, arg2],
                        ),
                    }),
                    Type::F32 | Type::F64 => Ok(Expr {
                        span: node.span.clone(),
                        type_: Type::Bool.into(),
                        data: ExprData::Op(
                            TypedWasmOp {
                                op: UntypedWasmOp::from_binop_for_float(*op).unwrap(),
                                type_: type_.wasm(),
                            },
                            vec![arg1, arg2],
                        ),
                    }),
                    _ => Err(Error {
                        span: vec![node.span.clone()],
                        message: format!("{:?} not supported for {}", op, type_),
                    }),
                }
            }
            Binop::Add | Binop::Subtract | Binop::Multiply | Binop::Remainder => {
                let arg1 = solve_value_expr(lscope, arg1, None)?;
                let arg2 = solve_value_expr(lscope, arg2, None)?;
                let type_ = match (arg1.type_.value().unwrap(), arg2.type_.value().unwrap()) {
                    (Type::F64, _) | (_, Type::F64) => Type::F64,
                    (Type::F32, _) | (_, Type::F32) => Type::F32,
                    (Type::I64, _) | (_, Type::I64) => Type::I64,
                    _ => Type::I32,
                };
                let arg1 = auto_cast(lscope, arg1, &type_.clone().into())?;
                let arg2 = auto_cast(lscope, arg2, &type_.clone().into())?;
                match type_ {
                    Type::I32 | Type::I64 => Ok(Expr {
                        span: node.span.clone(),
                        type_: type_.clone().into(),
                        data: ExprData::Op(
                            TypedWasmOp {
                                op: UntypedWasmOp::from_binop_for_int(*op).unwrap(),
                                type_: type_.wasm(),
                            },
                            vec![arg1, arg2],
                        ),
                    }),
                    Type::F32 | Type::F64 => Ok(Expr {
                        span: node.span.clone(),
                        type_: type_.clone().into(),
                        data: ExprData::Op(
                            TypedWasmOp {
                                op: UntypedWasmOp::from_binop_for_float(*op).unwrap(),
                                type_: type_.wasm(),
                            },
                            vec![arg1, arg2],
                        ),
                    }),
                    _ => Err(Error {
                        span: vec![node.span.clone()],
                        message: format!("{:?} not supported for {}", op, type_),
                    }),
                }
            }
            Binop::Divide => {
                let arg1 = solve_value_expr(lscope, arg1, None)?;
                let type_ = match &arg1.type_ {
                    ReturnType::Type(Type::I32) => Type::F32,
                    ReturnType::Type(Type::I64) => Type::F64,
                    _ => arg1.type_.value().unwrap().clone(),
                };
                let arg1 = auto_cast(lscope, arg1, &type_.clone().into())?;
                let arg2 = solve_typed_expr(lscope, arg2, &type_.clone().into())?;
                match type_ {
                    Type::F32 | Type::F64 => Ok(Expr {
                        span: node.span.clone(),
                        type_: type_.clone().into(),
                        data: ExprData::Op(
                            TypedWasmOp {
                                op: UntypedWasmOp::from_binop_for_float(*op).unwrap(),
                                type_: type_.wasm(),
                            },
                            vec![arg1, arg2],
                        ),
                    }),
                    _ => Err(Error {
                        span: vec![node.span.clone()],
                        message: format!("{:?} not supported for {}", op, type_),
                    }),
                }
            }
            Binop::TruncDivide => {
                let arg1 = solve_value_expr(lscope, arg1, None)?;
                let arg2 = solve_value_expr(lscope, arg2, None)?;
                let intermediate_type =
                    match (arg1.type_.value().unwrap(), arg2.type_.value().unwrap()) {
                        (Type::F64, _) | (_, Type::F64) => Type::F64,
                        (Type::F32, _) | (_, Type::F32) => Type::F32,
                        (Type::I64, _) | (_, Type::I64) => Type::I64,
                        _ => Type::I32,
                    };
                let arg1 = auto_cast(lscope, arg1, &intermediate_type.clone().into())?;
                let arg2 = auto_cast(lscope, arg2, &intermediate_type.clone().into())?;
                match intermediate_type {
                    Type::F32 => auto_cast(
                        lscope,
                        Expr {
                            span: node.span.clone(),
                            type_: intermediate_type.clone().into(),
                            data: ExprData::Op(
                                TypedWasmOp {
                                    op: UntypedWasmOp::div,
                                    type_: intermediate_type.wasm(),
                                },
                                vec![arg1, arg2],
                            ),
                        },
                        &Type::I32.into(),
                    ),
                    Type::F64 => auto_cast(
                        lscope,
                        Expr {
                            span: node.span.clone(),
                            type_: intermediate_type.clone().into(),
                            data: ExprData::Op(
                                TypedWasmOp {
                                    op: UntypedWasmOp::div,
                                    type_: intermediate_type.wasm(),
                                },
                                vec![arg1, arg2],
                            ),
                        },
                        &Type::I64.into(),
                    ),
                    Type::I32 | Type::I64 => Ok(Expr {
                        span: node.span.clone(),
                        type_: intermediate_type.clone().into(),
                        data: ExprData::Op(
                            TypedWasmOp {
                                op: UntypedWasmOp::div_u,
                                type_: intermediate_type.wasm(),
                            },
                            vec![arg1, arg2],
                        ),
                    }),
                    it => panic!("Impossible truncdiv intermediate type: {}", it),
                }
            }
            _ => panic!("Operator {:?} not yet supported", op),
        },
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
        RawExprData::Raw(name) => {
            let var = lscope.get_variable(&node.span, name)?;
            Ok(Expr {
                span: node.span.clone(),
                type_: match var.type_() {
                    Type::I64 | Type::F64 | Type::Id => Type::I64.into(),
                    _ => Type::I32.into(),
                },
                data: ExprData::Raw(var),
            })
        }
        RawExprData::Char(ch) => Ok(Expr {
            span: node.span.clone(),
            type_: Type::I32.into(),
            data: ExprData::I32(*ch as i32),
        }),
        RawExprData::Read(byte_count, addr, offset) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            let offset = solve_constexpr_u32(lscope.gscope(), offset, Some(Type::I32))?;
            Ok(Expr {
                span: node.span.clone(),
                type_: (if let ByteCount::N8 = byte_count {
                    Type::I64
                } else {
                    Type::I32
                })
                .into(),
                data: ExprData::Read(*byte_count, addr.into(), offset),
            })
        }
        RawExprData::Write(byte_count, addr, data, offset) => {
            let addr = solve_typed_expr(lscope, addr, &Type::I32.into())?;
            let data = solve_typed_expr(
                lscope,
                data,
                &(if let ByteCount::N8 = byte_count {
                    Type::I64
                } else {
                    Type::I32
                })
                .into(),
            )?;
            let offset = solve_constexpr_u32(lscope.gscope(), offset, Some(Type::I32))?;
            Ok(Expr {
                span: node.span.clone(),
                type_: ReturnType::Void,
                data: ExprData::Write(*byte_count, addr.into(), data.into(), offset),
            })
        }
    }
}
