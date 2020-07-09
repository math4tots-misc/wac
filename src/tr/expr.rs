use super::*;

pub(super) fn translate_expr(
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
            let ptr = out.intern_str(value);
            sink.writeln(format!("(i32.const {})", ptr));
            retain(lscope, sink, Type::String, DropPolicy::Keep);
            auto_cast(sink, span, lscope, ReturnType::Value(Type::String), etype)?;
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
                auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
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
            translate_fcall(out, lscope, sink, etype, span, fname, &argexprs.iter().collect())?;
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
        Expr::While(span, cond, body) => {
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

            auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
        }
        Expr::GetAttr(_span, owner, field) => match get_type_from_expr(lscope, owner) {
            Some(type_) => match type_ {
                Type::Enum(_) => match get_enum_value_from_name(type_, field) {
                    Some(value) => {
                        sink.writeln(format!("(i32.const {})", value));
                    }
                    None => {
                        return Err(Error::Type {
                            span: owner.span().clone(),
                            expected: format!("{} in enum {}", field, type_),
                            got: "not found".into(),
                        })
                    }
                },
                _ => {
                    return Err(Error::Type {
                        span: owner.span().clone(),
                        expected: "expression".into(),
                        got: format!("type {}", type_),
                    })
                }
            },
            None => {
                return Err(Error::Type {
                    span: owner.span().clone(),
                    expected: "enum".into(),
                    got: format!("{} expression", guess_type(lscope, expr)?),
                })
            }
        },
        Expr::Binop(span, op, left, right) => {
            // == binops ==
            // identity ops
            //   is, is not
            //     * always returns bool
            //     * arguments same type
            //     * applies to (almost?) any type
            // equality ops
            //   ==, !=
            //     * Syntactic sugar for Eq(..), !Eq(..)
            // comparison ops
            //   <, >, <=, >=
            //     * always returns bool
            //     * arguments same type
            //     * applies to numeric types, list and str
            // arithmetic ops
            //   +, -, *, %
            //     * either (i32, i32) or mixed i32, f32.
            //     * always returns same as argument type (f32 if mixed)
            //     * arguments always same type
            //         ints may be converted to floats
            //     * applies to numeric types
            // division ops
            //   /, //
            //     * / always returns f32, // always returns i32
            //     * arguments may be i32 or f32
            // bitwise
            //   &, ^, |, <<, >>
            //     * only accepts i32
            //     * always returns i32
            match op {
                Binop::Equal => {
                    translate_fcall(out, lscope, sink, etype, span, &"Eq".into(), &vec![left, right])?;
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
                }
                Binop::NotEqual => {
                    translate_fcall(out, lscope, sink, etype, span, &"Eq".into(), &vec![left, right])?;
                    sink.writeln("i32.eqz");
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
                }
                Binop::Is | Binop::IsNot => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    let union_type = best_union_type(ltype, rtype);
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                    // for float types and 64-bit types, we need to use specific opcodes,
                    // but for all other values, they should be just i32
                    let code = match (op, union_type) {
                        (Binop::Is, Type::Id) => "i64.eq",
                        (Binop::Is, Type::I64) => "i64.eq",
                        (Binop::Is, Type::F64) => "f64.eq",
                        (Binop::Is, Type::F32) => "f32.eq",
                        (Binop::Is, _) => "i32.eq",

                        (Binop::IsNot, Type::Id) => "i64.ne",
                        (Binop::IsNot, Type::I64) => "i64.ne",
                        (Binop::IsNot, Type::F64) => "f64.ne",
                        (Binop::IsNot, Type::F32) => "f32.ne",
                        (Binop::IsNot, _) => "i32.ne",

                        (Binop::Equal, _) => panic!("TODO translate_expr =="),
                        (Binop::NotEqual, _) => panic!("TODO translate_expr !="),

                        _ => panic!("impossible eq op {:?}", op),
                    };
                    sink.writeln(code);
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
                }
                Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    let union_type = match best_union_type(ltype, rtype) {
                        Type::I32 => Type::I32,
                        _ => Type::F32,
                    };
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                    let code = match (op, union_type) {
                        (Binop::Less, Type::I32) => "i32.lt_s",
                        (Binop::Less, Type::F32) => "f32.lt",

                        (Binop::LessOrEqual, Type::I32) => "i32.le_s",
                        (Binop::LessOrEqual, Type::F32) => "f32.le",

                        (Binop::Greater, Type::I32) => "i32.gt_s",
                        (Binop::Greater, Type::F32) => "f32.gt",

                        (Binop::GreaterOrEqual, Type::I32) => "i32.ge_s",
                        (Binop::GreaterOrEqual, Type::F32) => "f32.ge",

                        (Binop::Less, _)
                        | (Binop::LessOrEqual, _)
                        | (Binop::Greater, _)
                        | (Binop::GreaterOrEqual, _) => Err(Error::Type {
                            span: span.clone(),
                            expected: "comparable values".into(),
                            got: format!("{:?}, {:?}", ltype, rtype),
                        })?,

                        _ => panic!("impossible cmp op {:?}", op),
                    };
                    sink.writeln(code);
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
                }
                Binop::Add | Binop::Subtract | Binop::Multiply | Binop::Remainder => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    let union_type = match best_union_type(ltype, rtype) {
                        Type::I32 => Type::I32,
                        _ => Type::F32,
                    };
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                    let code = match (op, union_type) {
                        (Binop::Add, Type::I32) => "i32.add",
                        (Binop::Add, Type::F32) => "f32.add",

                        (Binop::Subtract, Type::I32) => "i32.sub",
                        (Binop::Subtract, Type::F32) => "f32.sub",

                        (Binop::Multiply, Type::I32) => "i32.mul",
                        (Binop::Multiply, Type::F32) => "f32.mul",

                        (Binop::Remainder, Type::I32) => "i32.rem_s",
                        (Binop::Remainder, Type::F32) => panic!("f32 % not yet implemented"),

                        (Binop::Add, _)
                        | (Binop::Subtract, _)
                        | (Binop::Multiply, _)
                        | (Binop::Remainder, _) => Err(Error::Type {
                            span: span.clone(),
                            expected: "numeric values".into(),
                            got: format!("{:?}, {:?}", ltype, rtype),
                        })?,

                        _ => panic!("impossible arithmetic op {:?}", op),
                    };
                    sink.writeln(code);
                    auto_cast(sink, span, lscope, ReturnType::Value(union_type), etype)?;
                }
                Binop::Divide => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    let union_type = best_union_type(ltype, rtype);
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                    explicit_cast(
                        sink,
                        span,
                        lscope,
                        ReturnType::Value(union_type),
                        ReturnType::Value(Type::F32),
                    )?;
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                    explicit_cast(
                        sink,
                        span,
                        lscope,
                        ReturnType::Value(union_type),
                        ReturnType::Value(Type::F32),
                    )?;
                    sink.writeln("f32.div");
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::F32), etype)?;
                }
                Binop::TruncDivide => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    let union_type = best_union_type(ltype, rtype);
                    match union_type {
                        Type::I32 => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), right)?;
                            sink.writeln("i32.div_s");
                        }
                        Type::F32 | Type::Id => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), right)?;
                            sink.writeln("f32.div\ni32.trunc_f32_s");
                        }
                        _ => Err(Error::Type {
                            span: span.clone(),
                            expected: "trunc-divisible values".into(),
                            got: format!("{:?}, {:?}", ltype, rtype),
                        })?,
                    }
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
                }
                Binop::BitwiseAnd
                | Binop::BitwiseOr
                | Binop::BitwiseXor
                | Binop::ShiftLeft
                | Binop::ShiftRight => {
                    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), left)?;
                    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), right)?;
                    let code = match op {
                        Binop::BitwiseAnd => "i32.and",
                        Binop::BitwiseOr => "i32.or",
                        Binop::BitwiseXor => "i32.xor",
                        Binop::ShiftLeft => "i32.shl",
                        Binop::ShiftRight => "i32.shr_u",
                        _ => panic!("impossible bitwise op {:?}", op),
                    };
                    sink.writeln(code);
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
                }
            }
        }
        Expr::Unop(span, op, expr) => match op {
            // == unops ==
            // sign ops
            //   +, -
            //     * returns i32 or f32 to match arg type
            // logical
            //   !
            //     * always returns bool
            Unop::Plus | Unop::Minus => {
                let guessed_type = match guess_type(lscope, expr)? {
                    Type::I32 => Type::I32,
                    _ => Type::F32,
                };
                translate_expr(out, sink, lscope, ReturnType::Value(guessed_type), expr)?;
                auto_cast(sink, span, lscope, ReturnType::Value(guessed_type), etype)?
            }
            Unop::Not => {
                translate_expr(out, sink, lscope, ReturnType::Value(Type::Bool), expr)?;
                sink.writeln("i32.eqz");
                auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?
            }
        },
        Expr::AssertType(span, type_, expr) => {
            translate_expr(out, sink, lscope, ReturnType::Value(*type_), expr)?;
            auto_cast(sink, span, lscope, ReturnType::Value(*type_), etype)?
        }
        Expr::CString(span, value) => {
            let ptr = out.intern_cstr(value);
            sink.writeln(format!("i32.const {}", ptr));
            auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?
        }
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

/// Duplicates TOS
/// Requires a WasmType -- this function does not take into account
/// any sort of reference counting
pub(super) fn raw_dup(lscope: &mut LocalScope, sink: &Rc<Sink>, wasm_type: WasmType) {
    let t = translate_wasm_type(wasm_type);
    let tmpvar = format!("$rt_tmp_dup_{}", t);
    lscope.helper_shared(&tmpvar, wasm_type.wac());
    sink.writeln(format!("local.tee {}", tmpvar));
    sink.writeln(format!("local.get {}", tmpvar));
}

/// adds opcodes to convert an i32 type to an 'id'
pub(super) fn cast_to_id(sink: &Rc<Sink>, tag: i32) {
    sink.writeln("i64.extend_i32_u");
    sink.writeln(format!("i64.const {}", tag));
    sink.writeln("i64.const 32");
    sink.writeln("i64.shl");
    sink.writeln("i64.or");
}
