use super::*;

pub(super) fn translate_expr(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    expr: &Expr,
) -> Result<(), Error> {
    match expr {
        Expr::Bool(span, _, x) => {
            match etype {
                ReturnType::Value(Type::Bool) => {
                    sink.i32_const(if *x { 1 } else { 0 });
                }
                ReturnType::Value(Type::Id) => {
                    sink.i32_const(if *x { 1 } else { 0 });
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
        Expr::Int(span, _, x) => {
            match etype {
                ReturnType::Value(Type::I32) => {
                    sink.i32_const(*x as i32);
                }
                ReturnType::Value(Type::I64) => {
                    sink.i64_const(*x);
                }
                ReturnType::Value(Type::F32) => {
                    sink.f32_const(*x as f32);
                }
                ReturnType::Value(Type::F64) => {
                    sink.f64_const(*x as f64);
                }
                ReturnType::Value(Type::Id) => {
                    sink.i32_const(*x as i32);
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
        Expr::Float(span, _, x) => {
            match etype {
                ReturnType::Value(Type::F32) => {
                    sink.f32_const(*x as f32);
                }
                ReturnType::Value(Type::F64) => {
                    sink.f64_const(*x as f64);
                }
                ReturnType::Value(Type::Id) => {
                    sink.f32_const(*x as f32);
                    sink.i32_reinterpret_f32();
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
        Expr::String(span, _, value) => {
            let ptr = out.intern_str(value);
            sink.i32_const(ptr);
            retain(lscope, sink, Type::String, DropPolicy::Keep);
            auto_cast(sink, span, lscope, ReturnType::Value(Type::String), etype)?;
        }
        Expr::List(span, _, exprs) => {
            sink.call("$f___new_list");
            for expr in exprs {
                raw_dup(lscope, sink, WasmType::I32);
                translate_expr(out, sink, lscope, ReturnType::Value(Type::Id), expr)?;
                sink.call("$f___WAC_list_push_raw_no_retain");
            }
            auto_cast(sink, span, lscope, ReturnType::Value(Type::List), etype)?;
        }
        Expr::Block(span, _, exprs) => {
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
        Expr::GetVar(span, _, name) => {
            let entry = lscope.get_or_err(span.clone(), name)?;
            let gtype = entry.type_();
            match entry {
                ScopeEntry::Local(info) => {
                    sink.local_get(&info.wasm_name);
                    retain(lscope, sink, info.type_, DropPolicy::Keep);
                }
                ScopeEntry::Global(info) => {
                    sink.global_get(&info.wasm_name);
                    retain(lscope, sink, info.type_, DropPolicy::Keep);
                }
                ScopeEntry::Constant(info) => match &info.value {
                    ConstValue::I32(x) => {
                        sink.i32_const(*x);
                    }
                    ConstValue::Type(t) => {
                        sink.i32_const(t.tag());
                    }
                    ConstValue::Enum(_, value) => {
                        sink.i32_const(*value);
                    }
                },
            }
            auto_cast(sink, span, lscope, ReturnType::Value(gtype), etype)?;
        }
        Expr::SetVar(span, _, name, setexpr) => {
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
                    sink.local_set(&info.wasm_name);
                }
                ScopeEntry::Global(info) => {
                    translate_expr(out, sink, lscope, ReturnType::Value(info.type_), setexpr)?;
                    release_var(sink, Scope::Global, &info.wasm_name, info.type_);
                    sink.global_set(&info.wasm_name);
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
        Expr::DeclVar(span, _, name, type_, setexpr) => {
            let type_ = match type_ {
                Some(t) => *t,
                None => guess_type(lscope, setexpr)?,
            };
            // There's no need to retain here, because anything that's currently
            // on the stack already has a retain on it. By popping from the
            // stack, we're transferring the retain on the stack into the
            // variable itself.
            //
            // however, the old value still has to be released
            // normally the old value would be just '0', but a variable
            // may be declared more than once if it's part of a loop
            translate_expr(out, sink, lscope, ReturnType::Value(type_), setexpr)?;

            let info = lscope.decl(span.clone(), name.clone(), type_);
            release_var(sink, Scope::Local, &info.wasm_name, type_);

            sink.local_set(&info.wasm_name);
            auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
        }
        Expr::FunctionCall(span, _, fname, argexprs) => {
            translate_fcall(
                out,
                lscope,
                sink,
                etype,
                span,
                fname,
                &argexprs.iter().collect(),
            )?;
        }
        Expr::AssociatedFunctionCall(span, _, owner, fname_part, argexprs) => {
            let owner_type = guess_type(lscope, owner)?;
            let fname = format!("{}.{}", owner_type, fname_part);
            let mut args = vec![&**owner];
            args.extend(argexprs.iter());
            translate_fcall(out, lscope, sink, etype, span, &fname.into(), &args)?;
        }
        Expr::If(_span, _, pairs, other) => {
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
        Expr::While(span, _, cond, body) => {
            let break_label = lscope.new_label_id();
            let continue_label = lscope.new_label_id();
            lscope.break_labels.push(break_label);
            lscope.continue_labels.push(continue_label);

            sink.start_loop(break_label, continue_label);
            translate_expr(out, sink, lscope, ReturnType::Value(Type::Bool), cond)?;
            sink.i32_eqz();
            sink.br_if(break_label);
            translate_expr(out, sink, lscope, ReturnType::Void, body)?;
            sink.br(continue_label);
            sink.end_loop();

            lscope.break_labels.pop();
            lscope.continue_labels.pop();

            auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
        }
        Expr::For(span, _, name, start, end, body) => {
            let start_var = lscope.helper_unique("forstart", Type::I32);
            let end_var = lscope.helper_unique("forend", Type::I32);
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), start)?;
            sink.local_set(&start_var);
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), end)?;
            sink.local_set(&end_var);

            lscope.push();
            let var_info = lscope.decl(span.clone(), name.clone(), Type::I32);

            let break_label = lscope.new_label_id();
            let continue_label = lscope.new_label_id();
            lscope.break_labels.push(break_label);
            lscope.continue_labels.push(continue_label);

            sink.start_loop(break_label, continue_label);

            sink.local_get(&start_var);
            sink.local_get(&end_var);
            sink.writeln("i32.ge_s");
            sink.br_if(break_label);
            sink.local_get(&start_var);
            sink.local_set(&var_info.wasm_name);

            translate_expr(out, sink, lscope, ReturnType::Void, body)?;

            // increment the start variable
            sink.local_get(&start_var);
            sink.i32_const(1);
            sink.i32_add();
            sink.local_set(&start_var);
            sink.br(continue_label);
            sink.end_loop();

            lscope.break_labels.pop();
            lscope.continue_labels.pop();
            lscope.pop();

            auto_cast(sink, span, lscope, ReturnType::Void, etype)?;
        }
        Expr::GetAttr(span, _, owner, field) => match get_type_from_expr(lscope, owner) {
            Some(type_) => match type_ {
                Type::Enum(_) => match get_enum_value_from_name(type_, field) {
                    Some(value) => {
                        sink.i32_const(value);
                        auto_cast(sink, span, lscope, ReturnType::Value(type_), etype)?;
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
            None => match guess_type(lscope, owner)? {
                owner_type if owner_type.is_record() => {
                    let fields = &lscope.g.record_fields[&owner_type.name()];
                    let mut index = None;
                    for (i, (field_name, field_type)) in fields.iter().enumerate() {
                        if field == field_name {
                            index = Some((i, *field_type));
                            break;
                        }
                    }
                    if index.is_none() {
                        return Err(Error::Type {
                            span: owner.span().clone(),
                            expected: format!("field for {}", owner_type.name()),
                            got: format!("unrecognized field {}", field),
                        });
                    }
                    let (index, field_type) = index.unwrap();

                    let owner_var = lscope.helper_unique("getattr", Type::I32);

                    translate_expr(out, sink, lscope, ReturnType::Value(owner_type), owner)?;
                    raw_dup(lscope, sink, WasmType::I32);
                    sink.local_set(&owner_var);
                    sink.i32_const((8 + 8 * index) as i32);
                    sink.writeln("i32.add");
                    sink.writeln("i64.load");
                    auto_cast(
                        sink,
                        span,
                        lscope,
                        ReturnType::Value(Type::Id),
                        ReturnType::Value(field_type),
                    )?;
                    auto_cast(sink, span, lscope, ReturnType::Value(field_type), etype)?;

                    // make sure to free the owner value
                    release_var(sink, Scope::Local, &owner_var, owner_type);
                }
                owner_type => {
                    return Err(Error::Type {
                        span: owner.span().clone(),
                        expected: "enum type or record expression".into(),
                        got: format!("{} expression", owner_type),
                    })
                }
            },
        },
        Expr::GetItem(span, _, owner, index) => {
            translate_fcall(
                out,
                lscope,
                sink,
                etype,
                span,
                &"GetItem".into(),
                &vec![owner, index],
            )?;
        }
        Expr::SetItem(span, _, owner, index, setexpr) => {
            translate_fcall(
                out,
                lscope,
                sink,
                etype,
                span,
                &"SetItem".into(),
                &vec![owner, index, setexpr],
            )?;
        }
        Expr::Switch(span, _, src, pairs, other) => {
            // TODO: use br_table

            let src_type = {
                let mut src_type = ReturnType::NoReturn;
                for (cvals, _) in pairs {
                    for cval in cvals {
                        src_type =
                            best_union_return_type(src_type, ReturnType::Value(cval.type_()));
                    }
                }
                match src_type {
                    ReturnType::Value(src_type) => src_type,
                    _ => panic!("Impossible cval type in switch: {:?}", src_type),
                }
            };

            // we want to make sure src_type is a copy, integral type
            match src_type {
                Type::I32 | Type::Type | Type::Enum(_) => {}
                _ => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "i32, type or enum type in switch".into(),
                        got: format!("{}", src_type),
                    })
                }
            }
            let wasm_src_type = src_type.wasm();

            translate_expr(out, sink, lscope, ReturnType::Value(src_type), src)?;
            let srcvar = lscope.helper_unique("switchtmp", src_type);
            sink.local_set(&srcvar);

            let break_label = lscope.new_label_id();
            sink.start_block(
                break_label,
                match etype {
                    ReturnType::Value(etype) => Some(etype.wasm()),
                    ReturnType::NoReturn | ReturnType::Void => None,
                },
            );

            for (cvals, body) in pairs {
                // check if any of the cvals match
                for cval in cvals {
                    sink.local_get(&srcvar);
                    translate_constval_i32(out, sink, lscope, cval)?;
                    sink.writeln(format!("{}.eq", wasm_src_type));
                    sink.writeln("if (result i32)");
                    sink.i32_const(1);
                    sink.writeln("else");
                }
                sink.i32_const(0);
                for _ in cvals {
                    sink.writeln("end");
                }

                // if it matches, drop the src value, run the body
                // and break out of the entire switch statement
                sink.writeln(format!(
                    "if {}",
                    match etype {
                        ReturnType::Value(etype) => format!("(result {})", etype.wasm()),
                        ReturnType::NoReturn | ReturnType::Void => "".to_owned(),
                    }
                ));
                translate_expr(out, sink, lscope, etype, body)?;
                sink.br(break_label);

                // otherwise, set up else for the alternative
                sink.writeln("else");
            }
            if let Some(other) = other {
                translate_expr(out, sink, lscope, etype, other)?;
            } else {
                sink.call("$f___WAC_no_matching_alternatives");
                sink.writeln("unreachable");
            }
            for _ in pairs {
                sink.writeln("end");
            }
            sink.end_block();
        }
        Expr::Return(span, _, ret) => match (lscope.return_type, ret) {
            (None, _) | (Some(ReturnType::NoReturn), _) => {
                return Err(Error::Other {
                    span: span.clone(),
                    message: "Return is not possible in this context".into(),
                })
            }
            (Some(ReturnType::Void), Some(_)) => {
                return Err(Error::Other {
                    span: span.clone(),
                    message: "Cannot return a value in a function returning void".into(),
                })
            }
            (Some(ReturnType::Value(_)), None) => {
                return Err(Error::Other {
                    span: span.clone(),
                    message: "Cannot return void in a function returning value".into(),
                })
            }
            (Some(ReturnType::Void), None) => {
                sink.writeln("br $rt_label_return");
            }
            (Some(ReturnType::Value(return_type)), Some(ret)) => {
                translate_expr(out, sink, lscope, ReturnType::Value(return_type), ret)?;
                sink.writeln("br $rt_label_return");
            }
        },
        Expr::New(span, _, type_, args) => {
            let name = match type_ {
                Type::Record(_) => type_.name(),
                _ => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: "record type".into(),
                        got: format!("{}", type_),
                    })
                }
            };
            let fields = lscope.g.record_fields[&name].clone();
            let len = lscope.g.record_fields[&name].len();
            if len != args.len() {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{} args for {} constructor", len, type_),
                    got: format!("{} args", args.len()),
                });
            }

            // First allocate memory for the record
            sink.i32_const(len as i32);
            sink.call("$f___WAC_record_alloc");

            // copy over field values
            // since we're popping from the stack, there's
            // no need for explicit retains
            for (i, (field, argexpr)) in fields.into_iter().zip(args).enumerate() {
                // compute the memory location that will store this next field
                raw_dup(lscope, sink, WasmType::I32);
                sink.i32_const((8 + 8 * i) as i32);
                sink.writeln("i32.add");

                translate_expr(out, sink, lscope, ReturnType::Value(field.1), argexpr)?;

                // we need to cast to 'id' because each field is actually stored as an id value
                auto_cast(
                    sink,
                    span,
                    lscope,
                    ReturnType::Value(field.1),
                    ReturnType::Value(Type::Id),
                )?;

                sink.writeln("i64.store");
            }

            auto_cast(sink, span, lscope, ReturnType::Value(*type_), etype)?;
        }
        Expr::Binop(span, _, op, left, right) => {
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
                    translate_fcall(
                        out,
                        lscope,
                        sink,
                        etype,
                        span,
                        &"Eq".into(),
                        &vec![left, right],
                    )?;
                }
                Binop::NotEqual => {
                    translate_fcall(
                        out,
                        lscope,
                        sink,
                        ReturnType::Value(Type::Bool),
                        span,
                        &"Eq".into(),
                        &vec![left, right],
                    )?;
                    sink.writeln("i32.eqz");
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
                }
                Binop::Is | Binop::IsNot => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;

                    // for is/is-not comparisons, we don't want to wash away the types
                    // by doing automatic conversions (e.g. between int/float)
                    // so the only conversion we potentially do here, is to cast both to
                    // 'id' if the two types are not exactly the same
                    let union_type = if ltype == rtype { ltype } else { Type::Id };

                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                    release(lscope, sink, union_type, DropPolicy::Keep);
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                    release(lscope, sink, union_type, DropPolicy::Keep);
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

                        _ => panic!("impossible eq op {:?}, {}", op, union_type),
                    };
                    sink.writeln(code);
                    auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?;
                }
                Binop::Less | Binop::LessOrEqual | Binop::Greater | Binop::GreaterOrEqual => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;

                    let union_type = best_union_type(ltype, rtype);

                    if union_type.builtin_primitive() {
                        translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                        translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                        let code = match (op, union_type) {
                            (Binop::Less, Type::I32) => "i32.lt_s",
                            (Binop::Less, Type::F32) => "f32.lt",
                            (Binop::Less, Type::I64) => "i64.lt_s",
                            (Binop::Less, Type::F64) => "f64.lt",

                            (Binop::LessOrEqual, Type::I32) => "i32.le_s",
                            (Binop::LessOrEqual, Type::F32) => "f32.le",
                            (Binop::LessOrEqual, Type::I64) => "i64.le_s",
                            (Binop::LessOrEqual, Type::F64) => "f64.le",

                            (Binop::Greater, Type::I32) => "i32.gt_s",
                            (Binop::Greater, Type::F32) => "f32.gt",
                            (Binop::Greater, Type::I64) => "i64.gt_s",
                            (Binop::Greater, Type::F64) => "f64.gt",

                            (Binop::GreaterOrEqual, Type::I32) => "i32.ge_s",
                            (Binop::GreaterOrEqual, Type::F32) => "f32.ge",
                            (Binop::GreaterOrEqual, Type::I64) => "i64.ge_s",
                            (Binop::GreaterOrEqual, Type::F64) => "f64.ge",

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
                    } else {
                        let opname = match op {
                            Binop::Less => "LessThan",
                            Binop::LessOrEqual => "LessThanOrEqual",
                            Binop::Greater => "GreaterThan",
                            Binop::GreaterOrEqual => "GreaterThanOrEqual",
                            _ => panic!("impossible comp op: {:?}", op),
                        };
                        translate_fcall(
                            out,
                            lscope,
                            sink,
                            etype,
                            span,
                            &opname.into(),
                            &vec![left, right],
                        )?;
                    }
                }
                Binop::Add | Binop::Subtract | Binop::Multiply | Binop::Remainder => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;

                    let union_type = best_union_type(ltype, rtype);

                    if union_type.builtin_primitive() {
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

                            (Binop::Add, Type::I64) => "i64.add",
                            (Binop::Add, Type::F64) => "f64.add",

                            (Binop::Subtract, Type::I64) => "i64.sub",
                            (Binop::Subtract, Type::F64) => "f64.sub",

                            (Binop::Multiply, Type::I64) => "i64.mul",
                            (Binop::Multiply, Type::F64) => "f64.mul",

                            (Binop::Remainder, Type::I64) => "i64.rem_s",
                            (Binop::Remainder, Type::F64) => panic!("f64 % not yet implemented"),

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
                    } else {
                        let opname = match op {
                            Binop::Add => "Add",
                            Binop::Subtract => "Subtract",
                            Binop::Multiply => "Multiply",
                            Binop::Remainder => "Remainder",
                            _ => panic!("impossible binop: {:?}", op),
                        };
                        translate_fcall(
                            out,
                            lscope,
                            sink,
                            etype,
                            span,
                            &opname.into(),
                            &vec![left, right],
                        )?;
                    }
                }
                Binop::Divide => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    match (ltype, rtype) {
                        (Type::I32, Type::I32)
                        | (Type::F32, Type::F32)
                        | (Type::F32, Type::I32)
                        | (Type::I32, Type::F32) => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), right)?;
                            sink.writeln("f32.div");
                            auto_cast(sink, span, lscope, ReturnType::Value(Type::F32), etype)?;
                        }
                        (Type::I64, Type::I64)
                        | (Type::F64, Type::F64)
                        | (Type::F64, Type::I64)
                        | (Type::I64, Type::F64) => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F64), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F64), right)?;
                            sink.writeln("f64.div");
                            auto_cast(sink, span, lscope, ReturnType::Value(Type::F64), etype)?;
                        }
                        _ => {
                            translate_fcall(
                                out,
                                lscope,
                                sink,
                                etype,
                                span,
                                &"Divide".into(),
                                &vec![left, right],
                            )?;
                        }
                    }
                }
                Binop::TruncDivide => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    match (ltype, rtype) {
                        (Type::I32, Type::I32) => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), right)?;
                            sink.writeln("i32.div_s");
                            auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
                        }
                        (Type::F32, Type::F32)
                        | (Type::I32, Type::F32)
                        | (Type::F32, Type::I32) => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), right)?;
                            sink.writeln("f32.div");
                            sink.writeln("i32.trunc_f32_s");
                            auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
                        }

                        (Type::I64, Type::I64) => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::I64), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::I64), right)?;
                            sink.writeln("i64.div_s");
                            auto_cast(sink, span, lscope, ReturnType::Value(Type::I64), etype)?;
                        }
                        (Type::F64, Type::F64)
                        | (Type::I64, Type::F64)
                        | (Type::F64, Type::I64) => {
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F64), left)?;
                            translate_expr(out, sink, lscope, ReturnType::Value(Type::F64), right)?;
                            sink.writeln("f64.div");
                            sink.writeln("i64.trunc_f64_s");
                            auto_cast(sink, span, lscope, ReturnType::Value(Type::I64), etype)?;
                        }
                        _ => {
                            translate_fcall(
                                out,
                                lscope,
                                sink,
                                etype,
                                span,
                                &"Divide".into(),
                                &vec![left, right],
                            )?;
                        }
                    }
                }
                Binop::BitwiseAnd
                | Binop::BitwiseOr
                | Binop::BitwiseXor
                | Binop::ShiftLeft
                | Binop::ShiftRight => {
                    let ltype = guess_type(lscope, left)?;
                    let rtype = guess_type(lscope, right)?;
                    let union_type = match (ltype, rtype) {
                        (Type::I32, Type::I32) => Type::I32,
                        (Type::I64, Type::I64) => Type::I64,
                        _ => {
                            return Err(Error::Type {
                                span: expr.span().clone(),
                                expected: "bitwise operands (i32xi32 or i64xi64)".into(),
                                got: format!("{}, {}", ltype, rtype),
                            })
                        }
                    };
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), left)?;
                    translate_expr(out, sink, lscope, ReturnType::Value(union_type), right)?;
                    let code = match op {
                        Binop::BitwiseAnd => format!("{}.and", union_type),
                        Binop::BitwiseOr => format!("{}.or", union_type),
                        Binop::BitwiseXor => format!("{}.xor", union_type),
                        Binop::ShiftLeft => format!("{}.shl", union_type),
                        Binop::ShiftRight => format!("{}.shr_u", union_type),
                        _ => panic!("impossible bitwise op {:?}", op),
                    };
                    sink.writeln(code);
                    auto_cast(sink, span, lscope, ReturnType::Value(union_type), etype)?;
                }
            }
        }
        Expr::Unop(span, _, op, expr) => match op {
            // == unops ==
            // sign ops
            //   +, -
            //     * returns i32 or f32 to match arg type
            // logical
            //   !
            //     * always returns bool
            Unop::Plus => {
                let guessed_type = match guess_type(lscope, expr)? {
                    Type::I32 => Type::I32,
                    _ => Type::F32,
                };
                translate_expr(out, sink, lscope, ReturnType::Value(guessed_type), expr)?;
                auto_cast(sink, span, lscope, ReturnType::Value(guessed_type), etype)?
            }
            Unop::Minus => match guess_type(lscope, expr)? {
                Type::I32 => {
                    let guessed_type = Type::I32;
                    sink.i32_const(0);
                    translate_expr(out, sink, lscope, ReturnType::Value(guessed_type), expr)?;
                    sink.i32_sub();
                    auto_cast(sink, span, lscope, ReturnType::Value(guessed_type), etype)?
                }
                _ => {
                    let guessed_type = Type::F32;
                    translate_expr(out, sink, lscope, ReturnType::Value(guessed_type), expr)?;
                    sink.writeln("f32.neg");
                    auto_cast(sink, span, lscope, ReturnType::Value(guessed_type), etype)?
                }
            },
            Unop::Not => {
                translate_expr(out, sink, lscope, ReturnType::Value(Type::Bool), expr)?;
                sink.writeln("i32.eqz");
                auto_cast(sink, span, lscope, ReturnType::Value(Type::Bool), etype)?
            }
        },
        Expr::AscribeType(span, _, expr, type_) => {
            translate_expr(out, sink, lscope, ReturnType::Value(*type_), expr)?;
            explicit_cast(sink, span, lscope, ReturnType::Value(*type_), etype)?
        }
        Expr::CString(span, _, value) => {
            let ptr = out.intern_cstr(value);
            sink.i32_const(ptr);
            auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?
        }
        Expr::Asm(span, _, args, type_, asm_code) => {
            for arg in args {
                let argtype = guess_type(lscope, arg)?;
                translate_expr(out, sink, lscope, ReturnType::Value(argtype), arg)?;
            }
            sink.writeln(asm_code);
            auto_cast(sink, span, lscope, *type_, etype)?;
        }
        Expr::Read1(_, _, subexpr, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), subexpr)?;
            sink.write("i32.load8_u");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Read2(_, _, subexpr, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), subexpr)?;
            sink.write("i32.load16_u");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Read4(_, _, subexpr, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), subexpr)?;
            sink.write("i32.load");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Read8(_, _, subexpr, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), subexpr)?;
            sink.write("i64.load");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Write1(_, _, addr, val, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), addr)?;
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), val)?;
            sink.write("i32.store8");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Write2(_, _, addr, val, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), addr)?;
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), val)?;
            sink.write("i32.store16");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Write4(_, _, addr, val, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), addr)?;
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), val)?;
            sink.write("i32.store");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
        }
        Expr::Write8(_, _, addr, val, offset) => {
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), addr)?;
            translate_expr(out, sink, lscope, ReturnType::Value(Type::I64), val)?;
            sink.write("i64.store");
            if *offset > 0 {
                sink.write(format!(" offset={}", offset));
            }
            sink.writeln("");
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
    sink.local_tee(&tmpvar);
    sink.local_get(&tmpvar);
}

/// adds opcodes to convert an i32 type to an 'id'
pub(super) fn cast_to_id(sink: &Rc<Sink>, tag: i32) {
    sink.i64_extend_i32_u();
    sink.i64_const(tag as i64);
    sink.i64_const(32);
    sink.writeln("i64.shl");
    sink.writeln("i64.or");
}

/// translate a constval whose result is a primitive i32 wasm type
/// (i.e. i32, enum or type)
pub(super) fn translate_constval_i32(
    _out: &mut Out,
    sink: &Rc<Sink>,
    _lscope: &mut LocalScope,
    cval: &ConstValue,
) -> Result<(), Error> {
    match cval {
        ConstValue::I32(value) => sink.i32_const(*value),
        ConstValue::Type(type_) => sink.i32_const(type_.tag()),
        ConstValue::Enum(_, value) => sink.i32_const(*value),
    }
    Ok(())
}
