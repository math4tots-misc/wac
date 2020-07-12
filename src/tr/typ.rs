use super::*;

pub(super) fn translate_func_type(ft: &FunctionType) -> String {
    let mut ret = String::new();
    let FunctionType {
        return_type,
        parameters,
        trace: _,
    } = ft;
    for (_name, ptype) in parameters {
        ret.push_str(&format!(" (param {})", translate_type(*ptype)));
    }
    match return_type {
        ReturnType::Value(rt) => {
            ret.push_str(&format!(" (result {})", translate_type(*rt)));
        }
        ReturnType::Void | ReturnType::NoReturn => {}
    }
    ret
}

pub(super) fn translate_type(t: Type) -> &'static str {
    translate_wasm_type(t.wasm())
}

pub(super) fn translate_wasm_type(wt: WasmType) -> &'static str {
    match wt {
        WasmType::I32 => "i32",
        WasmType::I64 => "i64",
        WasmType::F32 => "f32",
        WasmType::F64 => "f64",
    }
}

/// perform a cast of TOS from src to dst for when implicitly needed
pub(super) fn auto_cast(
    sink: &Rc<Sink>,
    span: &SSpan,
    lscope: &mut LocalScope,
    src: ReturnType,
    dst: ReturnType,
) -> Result<(), Error> {
    match (src, dst) {
        (ReturnType::Value(src), ReturnType::Value(dst)) if src == dst => {}
        (ReturnType::Void, ReturnType::Void) => {}
        (ReturnType::NoReturn, ReturnType::NoReturn) => {}
        (ReturnType::NoReturn, _) => {
            // if we're ever provided with a noreturn,
            // there's nothing really to do except let wasm know
            sink.unreachable();
        }
        (ReturnType::Value(Type::I32), ReturnType::Value(Type::F32)) => {
            sink.f32_convert_i32_s();
        }
        (ReturnType::Value(src), ReturnType::Value(Type::Id)) => match src {
            Type::Id => {}
            Type::I32 => {
                cast_to_id(sink, TAG_I32);
            }
            Type::F32 => {
                sink.i32_reinterpret_f32();
                cast_to_id(sink, TAG_F32);
            }
            Type::Bool => {
                cast_to_id(sink, TAG_BOOL);
            }
            Type::Type => {
                cast_to_id(sink, TAG_TYPE);
            }
            Type::Bytes => {
                cast_to_id(sink, TAG_BYTES);
            }
            Type::String => {
                cast_to_id(sink, TAG_STRING);
            }
            Type::List => {
                cast_to_id(sink, TAG_LIST);
            }
            Type::I64 | Type::F64 => {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{:?}", dst),
                    got: format!("{:?} (id cannot store 64-bit values)", src),
                });
            }
            Type::Enum(_) => {
                cast_to_id(sink, src.tag());
            }
            Type::Record(_) => {
                cast_to_id(sink, src.tag());
            }
        },
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::I32)) => {
            sink.call("$f___WAC_raw_id_to_i32");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::F32)) => {
            sink.call("$f___WAC_raw_id_to_f32");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::Bool)) => {
            sink.call("$f___WAC_raw_id_to_bool");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::Type)) => {
            sink.call("$f___WAC_raw_id_to_type");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::Bytes)) => {
            sink.call("$f___WAC_raw_id_to_bytes");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::String)) => {
            sink.call("$f___WAC_raw_id_to_str");
        }
        (ReturnType::Value(Type::Id), ReturnType::Value(Type::List)) => {
            sink.call("$f___WAC_raw_id_to_list");
        }
        (ReturnType::Value(src), ReturnType::Void) => {
            release(lscope, sink, src, DropPolicy::Drop);
        }
        (ReturnType::Value(src), ReturnType::Value(dst)) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: format!("{:?}", dst),
                got: format!("{:?}", src),
            });
        }
        (ReturnType::Value(src), ReturnType::NoReturn) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: "noreturn".into(),
                got: format!("{:?}", src),
            });
        }
        (ReturnType::Void, ReturnType::NoReturn) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: "noreturn".into(),
                got: "void".into(),
            });
        }
        (ReturnType::Void, ReturnType::Value(dst)) => {
            return Err(Error::Type {
                span: span.clone(),
                expected: format!("{:?}", dst),
                got: "void".into(),
            });
        }
    }
    Ok(())
}

/// perform a cast of TOS from src to dst for when explicitly requested
/// "stronger" than auto_cast
pub(super) fn explicit_cast(
    sink: &Rc<Sink>,
    span: &SSpan,
    lscope: &mut LocalScope,
    src: ReturnType,
    dst: ReturnType,
) -> Result<(), Error> {
    match (src, dst) {
        (ReturnType::Value(Type::F32), ReturnType::Value(Type::I32)) => {
            sink.i32_trunc_f32_s();
        }
        _ => auto_cast(sink, span, lscope, src, dst)?,
    }
    Ok(())
}

// return the best fitting type that fits the union of the two return types
pub(super) fn best_union_return_type(a: ReturnType, b: ReturnType) -> ReturnType {
    match (a, b) {
        // If we see Void anywhere, the overall return must be void
        (ReturnType::Void, _) | (_, ReturnType::Void) => ReturnType::Void,

        // NoReturn is a recessive, if we see one, always return the other type
        (ReturnType::NoReturn, _) => b,
        (_, ReturnType::NoReturn) => a,

        (ReturnType::Value(a), ReturnType::Value(b)) => ReturnType::Value(best_union_type(a, b)),
    }
}

pub(super) fn best_union_type(a: Type, b: Type) -> Type {
    match (a, b) {
        _ if a == b => a,

        // special case for int/floats -- ints can be used as floats
        // if needed
        (Type::I32, Type::F32) | (Type::F32, Type::I32) => Type::F32,

        // in all other cases, just use the id type
        _ => Type::Id,
    }
}
