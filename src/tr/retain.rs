use super::*;

pub(super) enum DropPolicy {
    Drop,
    Keep,
}

/// retains the TOS value given the provided type
/// the drop parameter determines if the value will be consumed/dropped or not
pub(super) fn retain(lscope: &mut LocalScope, sink: &Rc<Sink>, type_: Type, dp: DropPolicy) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Type | Type::Enum(_) => {
            match dp {
                DropPolicy::Drop => sink.drop_(),
                DropPolicy::Keep => {}
            }
        }
        Type::String | Type::List => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.call("$f___WAC_ptr_retain");
        }
        Type::Id => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I64),
            }
            sink.call("$f___WAC_id_retain");
        }
        Type::Record(_) => panic!("TODO: retain record"),
    }
}

/// drops the TOS given that TOS is the provided type
/// the drop parameter determines if the value will be consumed/dropped or not
pub(super) fn release(lscope: &mut LocalScope, sink: &Rc<Sink>, type_: Type, dp: DropPolicy) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Type | Type::Enum(_) => {
            match dp {
                DropPolicy::Drop => sink.drop_(),
                DropPolicy::Keep => {}
            }
        }
        Type::String => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.call("$f___WAC_str_release");
        }
        Type::List => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I32),
            }
            sink.call("$f___WAC_list_release");
        }
        Type::Id => {
            match dp {
                DropPolicy::Drop => {}
                DropPolicy::Keep => raw_dup(lscope, sink, WasmType::I64),
            }
            sink.call("$f___WAC_id_release");
        }
        Type::Record(_) => panic!("TODO: release record"),
    }
}

/// releases a reference in a var
/// overall, should leave the stack unchanged
pub(super) fn release_var(sink: &Rc<Sink>, scope: Scope, wasm_name: &Rc<str>, type_: Type) {
    match type_ {
        Type::Bool | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Type | Type::Enum(_) => {
        }
        Type::String => {
            sink.var_get(scope, wasm_name);
            sink.call("$f___WAC_str_release");
        }
        Type::List => {
            sink.var_get(scope, wasm_name);
            sink.call("$f___WAC_list_release");
        }
        Type::Id => {
            sink.var_get(scope, wasm_name);
            sink.call("$f___WAC_id_release");
        }
        Type::Record(_) => panic!("TODO: release_var record"),
    }
}
