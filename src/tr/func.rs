use super::*;

pub(super) fn translate_import(out: &Out, imp: Import) {
    match imp {
        Import::Function(FunctionImport {
            span: _,
            module_name,
            function_name,
            alias,
            type_,
        }) => {
            out.imports.writeln(format!(
                r#"(import "{}" "{}" (func $f_{} {}))"#,
                module_name,
                function_name,
                alias,
                translate_func_type(&type_),
            ));
        }
    }
}

pub(super) fn translate_func(out: &mut Out, gscope: &GlobalScope, func: Function) -> Result<(), Error> {
    let mut lscope = LocalScope::new(gscope, func.type_.trace);

    match func.visibility {
        Visibility::Public => {
            out.exports.writeln(format!(
                r#"(export "f_{}" (func $f_{}))"#,
                func.name, func.name
            ));
        }
        Visibility::Private => {}
    }

    let sink = out.funcs.spawn();
    sink.writeln(format!("(func $f_{}", func.name));

    for parameter in &func.type_.parameters {
        let info = lscope.decl(func.span.clone(), parameter.0.clone(), parameter.1);
        sink.writeln(format!(
            " (param {} {})",
            info.wasm_name,
            translate_type(info.type_)
        ));
    }
    match func.type_.return_type {
        ReturnType::Value(return_type) => {
            sink.writeln(format!(" (result {})", translate_type(return_type)));
        }
        ReturnType::NoReturn | ReturnType::Void => {}
    }
    // we won't know what locals we have until we finish translate_expr on the body
    let locals_sink = sink.spawn();
    let locals_init = sink.spawn();
    translate_expr(out, &sink, &mut lscope, func.type_.return_type, &func.body)?;
    let epilogue = sink.spawn();
    sink.writeln(")");

    // special local variables used by some operations
    // temporary variable for duplicating values on TOS
    let mut helper_locals: Vec<(_, _)> = lscope.helper_locals.into_iter().collect();
    helper_locals.sort_by(|a, b| a.0.cmp(&b.0));
    for (wasm_name, type_) in helper_locals {
        locals_sink.writeln(format!("(local {} {})", wasm_name, translate_type(type_)));
        release_var(&epilogue, Scope::Local, &wasm_name, type_);
    }

    // declare all the local variables (skipping parameters)
    for info in lscope.decls.iter().skip(func.type_.parameters.len()) {
        locals_sink.writeln(format!(
            " (local {} {})",
            info.wasm_name,
            translate_type(info.type_)
        ));
        locals_init.writeln(format!(
            "(local.set {} ({}.const 0))",
            info.wasm_name,
            translate_type(info.type_)
        ));
    }

    // Make sure to release all local variables, even the parameters
    for info in lscope.decls {
        release_var(&epilogue, Scope::Local, &info.wasm_name, info.type_);
    }
    Ok(())
}
