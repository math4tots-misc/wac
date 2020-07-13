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

pub(super) fn translate_impl(
    out: &mut Out,
    gscope: &mut GlobalScope,
    imp: Impl,
) -> Result<(), Error> {
    let trait_info = gscope.get_trait(&imp.span, &imp.trait_name)?.clone();
    let impl_info = gscope.get_impl(imp.receiver_type, trait_info.id).unwrap();

    // check that the impl type matches the trait type exactly
    if trait_info.type_ != imp.type_ {
        return Err(Error::Type {
            span: imp.span.clone(),
            expected: format!("impl type {}", trait_info.type_),
            got: format!("impl type {}", imp.type_),
        });
    }

    // create the function associated with this impl
    // However, we make some modifications:
    //   * the name should be something private and unique
    //   * the first parameter is an 'id', but from the point of view of
    //       the user, we want it to look like it has already been cast to the
    //       receiver type.
    //       So rename the first parameter to __WAC_self, then assign self to it
    let fname = &impl_info.fname;
    let mut ftype = imp.type_.clone();
    assert_eq!(ftype.parameters[0].0.as_ref(), "self");
    assert_eq!(ftype.parameters[0].1, Type::Id);
    ftype.parameters[0].0 = "__WAC_self".into();
    let body = Expr::Block(
        imp.span.clone(),
        vec![
            Expr::DeclVar(
                imp.span.clone(),
                "self".into(),
                Some(imp.receiver_type),
                Expr::GetVar(imp.span.clone(), "__WAC_self".into()).into(),
            ),
            imp.body,
        ],
    );
    translate_func_from_parts(
        out,
        gscope,
        Visibility::Private,
        &imp.span,
        &fname,
        &ftype,
        &body,
    )?;

    Ok(())
}

pub(super) fn translate_func(
    out: &mut Out,
    gscope: &mut GlobalScope,
    func: Function,
) -> Result<(), Error> {
    translate_func_from_parts(
        out,
        gscope,
        func.visibility,
        &func.span,
        &func.name,
        &func.type_,
        &func.body,
    )
}

pub(super) fn translate_func_from_parts(
    out: &mut Out,
    gscope: &mut GlobalScope,
    visibility: Visibility,
    fspan: &SSpan,
    fname: &Rc<str>,
    ftype: &FunctionType,
    body: &Expr,
) -> Result<(), Error> {
    let mut lscope = LocalScope::new(gscope, ftype.trace);

    match visibility {
        Visibility::Public => {
            out.exports
                .writeln(format!(r#"(export "f_{}" (func $f_{}))"#, fname, fname));
        }
        Visibility::Private => {}
    }

    let sink = out.funcs.spawn();
    sink.writeln(format!("(func $f_{}", fname));

    for parameter in &ftype.parameters {
        let info = lscope.decl(fspan.clone(), parameter.0.clone(), parameter.1);
        sink.writeln(format!(
            " (param {} {})",
            info.wasm_name,
            translate_type(info.type_)
        ));
    }
    match ftype.return_type {
        ReturnType::Value(return_type) => {
            sink.writeln(format!(" (result {})", translate_type(return_type)));
        }
        ReturnType::NoReturn | ReturnType::Void => {}
    }
    // we won't know what locals we have until we finish translate_expr on the body
    let locals_sink = sink.spawn();
    translate_expr(out, &sink, &mut lscope, ftype.return_type, &body)?;
    let epilogue = sink.spawn();
    sink.writeln(")");

    // special local variables used by some operations
    // temporary variable for duplicating values on TOS
    let mut helper_locals: Vec<(_, _)> = lscope.helper_locals.into_iter().collect();
    helper_locals.sort_by(|a, b| a.0.cmp(&b.0));
    for (wasm_name, type_) in helper_locals {
        assert!(type_.primitive());
        locals_sink.writeln(format!("(local {} {})", wasm_name, translate_type(type_)));
        release_var(&epilogue, Scope::Local, &wasm_name, type_);
    }

    // declare all the local variables (skipping parameters)
    for info in lscope.decls.iter().skip(ftype.parameters.len()) {
        locals_sink.writeln(format!(
            " (local {} {})",
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
