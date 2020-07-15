use super::*;

pub(super) fn translate_fcall(
    out: &mut Out,
    lscope: &mut LocalScope,
    sink: &Rc<Sink>,
    etype: ReturnType,
    span: &SSpan,
    fname: &Rc<str>,
    argexprs: &Vec<&Expr>,
) -> Result<(), Error> {
    let fentry = lscope.getf_or_err(span, fname)?;
    let ftype = fentry.type_();
    let trace = ftype.trace;
    if argexprs.len() != ftype.parameters.len() {
        return Err(Error::Type {
            span: span.clone(),
            expected: format!("{} arg(s)", ftype.parameters.len()),
            got: format!("{} arg(s)", argexprs.len()),
        });
    }

    let mut trait_type_var = None;
    let mut trait_receiver_type = None;

    for (i, (argexpr, (_pname, ptype))) in argexprs.iter().zip(&ftype.parameters).enumerate() {
        translate_expr(out, sink, lscope, ReturnType::Value(*ptype), argexpr)?;

        if i == 0 {
            match &fentry {
                FunctionEntry::Trait(_) => {
                    // we want to check the receiver type, to see if we can find the
                    // actual function directly
                    let receiver_type = guess_type(lscope, argexpr)?;

                    if let Type::Id = receiver_type {
                        // for id receivers, we need to save the dynamic type of the first argument
                        // for the actual call later
                        assert_eq!(*ptype, Type::Id);

                        let varname = lscope.helper_unique("tcall", Type::I32);

                        raw_dup(lscope, sink, WasmType::I64);

                        sink.writeln("i64.const 32");
                        sink.writeln("i64.shr_u");
                        sink.writeln("i32.wrap_i64");
                        sink.writeln(format!("local.set {}", varname));

                        trait_type_var = Some(varname);
                    } else {
                        // in this case, the receiver_type is not 'id', so we can
                        // retrieve it directly
                        //
                        // NOTE: we still pass the argument as an 'id'.
                        trait_receiver_type = Some(receiver_type);
                    }
                }
                FunctionEntry::Function(_) => {}
            }
        }
    }

    if trace && !lscope.trace {
        return Err(Error::Type {
            span: span.clone(),
            expected: "notrace function can only call other notrace functions".into(),
            got: "traced function".into(),
        });
    }

    if TRACE_MODE.check(trace) {
        let ptr = out.intern_cstr(&span.source.name);
        let lineno = span.lineno() as i32;
        sink.writeln(format!("i32.const {}", ptr));
        sink.writeln(format!("i32.const {}", lineno));
        sink.writeln(format!("call $rt_stack_push"));
    }

    match &fentry {
        FunctionEntry::Function(info) => {
            sink.writeln(format!("call $f_{}", info.name));
        }
        FunctionEntry::Trait(info) => {
            match trait_receiver_type {
                Some(receiver_type) => {
                    // if we get here, it means we could determine
                    // the actual receiver type
                    match lscope.g.get_impl(receiver_type, info.id) {
                        Some(impl_info) => {
                            sink.writeln(format!("call $f_{}", impl_info.fname));
                        }
                        None => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{} trait impl", info.name),
                                got: format!("no matching impl found for {}", receiver_type),
                            })
                        }
                    }
                }
                None => {
                    let trait_type_var = trait_type_var.unwrap();
                    sink.writeln(format!("local.get {}", trait_type_var));
                    sink.writeln(format!("i32.const {}", info.id));
                    sink.writeln("call $f___WAC_find_funcptr");
                    sink.writeln(format!(
                        "call_indirect {}",
                        translate_func_type(&info.type_)
                    ));
                }
            }
        }
    }

    if TRACE_MODE.check(trace) {
        // pop stack pointer
        sink.writeln("call $rt_stack_pop");
    }

    auto_cast(sink, span, lscope, ftype.return_type, etype)?;
    Ok(())
}
