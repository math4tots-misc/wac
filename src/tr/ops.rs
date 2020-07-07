//! binary and unary operator helpers
//! TODO: factor this hairy mess
use super::*;

pub(super) type BinopHandler = fn(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error>;

pub(super) struct BinopCase {
    pub(super) lhs: Type,
    pub(super) rhs: Type,
    pub(super) type_: Type,
    pub(super) handler: BinopHandler,
}

pub(super) fn cases_for_binop(op: Binop) -> &'static [BinopCase] {
    match op {
        Binop::Add => BINOP_ADD_CASES,
        Binop::Subtract => BINOP_SUBTRACT_CASES,
        Binop::Multiply => BINOP_MULTIPLY_CASES,
        Binop::Divide => BINOP_DIVIDE_CASES,
        Binop::TruncDivide => BINOP_TRUNC_DIVIDE_CASES,
        Binop::Remainder => BINOP_REMAINDER_CASES,
        _ => panic!("TODO cases_for_binop {:?}", op),
    }
}

pub(super) const BINOP_ADD_CASES: &'static [BinopCase] = &[
    // i32 + i32
    BinopCase {
        lhs: Type::I32,
        rhs: Type::I32,
        type_: Type::I32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::I32,
                Type::I32,
                etype,
                span,
                "add",
                left,
                right,
            )
        },
    },

    // f32 + i32
    BinopCase {
        lhs: Type::F32,
        rhs: Type::I32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "add",
                left,
                right,
            )
        },
    },

    // i32 + f32
    BinopCase {
        lhs: Type::I32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "add",
                left,
                right,
            )
        },
    },

    // f32 + f32
    BinopCase {
        lhs: Type::F32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "add",
                left,
                right,
            )
        },
    },
];

pub(super) const BINOP_SUBTRACT_CASES: &'static [BinopCase] = &[
    BinopCase {
        lhs: Type::I32,
        rhs: Type::I32,
        type_: Type::I32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::I32,
                Type::I32,
                etype,
                span,
                "sub",
                left,
                right,
            )
        },
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::I32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "sub",
                left,
                right,
            )
        },
    },
    BinopCase {
        lhs: Type::I32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "sub",
                left,
                right,
            )
        },
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "sub",
                left,
                right,
            )
        },
    },
];

pub(super) const BINOP_MULTIPLY_CASES: &'static [BinopCase] = &[
    BinopCase {
        lhs: Type::I32,
        rhs: Type::I32,
        type_: Type::I32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::I32,
                Type::I32,
                etype,
                span,
                "mul",
                left,
                right,
            )
        },
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::I32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "mul",
                left,
                right,
            )
        },
    },
    BinopCase {
        lhs: Type::I32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "mul",
                left,
                right,
            )
        },
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: |out, sink, lscope, etype, span, left, right| {
            arith_binop(
                out,
                sink,
                lscope,
                Type::F32,
                Type::F32,
                etype,
                span,
                "mul",
                left,
                right,
            )
        },
    },
];

pub(super) const BINOP_DIVIDE_CASES: &'static [BinopCase] = &[
    BinopCase {
        lhs: Type::I32,
        rhs: Type::I32,
        type_: Type::F32,
        handler: divide_handler,
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::I32,
        type_: Type::F32,
        handler: divide_handler,
    },
    BinopCase {
        lhs: Type::I32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: divide_handler,
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: divide_handler,
    },
];

pub(super) const BINOP_TRUNC_DIVIDE_CASES: &'static [BinopCase] = &[
    BinopCase {
        lhs: Type::I32,
        rhs: Type::I32,
        type_: Type::I32,
        handler: i32_trunc_divide_handler,
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::I32,
        type_: Type::F32,
        handler: f32_trunc_divide_handler,
    },
    BinopCase {
        lhs: Type::I32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: f32_trunc_divide_handler,
    },
    BinopCase {
        lhs: Type::F32,
        rhs: Type::F32,
        type_: Type::F32,
        handler: f32_trunc_divide_handler,
    },
];

pub(super) const BINOP_REMAINDER_CASES: &'static [BinopCase] = &[
    BinopCase {
        lhs: Type::I32,
        rhs: Type::I32,
        type_: Type::I32,
        handler: i32_remainder_handler,
    },
];

fn divide_handler(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), right)?;
    sink.writeln("f32.div");
    auto_cast(sink, span, lscope, ReturnType::Value(Type::F32), etype)?;
    Ok(())
}

fn i32_trunc_divide_handler(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), right)?;
    sink.writeln("i32.div_s");
    auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
    Ok(())
}

fn f32_trunc_divide_handler(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(Type::F32), right)?;
    sink.writeln("f32.div");
    explicit_cast(
        sink,
        span,
        lscope,
        ReturnType::Value(Type::F32),
        ReturnType::Value(Type::I32),
    )?;
    auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
    Ok(())
}

fn i32_remainder_handler(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    etype: ReturnType,
    span: &SSpan,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(Type::I32), right)?;
    sink.writeln("i32.rem_s");
    auto_cast(sink, span, lscope, ReturnType::Value(Type::I32), etype)?;
    Ok(())
}

/// util for binary arithmetic operators (e.g. Add, Subtract, etc)
///   * both arguments are always same type
///   * always returns argument type
///   * not split by sign
fn arith_binop(
    out: &mut Out,
    sink: &Rc<Sink>,
    lscope: &mut LocalScope,
    argtype: Type,
    rtype: Type,
    etype: ReturnType,
    span: &SSpan,
    opname: &str,
    left: &Expr,
    right: &Expr,
) -> Result<(), Error> {
    translate_expr(out, sink, lscope, ReturnType::Value(argtype), left)?;
    translate_expr(out, sink, lscope, ReturnType::Value(argtype), right)?;
    sink.writeln(format!("{}.{}", translate_type(argtype), opname));
    auto_cast(sink, span, lscope, ReturnType::Value(rtype), etype)?;
    Ok(())
}
