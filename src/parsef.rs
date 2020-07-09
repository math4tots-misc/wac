//! Parse functions/grammars that build on top of parser.rs
use crate::ir::*;
use crate::ParseError;
use crate::Parser;
use crate::Pattern;
use crate::Token;
use std::collections::HashMap;
use std::rc::Rc;

const PREC_POSTFIX: u32 = 1000;
const PREC_UNARY: u32 = 900;
const PREC_PRODUCT: u32 = 600;
const PREC_SUM: u32 = 500;
const PREC_SHIFT: u32 = 400;
const PREC_BITWISE_AND: u32 = 300;
const PREC_BITWISE_XOR: u32 = 275;
const PREC_BITWISE_OR: u32 = 250;
const PREC_CMP: u32 = 200;
const PREC_LOGICAL_AND: u32 = 150;
const PREC_LOGICAL_OR: u32 = 140;
const PREC_ASSIGN: u32 = 100;

pub fn parse_file(parser: &mut Parser) -> Result<File, ParseError> {
    let mut imports = Vec::new();
    consume_delim(parser);
    while parser.at_name("import") {
        let span = parser.span();
        parser.gettok();
        parser.expect(Token::Name("fn"))?;
        let module_name = parser.expect_string()?;
        let function_name = parser.expect_string()?;
        let alias = parser.expect_name()?;
        let type_ = parse_function_type(parser, false)?;
        let span = span.upto(&parser.span());
        consume_delim(parser);
        imports.push(Import::Function(FunctionImport {
            span,
            module_name,
            function_name,
            alias,
            type_,
        }));
    }
    let mut globalvars = Vec::new();
    let mut functions = Vec::new();
    let mut traits = Vec::new();
    let mut impls = Vec::new();
    let mut enums = Vec::new();
    let mut records = Vec::new();
    let mut constants = Vec::new();
    let mut constants_map = HashMap::new();
    consume_delim(parser);
    while !parser.at(Token::EOF) {
        match parser.peek() {
            Token::Name("fn") => functions.push(parse_func(parser)?),
            Token::Name("trait") => traits.push(parse_trait(parser)?),
            Token::Name("impl") => impls.push(parse_impl(parser)?),
            Token::Name("enum") => enums.push(parse_enum(parser)?),
            Token::Name("record") => records.push(parse_record(parser)?),
            Token::Name("var") => globalvars.push(parse_globalvar(parser)?),
            Token::Name("const") => constants.push(parse_constant(parser, &mut constants_map)?),
            _ => {
                return Err(ParseError::InvalidToken {
                    span: parser.span(),
                    expected: "Function".into(),
                    got: format!("{:?}", parser.peek()),
                })
            }
        }
        consume_delim(parser);
    }
    Ok(File {
        imports,
        constants,
        functions,
        traits,
        impls,
        enums,
        records,
        globalvars,
    })
}

fn parse_constant(
    parser: &mut Parser,
    map: &mut HashMap<Rc<str>, ConstValue>,
) -> Result<Constant, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("const"))?;
    let name = parser.expect_name()?;
    parser.expect(Token::Eq)?;
    let expr = parse_expr(parser, 0)?;
    let value = eval_constexpr(&expr, map)?;
    let span = span.upto(&parser.span());
    Ok(Constant { span, name, value })
}

fn eval_constexpr(
    expr: &Expr,
    map: &mut HashMap<Rc<str>, ConstValue>,
) -> Result<ConstValue, ParseError> {
    match expr {
        Expr::Int(_, value) => Ok(ConstValue::I32(*value as i32)),
        Expr::GetVar(span, name) => match map.get(name) {
            Some(value) => Ok(value.clone()),
            None => Err(ParseError::InvalidToken {
                span: span.clone(),
                expected: "named constant".into(),
                got: "NotFound".into(),
            }),
        },
        _ => Err(ParseError::InvalidToken {
            span: expr.span().clone(),
            expected: "constexpr".into(),
            got: "non-const expression".into(),
        }),
    }
}

fn parse_func(parser: &mut Parser) -> Result<Function, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("fn"))?;
    let mut visibility = Visibility::Private;
    if parser.consume(Token::LBracket) {
        loop {
            match parser.peek() {
                Token::Name("pub") => {
                    parser.gettok();
                    visibility = Visibility::Public;
                }
                Token::RBracket => {
                    parser.gettok();
                    break;
                }
                _ => {
                    return Err(ParseError::InvalidToken {
                        span,
                        expected: "Function attribute".into(),
                        got: format!("{:?}", parser.peek()),
                    })
                }
            }
        }
    }
    let name = parser.expect_name()?;
    let type_ = parse_function_type(parser, false)?;
    let body = parse_block(parser)?;
    let span = span.upto(&parser.span());
    Ok(Function {
        span,
        visibility,
        name,
        type_,
        body,
    })
}

fn parse_trait(parser: &mut Parser) -> Result<Trait, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("trait"))?;
    let name = parser.expect_name()?;
    let type_ = parse_function_type(parser, true)?;
    let span = span.upto(&parser.span());
    Ok(Trait { span, name, type_ })
}

fn parse_impl(parser: &mut Parser) -> Result<Impl, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("impl"))?;
    let receiver_type = parse_type(parser)?;
    parser.expect(Token::Name("for"))?;
    let trait_name = parser.expect_name()?;
    let type_ = parse_function_type(parser, true)?;
    let body = parse_block(parser)?;
    let span = span.upto(&parser.span());
    Ok(Impl {
        span,
        receiver_type,
        trait_name,
        type_,
        body,
    })
}

fn parse_globalvar(parser: &mut Parser) -> Result<GlobalVariable, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("var"))?;
    let mut visibility = Visibility::Private;
    if parser.consume(Token::LBracket) {
        loop {
            match parser.peek() {
                Token::Name("pub") => {
                    parser.gettok();
                    visibility = Visibility::Public;
                }
                Token::RBracket => {
                    parser.gettok();
                    break;
                }
                _ => {
                    return Err(ParseError::InvalidToken {
                        span,
                        expected: "Global variable attribute".into(),
                        got: format!("{:?}", parser.peek()),
                    })
                }
            }
        }
    }
    let name = parser.expect_name()?;
    let type_ = if parser.at(Pattern::Name) {
        Some(parse_type(parser)?)
    } else {
        None
    };
    parser.expect(Token::Eq)?;
    let init = parse_expr(parser, 0)?;
    Ok(GlobalVariable {
        span,
        visibility,
        name,
        type_,
        init,
    })
}

fn parse_enum(parser: &mut Parser) -> Result<Enum, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("enum"))?;
    let name = parser.expect_name()?;
    let mut members = Vec::new();
    parser.expect(Token::LBrace)?;
    consume_delim(parser);
    while !parser.consume(Token::RBrace) {
        let member_name = parser.expect_name()?;
        members.push(member_name);
        if !parser.consume(Token::Comma) {
            parser.expect(Token::RBrace)?;
            break;
        }
        consume_delim(parser);
    }
    let span = span.upto(&parser.span());

    Ok(Enum {
        span,
        name: name,
        members,
    })
}

fn parse_record(parser: &mut Parser) -> Result<Record, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("record"))?;
    let name = parser.expect_name()?;
    let mut fields = Vec::new();
    parser.expect(Token::LBrace)?;
    consume_delim(parser);
    while !parser.consume(Token::RBrace) {
        let member_name = parser.expect_name()?;
        let type_ = parse_type(parser)?;
        fields.push((member_name, type_));
        if !parser.consume(Token::Comma) {
            parser.expect(Token::RBrace)?;
            break;
        }
        consume_delim(parser);
    }
    let span = span.upto(&parser.span());
    Ok(Record {
        span,
        name: name,
        fields,
    })
}

fn consume_delim(parser: &mut Parser) {
    loop {
        match parser.peek() {
            Token::Newline => {
                parser.gettok();
            }
            _ => break,
        }
    }
}

fn parse_stmt(parser: &mut Parser) -> Result<Expr, ParseError> {
    let expr = parse_expr(parser, 0)?;
    consume_delim(parser);
    Ok(expr)
}

fn parse_expr(parser: &mut Parser, prec: u32) -> Result<Expr, ParseError> {
    let atom = parse_atom(parser)?;
    parse_infix(parser, atom, prec)
}

fn parse_atom(parser: &mut Parser) -> Result<Expr, ParseError> {
    let span = parser.span();
    match parser.peek() {
        Token::Int(x) => {
            parser.gettok();
            Ok(Expr::Int(span, x))
        }
        Token::Float(x) => {
            parser.gettok();
            Ok(Expr::Float(span, x))
        }
        Token::NormalString(_) | Token::RawString(_) => {
            let s = parser.expect_string()?;
            Ok(Expr::String(span, s))
        }
        Token::Name("true") => {
            parser.gettok();
            Ok(Expr::Bool(span, true))
        }
        Token::Name("false") => {
            parser.gettok();
            Ok(Expr::Bool(span, false))
        }
        Token::Name("if") => parse_if(parser),
        Token::Name("while") => parse_while(parser),
        Token::Name("var") => {
            parser.gettok();
            let name = parser.expect_name()?;
            let type_ = if parser.at(Pattern::Name) {
                Some(parse_type(parser)?)
            } else {
                None
            };
            parser.expect(Token::Eq)?;
            let setexpr = parse_expr(parser, 0)?;
            let span = span.upto(&parser.span());
            Ok(Expr::DeclVar(span, name, type_, setexpr.into()))
        }
        Token::Name(name) => {
            parser.gettok();
            Ok(Expr::GetVar(span, name.into()))
        }
        Token::Minus | Token::Plus | Token::Exclamation => {
            let op = match parser.peek() {
                Token::Minus => Unop::Minus,
                Token::Plus => Unop::Plus,
                Token::Exclamation => Unop::Not,
                t => panic!("parse_atom Plus/Minus {:?}", t),
            };
            parser.gettok();
            let expr = parse_expr(parser, PREC_UNARY)?;
            Ok(Expr::Unop(span, op, expr.into()))
        }
        Token::Dollar => {
            parser.gettok();
            match parser.peek() {
                Token::Name("cstr") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let string = parser.expect_string()?;
                    parser.expect(Token::RParen)?;
                    let span = span.upto(&parser.span());
                    Ok(Expr::CString(span, string))
                }
                Token::Name("asm") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    parser.expect(Token::LBracket)?;
                    let mut args = Vec::new();
                    while !parser.consume(Token::RBracket) {
                        args.push(parse_expr(parser, 0)?);
                        if !parser.consume(Token::Comma) {
                            parser.expect(Token::RBracket)?;
                            break;
                        }
                    }
                    parser.expect(Token::Comma)?;
                    let type_ = parse_return_type(parser)?;
                    parser.expect(Token::Comma)?;
                    let asm_code = parser.expect_string()?;
                    parser.consume(Token::Comma);
                    parser.expect(Token::RParen)?;
                    Ok(Expr::Asm(span, args, type_, asm_code))
                }
                _ => Err(ParseError::InvalidToken {
                    span,
                    expected: "intrinsic name".into(),
                    got: format!("{:?}", parser.peek()),
                }),
            }
        }
        Token::LBracket => {
            parser.gettok();
            let mut exprs = Vec::new();
            while !parser.consume(Token::RBracket) {
                exprs.push(parse_expr(parser, 0)?);
                if !parser.consume(Token::Comma) {
                    parser.expect(Token::RBracket)?;
                    break;
                }
            }
            Ok(Expr::List(span, exprs))
        }
        Token::LParen => {
            parser.gettok();
            let expr = parse_expr(parser, 0)?;
            parser.expect(Token::RParen)?;
            Ok(expr)
        }
        Token::LBrace => parse_block(parser),
        _ => Err(ParseError::InvalidToken {
            span,
            expected: "Expression".into(),
            got: format!("{:?}", parser.peek()).into(),
        }),
    }
}

fn parse_if(parser: &mut Parser) -> Result<Expr, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("if"))?;
    let cond = parse_expr(parser, 0)?;
    let body = parse_block(parser)?;
    let mut pairs = vec![(cond, body)];
    let mut other = Expr::Block(span.clone(), vec![]);

    while parser.consume(Token::Name("else")) {
        match parser.peek() {
            Token::Name("if") => {
                parser.gettok();
                let cond = parse_expr(parser, 0)?;
                let body = parse_block(parser)?;
                pairs.push((cond, body));
            }
            Token::LBrace => {
                other = parse_block(parser)?;
            }
            _ => {
                return Err(ParseError::InvalidToken {
                    span,
                    expected: "if or block (in else-branch)".into(),
                    got: format!("{:?}", parser.peek()),
                })
            }
        }
    }
    let span = span.upto(&parser.span());
    Ok(Expr::If(span, pairs, other.into()))
}

fn parse_while(parser: &mut Parser) -> Result<Expr, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("while"))?;
    let cond = parse_expr(parser, 0)?;
    let body = parse_block(parser)?;
    let span = span.upto(&parser.span());
    Ok(Expr::While(span, cond.into(), body.into()))
}

/// parse any infix expressions with given precedence or higher
fn parse_infix(parser: &mut Parser, mut lhs: Expr, prec: u32) -> Result<Expr, ParseError> {
    let start = parser.span();
    loop {
        match parser.peek() {
            Token::LParen => {
                if prec > PREC_POSTFIX {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                match lhs {
                    Expr::GetVar(_, name) => {
                        let mut args = Vec::new();
                        while !parser.consume(Token::RParen) {
                            args.push(parse_expr(parser, 0)?);
                            if !parser.consume(Token::Comma) {
                                parser.expect(Token::RParen)?;
                                break;
                            }
                        }
                        let end = parser.span();
                        let span = span.join(&start).upto(&end);
                        lhs = Expr::FunctionCall(span, name, args);
                    }
                    _ => {
                        return Err(ParseError::InvalidToken {
                            span,
                            expected: "Function call".into(),
                            got: format!("indirect function calls not yet supported"),
                        })
                    }
                }
            }
            Token::Dot => {
                if prec > PREC_POSTFIX {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                let name = parser.expect_name()?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::GetAttr(span, lhs.into(), name);
            }
            Token::Plus | Token::Minus => {
                if prec > PREC_SUM {
                    break;
                }
                let op = match parser.peek() {
                    Token::Plus => Binop::Add,
                    Token::Minus => Binop::Subtract,
                    tok => panic!("{:?}", tok),
                };
                let span = parser.span();
                parser.gettok();
                let right = parse_expr(parser, PREC_SUM + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, op, lhs.into(), right.into());
            }
            Token::Star | Token::Slash | Token::Slash2 | Token::Percent => {
                if prec > PREC_PRODUCT {
                    break;
                }
                let op = match parser.peek() {
                    Token::Star => Binop::Multiply,
                    Token::Slash => Binop::Divide,
                    Token::Slash2 => Binop::TruncDivide,
                    Token::Percent => Binop::Remainder,
                    tok => panic!("{:?}", tok),
                };
                let span = parser.span();
                parser.gettok();
                let right = parse_expr(parser, PREC_PRODUCT + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, op, lhs.into(), right.into());
            }
            Token::Caret => {
                if prec > PREC_BITWISE_XOR {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                let rhs = parse_expr(parser, PREC_BITWISE_XOR + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, Binop::BitwiseXor, lhs.into(), rhs.into());
            }
            Token::Ampersand => {
                if prec > PREC_BITWISE_AND {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                let rhs = parse_expr(parser, PREC_BITWISE_AND + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, Binop::BitwiseAnd, lhs.into(), rhs.into());
            }
            Token::VerticalBar => {
                if prec > PREC_BITWISE_OR {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                let rhs = parse_expr(parser, PREC_BITWISE_OR + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, Binop::BitwiseOr, lhs.into(), rhs.into());
            }
            Token::Lt2 | Token::Gt2 => {
                if prec > PREC_SHIFT {
                    break;
                }
                let op = match parser.peek() {
                    Token::Lt2 => Binop::ShiftLeft,
                    Token::Gt2 => Binop::ShiftRight,
                    tok => panic!("{:?}", tok),
                };
                let span = parser.span();
                parser.gettok();
                let rhs = parse_expr(parser, PREC_SHIFT + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, op, lhs.into(), rhs.into());
            }
            Token::Name("and") => {
                if prec > PREC_LOGICAL_AND {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                let rhs = parse_expr(parser, PREC_LOGICAL_AND + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::If(
                    span.clone(),
                    vec![(lhs, Expr::AssertType(span.clone(), Type::Bool, rhs.into()))],
                    Expr::Bool(span.clone(), false).into(),
                )
            }
            Token::Name("or") => {
                if prec > PREC_LOGICAL_OR {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                let rhs = parse_expr(parser, PREC_LOGICAL_OR + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::If(
                    span.clone(),
                    vec![(lhs, Expr::Bool(span.clone(), true))],
                    rhs.into(),
                )
            }
            Token::Eq2
            | Token::Ne
            | Token::Lt
            | Token::Gt
            | Token::Le
            | Token::Ge
            | Token::Name("is") => {
                if prec > PREC_CMP {
                    break;
                }
                let mut op = match parser.peek() {
                    Token::Eq2 => Binop::Equal,
                    Token::Ne => Binop::NotEqual,
                    Token::Lt => Binop::Less,
                    Token::Gt => Binop::Greater,
                    Token::Le => Binop::LessOrEqual,
                    Token::Ge => Binop::GreaterOrEqual,
                    Token::Name("is") => Binop::Is,
                    tok => panic!("{:?}", tok),
                };
                let span = parser.span();
                parser.gettok();
                if let Binop::Is = op {
                    if parser.consume(Token::Name("not")) {
                        op = Binop::IsNot;
                    }
                }
                let right = parse_expr(parser, PREC_CMP + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = Expr::Binop(span, op, lhs.into(), right.into());
            }
            Token::Eq => {
                if prec > PREC_ASSIGN {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                match lhs {
                    Expr::GetVar(_, name) => {
                        let setexpr = parse_expr(parser, 0)?;
                        lhs = Expr::SetVar(span.join(&start), name, setexpr.into());
                    }
                    _ => {
                        return Err(ParseError::InvalidToken {
                            span,
                            expected: "Assignment".into(),
                            got: format!("assignments only supported for variables"),
                        })
                    }
                }
            }
            _ => break,
        }
    }
    Ok(lhs)
}

fn parse_block(parser: &mut Parser) -> Result<Expr, ParseError> {
    let span = parser.span();
    parser.expect(Token::LBrace)?;
    let mut exprs = Vec::new();
    consume_delim(parser);
    while !parser.consume(Token::RBrace) {
        exprs.push(parse_stmt(parser)?);
    }
    let span = span.upto(&parser.span());
    Ok(Expr::Block(span, exprs))
}

fn parse_function_type(parser: &mut Parser, has_self: bool) -> Result<FunctionType, ParseError> {
    let mut trace = true;
    if parser.consume(Token::LBracket) {
        loop {
            match parser.peek() {
                Token::Name("notrace") => {
                    parser.gettok();
                    trace = false
                }
                Token::RBracket => {
                    parser.gettok();
                    break;
                }
                _ => {
                    return Err(ParseError::InvalidToken {
                        span: parser.span(),
                        expected: "FunctionType attribute".into(),
                        got: format!("{:?}", parser.peek()),
                    })
                }
            }
        }
    }
    let mut parameters = Vec::new();
    parser.expect(Token::LParen)?;
    let parse_remaining_params = if has_self {
        // The first parameter is always a fixed 'self' parameter
        // (with implied id type) for has_self functions
        // has_self applies for 'trait' and 'impl' functions
        parser.expect(Token::Name("self"))?;
        parameters.push(("self".into(), Type::Id));

        // in order for there to be more parameters,
        // 'self' has to be followed by a comma
        if parser.consume(Token::Comma) {
            true
        } else {
            parser.expect(Token::RParen)?;
            false
        }
    } else {
        true
    };
    if parse_remaining_params {
        while !parser.consume(Token::RParen) {
            let name = parser.expect_name()?;
            let type_ = parse_type(parser)?;
            parameters.push((name, type_));
            if !parser.consume(Token::Comma) {
                parser.expect(Token::RParen)?;
                break;
            }
        }
    }
    let return_type = if parser.at(Pattern::Name) {
        parse_return_type(parser)?
    } else {
        ReturnType::Void
    };
    Ok(FunctionType {
        parameters,
        return_type,
        trace,
    })
}

fn parse_return_type(parser: &mut Parser) -> Result<ReturnType, ParseError> {
    match parser.peek() {
        Token::Name("void") => {
            parser.gettok();
            Ok(ReturnType::Void)
        }
        Token::Name("noreturn") => {
            parser.gettok();
            Ok(ReturnType::NoReturn)
        }
        _ => Ok(ReturnType::Value(parse_type(parser)?)),
    }
}

fn parse_type(parser: &mut Parser) -> Result<Type, ParseError> {
    let opt = match parser.peek() {
        Token::Name("i32") => Some(Type::I32),
        Token::Name("i64") => Some(Type::I64),
        Token::Name("f32") => Some(Type::F32),
        Token::Name("f64") => Some(Type::F64),
        Token::Name("bool") => Some(Type::Bool),
        Token::Name("type") => Some(Type::Type),
        Token::Name("str") => Some(Type::String),
        Token::Name("list") => Some(Type::List),
        Token::Name("id") => Some(Type::Id),
        Token::Name(name) => Some(parser.get_user_defined_type(&parser.span(), &name.into())?),
        _ => None,
    };
    if let Some(t) = opt {
        parser.gettok();
        Ok(t)
    } else {
        Err(ParseError::InvalidToken {
            span: parser.span(),
            expected: "Type".into(),
            got: format!("{:?}", parser.peek()),
        })
    }
}
