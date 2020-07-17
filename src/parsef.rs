#![allow(dead_code)]
use crate::ast::*;
use crate::Error;
use crate::File;
use crate::ParseError;
use crate::Parser;
use crate::Pattern;
use crate::Source;
use crate::Span;
use crate::Token;
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

pub fn parse(source: &Rc<Source>) -> Result<File, Error> {
    let mut parser = match Parser::new(&source) {
        Ok(parser) => parser,
        Err(error) => {
            let lspan = error.span();
            let span = Span {
                source: source.clone(),
                main: lspan.main,
                start: lspan.start,
                end: lspan.end,
            };
            return Err(Error {
                span: vec![span],
                message: format!("{:?}", error),
            });
        }
    };
    match parse_file(&mut parser) {
        Ok(file) => Ok(file),
        Err(error) => Err(Error {
            span: vec![error.span().clone()],
            message: format!("{:?}", error),
        }),
    }
}

fn parse_file(parser: &mut Parser) -> Result<File, ParseError> {
    let span = parser.span();
    let mut constants = Vec::new();
    let mut externs = Vec::new();
    let mut funcs = Vec::new();
    let mut globals = Vec::new();
    let mut records = Vec::new();
    consume_delim(parser);
    while !parser.at(Token::EOF) {
        match parser.peek() {
            Token::Name("const") => constants.push(parse_const(parser)?),
            Token::Name("extern") => externs.push(parse_extern(parser)?),
            Token::Name("fn") => funcs.push(parse_func(parser)?),
            Token::Name("record") => records.push(parse_record(parser)?),
            Token::Name("var") => globals.push(parse_global(parser)?),
            _ => {
                return Err(ParseError::InvalidToken {
                    span: parser.span(),
                    expected: "Function, extern or record".into(),
                    got: format!("{:?}", parser.peek()),
                })
            }
        }
        expect_delim(parser)?;
    }
    let span = span.upto(&parser.span());
    Ok(File {
        span,
        constants,
        externs,
        funcs,
        globals,
        records,
    })
}

fn consume_delim(parser: &mut Parser) {
    loop {
        match parser.peek() {
            Token::Newline | Token::Semicolon => {
                parser.gettok();
            }
            _ => break,
        }
    }
}

fn expect_delim(parser: &mut Parser) -> Result<(), ParseError> {
    if !parser.at(Token::RBrace) && !parser.at(Token::EOF) {
        if !parser.consume(Token::Semicolon) {
            parser.expect(Token::Newline)?;
        }
    }
    consume_delim(parser);
    Ok(())
}

fn at_delim(parser: &mut Parser) -> bool {
    parser.at(Token::RBrace)
        || parser.at(Token::EOF)
        || parser.at(Token::Semicolon)
        || parser.at(Token::Newline)
}

fn parse_const(parser: &mut Parser) -> Result<RawConstant, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("const"))?;
    let name = parser.expect_name()?;
    let type_ = if parser.at(Token::Eq) {
        None
    } else {
        Some(parse_type(parser)?)
    };
    parser.expect(Token::Eq)?;
    let expr = parse_expr(parser, 0)?;
    let span = span.upto(&parser.span());
    Ok(RawConstant {
        span,
        name,
        type_,
        expr,
    })
}

fn parse_global(parser: &mut Parser) -> Result<RawGlobal, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("var"))?;
    let name = parser.expect_name()?;
    let type_ = if parser.at(Token::Eq) {
        None
    } else {
        Some(parse_type(parser)?)
    };
    parser.expect(Token::Eq)?;
    let init = parse_expr(parser, 0)?;
    let span = span.upto(&parser.span());
    Ok(RawGlobal {
        span,
        name,
        type_,
        init,
    })
}

fn parse_extern(parser: &mut Parser) -> Result<RawExtern, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("extern"))?;
    let path0 = parser.expect_string()?;
    let path1 = parser.expect_string()?;
    let name = parser.expect_name()?;
    let type_ = parse_func_type(parser, None)?;
    let span = span.upto(&parser.span());
    Ok(RawExtern {
        span,
        path: (path0, path1),
        name,
        type_,
    })
}

fn parse_record(parser: &mut Parser) -> Result<RawRecord, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("record"))?;
    let name = parser.expect_name()?;
    parser.expect(Token::LBrace)?;
    let mut fields = Vec::new();
    consume_delim(parser);
    while !parser.consume(Token::RBrace) {
        let field_name = parser.expect_name()?;
        let field_type = parse_type(parser)?;
        fields.push((field_name, field_type));
    }
    let span = span.upto(&parser.span());
    Ok(RawRecord { span, name, fields })
}

fn parse_func(parser: &mut Parser) -> Result<RawFunc, ParseError> {
    let span = parser.span();
    parser.expect(Token::Name("fn"))?;
    let name = parser.expect_name()?;
    let type_ = parse_func_type(parser, None)?;
    let body = parse_block(parser)?;
    let span = span.upto(&parser.span());
    Ok(RawFunc {
        span,
        name,
        type_,
        body,
    })
}

fn parse_block(parser: &mut Parser) -> Result<RawStmt, ParseError> {
    let span = parser.span();
    let mut stmts = Vec::new();
    parser.expect(Token::LBrace)?;
    consume_delim(parser);
    while !parser.consume(Token::RBrace) {
        let stmt = parse_stmt(parser)?;
        stmts.push(stmt);
        expect_delim(parser)?;
    }
    let span = span.upto(&parser.span());
    Ok(RawStmt {
        span,
        data: RawStmtData::Block(stmts),
    })
}

fn parse_stmt(parser: &mut Parser) -> Result<RawStmt, ParseError> {
    let span = parser.span();
    let data = match parser.peek() {
        Token::LBrace => parse_block(parser)?.data,
        Token::Name("return") => {
            parser.gettok();
            let expr = if at_delim(parser) {
                RawExpr {
                    span: span.clone(),
                    data: RawExprData::Void,
                }
            } else {
                parse_expr(parser, 0)?
            };
            RawStmtData::Return(expr)
        }
        Token::Name("var") => {
            parser.gettok();
            let name = parser.expect_name()?;
            let type_ = if parser.at(Token::Eq) {
                None
            } else {
                Some(parse_type(parser)?)
            };
            parser.expect(Token::Eq)?;
            let init = parse_expr(parser, 0)?;
            RawStmtData::DeclVar(name, type_, init)
        }
        _ => {
            let expr = parse_expr(parser, 0)?;
            RawStmtData::Expr(expr)
        }
    };
    let span = span.upto(&parser.span());
    Ok(RawStmt { span, data })
}

fn parse_expr(parser: &mut Parser, prec: u32) -> Result<RawExpr, ParseError> {
    let atom = parse_atom(parser)?;
    parse_infix(parser, atom, prec)
}

fn parse_atom(parser: &mut Parser) -> Result<RawExpr, ParseError> {
    let span = parser.span();
    match parser.peek() {
        Token::Int(x) => {
            parser.gettok();
            Ok(RawExpr {
                span,
                data: RawExprData::Int(x),
            })
        }
        Token::Float(x) => {
            parser.gettok();
            Ok(RawExpr {
                span,
                data: RawExprData::Float(x),
            })
        }
        Token::RawString(_) | Token::NormalString(_) => {
            let string = parser.expect_string()?;
            Ok(RawExpr {
                span,
                data: RawExprData::Str(string),
            })
        }
        Token::Name(_) => {
            let name = parser.expect_name()?;
            Ok(RawExpr {
                span,
                data: RawExprData::GetVar(name),
            })
        }
        Token::LParen => {
            parser.gettok();
            let expr = parse_expr(parser, 0)?;
            parser.expect(Token::RParen)?;
            Ok(expr)
        }
        Token::Dollar => {
            parser.gettok();
            match parser.peek() {
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
                    let type_ = parse_type(parser)?;
                    parser.expect(Token::Comma)?;
                    let code = parser.expect_string()?;
                    parser.consume(Token::Comma);
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Asm(args, type_, code),
                    })
                }
                Token::Name("raw") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let name = parser.expect_name()?;
                    parser.consume(Token::Comma);
                    parser.expect(Token::RParen)?;
                    let span = span.upto(&parser.span());
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Raw(name),
                    })
                }
                Token::Name("char") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let text = parser.expect_string()?;
                    let chars: Vec<_> = text.chars().collect();
                    if chars.len() != 1 {
                        return Err(ParseError::InvalidToken {
                            span: parser.span().clone(),
                            expected: "exactly one char".into(),
                            got: format!("{} chars ({})", chars.len(), text),
                        });
                    }
                    parser.consume(Token::Comma);
                    parser.expect(Token::RParen)?;
                    let span = span.upto(&parser.span());
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Char(chars[0]),
                    })
                }
                Token::Name("read1") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Read1(addr.into(), offset),
                    })
                }
                Token::Name("read2") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Read2(addr.into(), offset),
                    })
                }
                Token::Name("read4") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Read4(addr.into(), offset),
                    })
                }
                Token::Name("read8") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Read8(addr.into(), offset),
                    })
                }
                Token::Name("write1") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    parser.expect(Token::Comma)?;
                    let data = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Write1(addr.into(), data.into(), offset),
                    })
                }
                Token::Name("write2") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    parser.expect(Token::Comma)?;
                    let data = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Write2(addr.into(), data.into(), offset),
                    })
                }
                Token::Name("write4") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    parser.expect(Token::Comma)?;
                    let data = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Write4(addr.into(), data.into(), offset),
                    })
                }
                Token::Name("write8") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let addr = parse_expr(parser, 0)?;
                    parser.expect(Token::Comma)?;
                    let data = parse_expr(parser, 0)?;
                    let offset = if parser.consume(Token::Comma) {
                        parser.expect(Token::Name("offset"))?;
                        parser.expect(Token::Colon)?;
                        parser.expect_u32()?
                    } else {
                        0
                    };
                    parser.expect(Token::RParen)?;
                    Ok(RawExpr {
                        span,
                        data: RawExprData::Write8(addr.into(), data.into(), offset),
                    })
                }
                _ => {
                    return Err(ParseError::InvalidToken {
                        span: parser.span(),
                        expected: format!("intrinsic"),
                        got: format!("{:?}", parser.peek()),
                    })
                }
            }
        }
        _ => Err(ParseError::InvalidToken {
            span,
            expected: "Expression".into(),
            got: format!("{:?}", parser.peek()),
        }),
    }
}

/// parse any infix expressions with given precedence or higher
fn parse_infix(parser: &mut Parser, mut lhs: RawExpr, prec: u32) -> Result<RawExpr, ParseError> {
    let start = parser.span();
    loop {
        let span = parser.span();
        match parser.peek() {
            Token::LParen => {
                if prec > PREC_POSTFIX {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                match lhs.data {
                    RawExprData::GetVar(name) => {
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
                        lhs = RawExpr {
                            span,
                            data: RawExprData::CallFunc(name, args),
                        };
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
            Token::Lt | Token::Le | Token::Gt | Token::Ge | Token::Name("is") => {
                if prec > PREC_CMP {
                    break;
                }
                let op = if parser.consume(Token::Name("is")) {
                    if parser.consume(Token::Name("not")) {
                        Binop::IsNot
                    } else {
                        Binop::Is
                    }
                } else {
                    let token = parser.gettok();
                    match Binop::from_token(token) {
                        Some(op) => op,
                        None => panic!("Impossible op token: {:?}", token),
                    }
                };
                let rhs = parse_expr(parser, PREC_CMP + 1)?;
                let span = span.join(&start).upto(&parser.span());
                lhs = RawExpr {
                    span,
                    data: RawExprData::Binop(op, lhs.into(), rhs.into()),
                };
            }
            Token::Eq => {
                if prec > PREC_ASSIGN {
                    break;
                }
                let span = parser.span();
                parser.gettok();
                match lhs.data {
                    RawExprData::GetVar(name) => {
                        let setexpr = parse_expr(parser, 0)?;
                        let end = parser.span();
                        let span = span.join(&start).upto(&end);
                        lhs = RawExpr {
                            span,
                            data: RawExprData::SetVar(name, setexpr.into()),
                        };
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
            Token::Plus | Token::Minus => {
                if prec > PREC_SUM {
                    break;
                }
                let span = parser.span();
                let op = Binop::from_token(parser.gettok()).expect("impossible binop");
                let rhs = parse_expr(parser, PREC_SUM + 1)?;
                let span = span.upto(&parser.span());
                lhs = RawExpr {
                    span,
                    data: RawExprData::Binop(op, lhs.into(), rhs.into()),
                };
            }
            Token::Star | Token::Slash | Token::Slash2 | Token::Percent => {
                if prec > PREC_PRODUCT {
                    break;
                }
                let span = parser.span();
                let op = Binop::from_token(parser.gettok()).expect("impossible binop");
                let rhs = parse_expr(parser, PREC_PRODUCT + 1)?;
                let span = span.upto(&parser.span());
                lhs = RawExpr {
                    span,
                    data: RawExprData::Binop(op, lhs.into(), rhs.into()),
                };
            }
            _ => break,
        }
    }
    Ok(lhs)
}

fn parse_type(parser: &mut Parser) -> Result<TypeExpr, ParseError> {
    let span = parser.span();
    let texpr = TypeExpr {
        span,
        name: parser.expect_name()?,
    };
    Ok(texpr)
}

fn parse_func_type(
    parser: &mut Parser,
    self_type: Option<Rc<str>>,
) -> Result<FuncTypeExpr, ParseError> {
    let start = parser.span();
    let mut parameters = Vec::new();
    parser.expect(Token::LParen)?;
    let parse_remaining_params = if let Some(self_type) = self_type {
        // The first parameter is always a fixed 'self' parameter
        // (with implied id type) for has_self functions
        // has_self applies for 'trait' and 'impl' functions
        parser.expect(Token::Name("self"))?;
        parameters.push((
            "self".into(),
            TypeExpr {
                span: parser.span(),
                name: self_type,
            },
        ));

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
        parse_type(parser)?
    } else {
        TypeExpr {
            span: start.clone(),
            name: "void".into(),
        }
    };
    let span = start.upto(&parser.span());
    Ok(FuncTypeExpr {
        span,
        parameters,
        return_type,
    })
}
