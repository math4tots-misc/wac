use crate::ir::*;
use crate::Error;
use crate::ParseError;
use crate::Parser;
use crate::Pattern;
use crate::Sink;
use crate::Span;
use crate::Token;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::Cell;

pub const RESERVED_BYTES: usize = 1024;

/// translates a list of (filename, wac-code) pairs into
/// a wat webassembly module
pub fn translate(mut sources: Vec<(Rc<str>, Rc<str>)>) -> Result<String, Error> {
    let prelude_name: Rc<str> = "[prelude]".into();
    let prelude_str: Rc<str> = include_str!("prelude.wac").into();
    sources.insert(0, (prelude_name, prelude_str));
    let mut files = Vec::new();
    for (filename, data) in sources {
        let mut parser = match Parser::new(&data) {
            Ok(parser) => parser,
            Err(error) => return Err(Error::Lex(filename.clone(), error)),
        };
        let file = match parse_file(&mut parser) {
            Ok(file) => file,
            Err(error) => return Err(Error::Parse(filename.clone(), error)),
        };
        files.push((filename, file));
    }
    let out = Out::new();
    let mut functions = HashMap::new();
    for (_filename, file) in &files {
        for imp in &file.imports {
            match imp {
                Import::Function(FunctionImport { alias, type_, .. }) => {
                    functions.insert(alias.clone(), type_.clone());
                }
            }
        }
        for func in &file.functions {
            functions.insert(func.name.clone(), func.type_().clone());
        }
    }
    let gscope = GlobalScope { functions };
    for (_filename, file) in files {
        for imp in file.imports {
            translate_import(&out, imp);
        }
        for func in file.functions {
            translate_func(&out, &gscope, func)?;
        }
    }
    Ok(out.get())
}

struct GlobalScope {
    functions: HashMap<Rc<str>, FunctionType>,
}

struct LocalScope<'a> {
    g: &'a GlobalScope,
    locals: HashMap<Rc<str>, Type>,
}

impl<'a> LocalScope<'a> {
    fn get(&self, name: &Rc<str>) -> Option<ScopeEntry> {
        match self.locals.get(name) {
            Some(t) => Some(ScopeEntry::Local(*t)),
            None => None,
        }
    }
    fn get_or_err(&self, span: Span, name: &Rc<str>) -> Result<ScopeEntry, Error> {
        match self.get(name) {
            Some(e) => Ok(e),
            None => Err(Error::Type {
                span,
                expected: format!("Variable {}", name),
                got: "NotFound".into(),
            }),
        }
    }
    fn getf(&self, name: &Rc<str>) -> Option<FunctionType> {
        self.g.functions.get(name).cloned()
    }
    fn getf_or_err(&self, span: Span, name: &Rc<str>) -> Result<FunctionType, Error> {
        match self.getf(name) {
            Some(e) => Ok(e),
            None => Err(Error::Type {
                span,
                expected: format!("Function {}", name),
                got: "NotFound".into(),
            }),
        }
    }
}

enum ScopeEntry {
    Local(Type),
}

fn translate_func_type(ft: &FunctionType) -> String {
    let mut ret = String::new();
    let FunctionType {
        return_type,
        parameter_types,
    } = ft;
    for pt in parameter_types {
        ret.push_str(&format!(" (param {})", translate_type(*pt)));
    }
    if let Some(rt) = return_type {
        ret.push_str(&format!(" (result {})", translate_type(*rt)));
    }
    ret
}

fn translate_type(t: Type) -> &'static str {
    match t.wasm() {
        WasmType::I32 => "i32",
        WasmType::I64 => "i64",
        WasmType::F32 => "f32",
        WasmType::F64 => "f64",
    }
}

fn translate_import(out: &Out, imp: Import) {
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

fn translate_func(out: &Out, gscope: &GlobalScope, func: Function) -> Result<(), Error> {
    let mut locals = HashMap::new();
    for (lname, ltype) in &func.locals {
        locals.insert(lname.clone(), *ltype);
    }
    let lscope = LocalScope { g: gscope, locals };
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
    for parameter_type in func.type_().parameter_types {
        sink.writeln(format!(
            " (param $l_{} {})",
            func.name,
            translate_type(parameter_type)
        ));
    }
    if let Some(return_type) = func.return_type {
        sink.writeln(format!(" (result {})", translate_type(return_type)));
    }
    for (lname, ltype) in &func.locals {
        sink.writeln(format!(" (local $l_{} {})", lname, translate_type(*ltype)));
    }
    translate_expr(out, &sink, &lscope, func.return_type, &func.body)?;
    sink.writeln(")");
    Ok(())
}

fn translate_expr(
    out: &Out,
    sink: &Rc<Sink>,
    lscope: &LocalScope,
    etype: Option<Type>,
    expr: &Expr,
) -> Result<(), Error> {
    match expr {
        Expr::Int(span, x) => {
            match etype {
                Some(Type::I32) => {
                    sink.writeln(format!("(i32.const {})", x));
                }
                Some(Type::I64) => {
                    sink.writeln(format!("(i64.const {})", x));
                }
                Some(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "Int".into(),
                    })
                }
                None => {
                    // no-op value is dropped
                }
            }
        }
        Expr::Float(span, x) => {
            match etype {
                Some(Type::F32) => {
                    sink.writeln(format!("(f32.const {})", x));
                }
                Some(Type::F64) => {
                    sink.writeln(format!("(f64.const {})", x));
                }
                Some(t) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", t),
                        got: "Float".into(),
                    })
                }
                None => {
                    // no-op value is dropped
                }
            }
        }
        Expr::Block(span, exprs) => {
            if let Some(last) = exprs.last() {
                for expr in &exprs[..exprs.len() - 1] {
                    translate_expr(out, sink, lscope, None, expr)?;
                }
                translate_expr(out, sink, lscope, etype, last)?;
            } else {
                match etype {
                    None => {}
                    Some(t) => {
                        return Err(Error::Type {
                            span: span.clone(),
                            expected: format!("{:?}", t),
                            got: "Void (empty-block)".into(),
                        })
                    }
                }
            }
        }
        Expr::GetVar(span, name) => {
            let entry = lscope.get_or_err(span.clone(), name)?;
            match entry {
                ScopeEntry::Local(vartype) => {
                    match etype {
                        Some(etype) if etype == vartype => {
                            sink.writeln(format!("local.get $l_{}", name));
                        }
                        Some(etype) => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{:?}", etype),
                                got: format!("{:?}", vartype),
                            })
                        }
                        None => {
                            // we already checked this variable exists,
                            // if we don't use the return value,
                            // there's nothing we need to do here
                        }
                    }
                }
            }
        }
        Expr::SetVar(span, name, setexpr) => {
            let entry = lscope.get_or_err(span.clone(), name)?;
            if let Some(etype) = etype {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{:?}", etype),
                    got: "Void (setvar)".into(),
                });
            }
            match entry {
                ScopeEntry::Local(vartype) => {
                    match etype {
                        Some(etype) => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{:?}", etype),
                                got: "Void (setvar)".into(),
                            })
                        }
                        None => {
                            translate_expr(out, sink, lscope, Some(vartype), setexpr)?;
                            sink.writeln(format!("local.set $l_{}", name));
                        }
                    }
                }
            }
        }
        Expr::FunctionCall(span, fname, argexprs) => {
            let ftype = lscope.getf_or_err(span.clone(), fname)?;
            if argexprs.len() != ftype.parameter_types.len() {
                return Err(Error::Type {
                    span: span.clone(),
                    expected: format!("{} args", ftype.parameter_types.len()),
                    got: format!("{} args", argexprs.len()),
                });
            }
            for (argexpr, ptype) in argexprs.iter().zip(ftype.parameter_types) {
                translate_expr(out, sink, lscope, Some(ptype), argexpr)?;
            }
            sink.writeln(format!("call $f_{}", fname));
            match etype {
                Some(etype) => {
                    match &ftype.return_type {
                        Some(return_type) if *return_type == etype => {
                            // types match
                        }
                        Some(return_type) => {
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{:?}", etype),
                                got: format!("{:?}", return_type),
                            })
                        }
                        None => {
                            // expects etype, but returns void
                            return Err(Error::Type {
                                span: span.clone(),
                                expected: format!("{:?}", etype),
                                got: "Void (function-return)".into(),
                            });
                        }
                    }
                }
                None => {
                    match ftype.return_type {
                        Some(_) => {
                            // expects void, but returns something
                            // we need to remove it
                            sink.writeln("drop");
                        }
                        None => {
                            // expects void, returns void
                        }
                    }
                }
            }
        }
        Expr::CString(span, value) => {
            match etype {
                Some(Type::I32) => {
                    let ptr = out.data(value.as_bytes());
                    sink.writeln(format!("i32.const {}", ptr));
                }
                Some(etype) => {
                    return Err(Error::Type {
                        span: span.clone(),
                        expected: format!("{:?}", etype),
                        got: "i32 (cstr)".into(),
                    })
                }
                None => {}
            }
        }
    }
    Ok(())
}

fn parse_file(parser: &mut Parser) -> Result<File, ParseError> {
    let mut imports = Vec::new();
    while parser.at_name("import") {
        let span = parser.span();
        parser.gettok();
        parser.expect(Token::Name("fn"))?;
        let module_name = parser.expect_string()?;
        let function_name = parser.expect_string()?;
        let alias = parser.expect_name()?;
        let type_ = parse_function_type(parser)?;
        let span = span.upto(parser.span());
        consume_delim(parser);
        imports.push(Import::Function(FunctionImport {
            span,
            module_name,
            function_name,
            alias,
            type_,
        }));
    }
    let mut functions = Vec::new();
    consume_delim(parser);
    while !parser.at(Token::EOF) {
        match parser.peek() {
            Token::Name("fn") => functions.push(parse_func(parser)?),
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
    Ok(File { imports, functions })
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
    let mut parameters = Vec::new();
    parser.expect(Token::LParen)?;
    while !parser.consume(Token::RParen) {
        let param_name = parser.expect_name()?;
        let param_type = parse_type(parser)?;
        parameters.push((param_name, param_type));
        if !parser.consume(Token::Comma) {
            parser.expect(Token::RParen)?;
            break;
        }
    }
    let return_type = if parser.at(Pattern::Name) {
        Some(parse_type(parser)?)
    } else {
        None
    };
    let mut locals = Vec::new();
    if parser.consume(Token::LBracket) {
        while !parser.consume(Token::RBracket) {
            let name = parser.expect_name()?;
            let type_ = parse_type(parser)?;
            locals.push((name, type_));
            if !parser.consume(Token::Comma) {
                parser.expect(Token::RBracket)?;
                break;
            }
        }
    }
    let body = parse_block(parser)?;
    let span = span.upto(parser.span());
    Ok(Function {
        span,
        visibility,
        name,
        parameters,
        return_type,
        locals,
        body,
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
    let expr = parse_expr(parser)?;
    consume_delim(parser);
    Ok(expr)
}

fn parse_expr(parser: &mut Parser) -> Result<Expr, ParseError> {
    parse_assign(parser)
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
        Token::Name(name) => {
            parser.gettok();
            Ok(Expr::GetVar(span, name.into()))
        }
        Token::Dollar => {
            parser.gettok();
            match parser.peek() {
                Token::Name("cstr") => {
                    parser.gettok();
                    parser.expect(Token::LParen)?;
                    let string = parser.expect_string()?;
                    parser.expect(Token::RParen)?;
                    let span = span.upto(parser.span());
                    Ok(Expr::CString(span, string))
                }
                _ => Err(ParseError::InvalidToken {
                    span,
                    expected: "intrinsic name".into(),
                    got: format!("{:?}", parser.peek()),
                })
            }
        }
        Token::LBrace => parse_block(parser),
        _ => Err(ParseError::InvalidToken {
            span,
            expected: "Expression".into(),
            got: format!("{:?}", parser.peek()).into(),
        }),
    }
}

fn parse_postfix(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut e = parse_atom(parser)?;
    let start = parser.span();
    loop {
        match parser.peek() {
            Token::LParen => {
                let span = parser.span();
                parser.gettok();
                match e {
                    Expr::GetVar(_, name) => {
                        let mut args = Vec::new();
                        while !parser.consume(Token::RParen) {
                            args.push(parse_expr(parser)?);
                            if !parser.consume(Token::Comma) {
                                parser.expect(Token::RParen)?;
                                break;
                            }
                        }
                        let end = parser.span();
                        let span = span.join(start).upto(end);
                        e = Expr::FunctionCall(span, name, args);
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
            _ => break,
        }
    }
    Ok(e)
}

fn parse_assign(parser: &mut Parser) -> Result<Expr, ParseError> {
    let mut e = parse_postfix(parser)?;
    let start = parser.span();
    loop {
        match parser.peek() {
            Token::Eq => {
                let span = parser.span();
                parser.gettok();
                match e {
                    Expr::GetVar(_, name) => {
                        let setexpr = parse_expr(parser)?;
                        e = Expr::SetVar(span.join(start), name, setexpr.into());
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
    Ok(e)
}

fn parse_block(parser: &mut Parser) -> Result<Expr, ParseError> {
    let span = parser.span();
    parser.expect(Token::LBrace)?;
    let mut exprs = Vec::new();
    consume_delim(parser);
    while !parser.consume(Token::RBrace) {
        exprs.push(parse_stmt(parser)?);
    }
    let span = span.upto(parser.span());
    Ok(Expr::Block(span, exprs))
}

fn parse_function_type(parser: &mut Parser) -> Result<FunctionType, ParseError> {
    let mut parameter_types = Vec::new();
    parser.expect(Token::LParen)?;
    while !parser.consume(Token::RParen) {
        let type_ = parse_type(parser)?;
        parameter_types.push(type_);
        if !parser.consume(Token::Comma) {
            parser.expect(Token::RParen)?;
            break;
        }
    }
    let return_type = if parser.at(Pattern::Name) {
        Some(parse_type(parser)?)
    } else {
        None
    };
    Ok(FunctionType {
        parameter_types,
        return_type,
    })
}

fn parse_type(parser: &mut Parser) -> Result<Type, ParseError> {
    let opt = match parser.peek() {
        Token::Name("i32") => Some(Type::I32),
        Token::Name("i64") => Some(Type::I64),
        Token::Name("f32") => Some(Type::F32),
        Token::Name("f64") => Some(Type::F64),
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

struct Out {
    main: Rc<Sink>,
    imports: Rc<Sink>,
    memory: Rc<Sink>,
    data: Rc<Sink>,
    funcs: Rc<Sink>,
    exports: Rc<Sink>,

    data_len: Cell<usize>,
}

impl Out {
    fn new() -> Self {
        let main = Sink::new();
        let imports = main.spawn();
        let memory = main.spawn();
        let data = main.spawn();
        let funcs = main.spawn();
        let exports = main.spawn();
        Self {
            main,
            imports,
            memory,
            data,
            funcs,
            exports,
            data_len: Cell::new(RESERVED_BYTES),
        }
    }

    fn get(self) -> String {
        self.memory.writeln("(memory $rt_mem 1)");
        self.main.get()
    }

    fn data(&self, data: &[u8]) -> u32 {
        let reserve_len = (data.len() + 8 - 1) / 8 * 8;
        let ptr = self.data_len.get();
        self.data_len.set(reserve_len + ptr);
        self.data.write(format!("(data (i32.const {}) \"", ptr));
        for byte in data {
            self.data.write(format!("\\{:0>2X}", byte));
        }
        self.data.writeln("\")");
        ptr as u32
    }
}
