use crate::lex;
use crate::LLExpr;
use crate::LLFile;
use crate::LLFunction;
use crate::LLFunctionImport;
use crate::LLFunctionType;
use crate::LLImport;
use crate::LLType;
use crate::LLVisibility;
use crate::LexError;
use crate::Span;
use crate::Token;
use std::rc::Rc;

pub fn parse(filename: Rc<str>, s: &str) -> Result<LLFile, ParseError> {
    let tokens_and_spans = lex(s)?;
    let parser = Parser {
        filename,
        i: 0,
        tokens_and_spans,
    };
    parser.file()
}

struct Parser<'a> {
    filename: Rc<str>,
    i: usize,
    tokens_and_spans: Vec<(Token<'a>, Span)>,
}

impl<'a> Parser<'a> {
    fn peek(&self) -> Token<'a> {
        self.tokens_and_spans[self.i].0
    }
    fn span(&self) -> Span {
        self.tokens_and_spans[self.i].1
    }
    fn gettok(&mut self) -> Token<'a> {
        let token = self.peek();
        self.i += 1;
        token
    }
    fn at<'b, P: Into<Pattern<'b>>>(&self, p: P) -> bool {
        p.into().matches(self.peek())
    }
    fn at_name(&self, name: &str) -> bool {
        if let Token::Name(s) = self.peek() {
            s == name
        } else {
            false
        }
    }
    fn consume<'b, P: Into<Pattern<'b>>>(&mut self, p: P) -> bool {
        if self.at(p) {
            self.gettok();
            true
        } else {
            false
        }
    }
    fn expect<'b, P: Into<Pattern<'b>>>(&mut self, p: P) -> Result<Token<'a>, ParseError> {
        let p = p.into();
        if self.at(p) {
            Ok(self.gettok())
        } else {
            Err(ParseError::InvalidToken {
                span: self.span(),
                expected: format!("{:?}", p),
                got: format!("{:?}", self.peek()),
            })
        }
    }
    fn expect_name(&mut self) -> Result<Rc<str>, ParseError> {
        match self.peek() {
            Token::Name(name) => {
                self.gettok();
                Ok(name.into())
            }
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "Name".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
    fn expect_string(&mut self) -> Result<Rc<str>, ParseError> {
        match self.peek() {
            Token::RawString(s) => {
                self.gettok();
                Ok(s.into())
            }
            Token::NormalString(s) => {
                self.gettok();
                Ok(resolve_escapes(s)?)
            }
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "String".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
    /// returns (expr, semicolon_required) pair
    fn stmt_body(&mut self) -> Result<(LLExpr, bool), ParseError> {
        Ok(match self.peek() {
            Token::LBracket => (self.block()?, false),
            _ => {
                (self.expr()?, true)
            }
        })
    }
    #[allow(dead_code)]
    fn stmt(&mut self) -> Result<LLExpr, ParseError> {
        let (expr, semicolon_required) = self.stmt_body()?;
        if semicolon_required {
            self.expect(Token::Semicolon)?;
        }
        Ok(expr)
    }
    fn expr(&mut self) -> Result<LLExpr, ParseError> {
        self.postfix()
    }
    fn atom(&mut self) -> Result<LLExpr, ParseError> {
        match self.peek() {
            Token::Int(i) => {
                self.gettok();
                Ok(LLExpr::Int(self.span(), i))
            }
            Token::Float(i) => {
                self.gettok();
                Ok(LLExpr::Float(self.span(), i))
            }
            Token::Name(name) => {
                self.gettok();
                Ok(LLExpr::GetVar(self.span(), name.into()))
            }
            Token::LParen => {
                self.gettok();
                let expr = self.expr()?;
                self.expect(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "Expression".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
    fn block(&mut self) -> Result<LLExpr, ParseError> {
        let start = self.span();
        self.expect(Token::LBrace)?;
        let mut exprs = Vec::new();
        let mut last_open = false;
        while !self.consume(Token::RBrace) {
            let (expr, semicolon_required) = self.stmt_body()?;
            exprs.push(expr);
            if semicolon_required && !self.at(Token::Semicolon) {
                last_open = true;
                self.expect(Token::RBrace)?;
                break;
            } else if self.consume(Token::Semicolon) {
                last_open = false;
            } else {
                last_open = true;
            }
        }
        let span = start.join(self.span());
        let last = if last_open {
            Some(Box::new(exprs.pop().unwrap()))
        } else {
            None
        };
        Ok(LLExpr::Block(span, exprs, last))
    }
    fn postfix(&mut self) -> Result<LLExpr, ParseError> {
        let mut expr = self.atom()?;
        loop {
            match self.peek() {
                Token::LParen => {
                    let start = self.span();
                    self.gettok();
                    let mut args = Vec::new();
                    while !self.at(Token::RParen) {
                        args.push(self.expr()?);
                        if !self.at(Token::RParen) {
                            self.expect(Token::Comma)?;
                        }
                    }
                    let end = self.span();
                    self.expect(Token::RParen)?;
                    expr = match expr {
                        LLExpr::GetVar(name_span, name) => {
                            LLExpr::FunctionCall(start.join(name_span).join(end), name, args)
                        }
                        _ => {
                            panic!("Indirect function calls not yet supported");
                        }
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }
    fn import(&mut self) -> Result<LLImport, ParseError> {
        self.expect(Token::Name("import"))?;
        self.expect(Token::Name("fn"))?;
        let module_name = self.expect_string()?;
        let function_name = self.expect_string()?;
        let imported_name = self.expect_name()?;
        let type_ = self.func_type()?;
        self.expect(Token::Semicolon)?;
        let fimp = LLFunctionImport {
            span: self.span(),
            module_name,
            function_name,
            imported_name,
            type_,
        };
        Ok(LLImport::Function(fimp))
    }
    fn func(&mut self) -> Result<LLFunction, ParseError> {
        let start = self.span();
        self.expect(Token::Name("fn"))?;
        let mut visibility = LLVisibility::Private;
        if self.consume(Token::LBracket) {
            loop {
                if self.consume(Token::Name("pub")) {
                    visibility = LLVisibility::Public;
                    continue;
                }
                self.expect(Token::RBracket)?;
                break;
            }
        }
        let name = self.expect_name()?;
        let mut parameters = Vec::new();
        self.expect(Token::LParen)?;
        while !self.consume(Token::RParen) {
            let name = self.expect_name()?;
            let paramtype = self.type_()?;
            parameters.push((name, paramtype));
            if !self.consume(Token::Comma) {
                self.expect(Token::RParen)?;
                break;
            }
        }
        let return_type = if self.at(Pattern::Name) {
            self.type_()?
        } else {
            // If not specified, i32 is assumed
            LLType::I32
        };
        let mut locals = Vec::new();
        if self.consume(Token::LBracket) {
            while self.at(Pattern::Name) {
                let name = self.expect_name()?;
                let localtype = self.type_()?;
                locals.push((name, localtype));
                if !self.consume(Token::Comma) {
                    break;
                }
            }
            self.expect(Token::RBracket)?;
        }
        let body = self.block()?;
        Ok(LLFunction {
            span: start.join(body.span()),
            visibility,
            name,
            parameters,
            return_type,
            locals,
            body,
        })
    }
    fn file(mut self) -> Result<LLFile, ParseError> {
        let mut imports = Vec::new();
        let mut functions = Vec::new();
        while self.at_name("import") {
            imports.push(self.import()?);
        }
        while self.at_name("fn") {
            functions.push(self.func()?);
        }
        Ok(LLFile {
            name: self.filename,
            imports,
            functions,
        })
    }
    fn type_(&mut self) -> Result<LLType, ParseError> {
        match self.peek() {
            Token::Name("i32") => {
                self.gettok();
                Ok(LLType::I32)
            }
            Token::Name("f32") => {
                self.gettok();
                Ok(LLType::F32)
            }
            Token::Name("i64") => {
                self.gettok();
                Ok(LLType::I64)
            }
            Token::Name("f64") => {
                self.gettok();
                Ok(LLType::F64)
            }
            Token::LParen => Ok(LLType::Function(self.func_type()?.into())),
            _ => Err(ParseError::InvalidToken {
                span: self.span(),
                expected: "Type".into(),
                got: format!("{:?}", self.peek()),
            }),
        }
    }
    fn func_type(&mut self) -> Result<LLFunctionType, ParseError> {
        self.expect(Token::LParen)?;
        let mut parameters = Vec::new();
        while !self.at(Token::RParen) {
            parameters.push(self.type_()?);
            if !self.at(Token::RParen) {
                self.expect(Token::Comma)?;
            }
        }
        self.expect(Token::RParen)?;
        let return_type = self.type_()?;
        Ok(LLFunctionType {
            parameters,
            return_type,
        })
    }
}

fn resolve_escapes(s: &str) -> Result<Rc<str>, ParseError> {
    enum State {
        Normal,
        Escape,
    }
    let mut ret = String::new();
    let mut state = State::Normal;
    for c in s.chars() {
        match state {
            State::Normal => {
                if c == '\\' {
                    state = State::Escape;
                } else {
                    ret.push(c);
                }
            }
            State::Escape => {
                match c {
                    'n' => ret.push('\n'),
                    'r' => ret.push('\r'),
                    't' => ret.push('\t'),
                    '0' => ret.push('\0'),
                    '"' => ret.push('"'),
                    '\'' => ret.push('\''),
                    '\\' => ret.push('\\'),
                    _ => return Err(ParseError::InvalidEscape(c)),
                }
                state = State::Normal;
            }
        }
    }
    Ok(ret.into())
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
enum Pattern<'a> {
    Exact(Token<'a>),
    Name,
    Int,
    Float,
    String,
}

impl<'a> Pattern<'a> {
    fn matches<'b>(&self, t: Token<'b>) -> bool {
        match self {
            Pattern::Exact(p) => t == *p,
            Pattern::Name => match t {
                Token::Name(_) => true,
                _ => false,
            },
            Pattern::Int => match t {
                Token::Int(_) => true,
                _ => false,
            },
            Pattern::Float => match t {
                Token::Float(_) => true,
                _ => false,
            },
            Pattern::String => match t {
                Token::NormalString(_) | Token::RawString(_) => true,
                _ => false,
            },
        }
    }
}

impl<'a> From<Token<'a>> for Pattern<'a> {
    fn from(t: Token<'a>) -> Self {
        Self::Exact(t)
    }
}

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    InvalidToken {
        span: Span,
        expected: String,
        got: String,
    },
    InvalidEscape(char),
}

impl From<LexError> for ParseError {
    fn from(e: LexError) -> ParseError {
        ParseError::Lex(e)
    }
}
