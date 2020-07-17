use crate::Span;
use crate::Token;
use std::rc::Rc;

pub struct File {
    pub span: Span,
    pub constants: Vec<RawConstant>,
    pub externs: Vec<RawExtern>,
    pub records: Vec<RawRecord>,
    pub globals: Vec<RawGlobal>,
    pub funcs: Vec<RawFunc>,
}

pub struct RawConstant {
    pub span: Span,
    pub name: Rc<str>,
    pub type_: Option<TypeExpr>,
    pub expr: RawExpr,
}

pub struct RawExtern {
    pub span: Span,
    pub path: (Rc<str>, Rc<str>),
    pub name: Rc<str>,
    pub type_: FuncTypeExpr,
}

pub struct RawRecord {
    pub span: Span,
    pub name: Rc<str>,
    pub fields: Vec<(Rc<str>, TypeExpr)>,
}

pub struct RawGlobal {
    pub span: Span,
    pub name: Rc<str>,
    pub type_: Option<TypeExpr>,
    pub init: RawExpr,
}

pub struct RawFunc {
    pub span: Span,
    pub name: Rc<str>,
    pub type_: FuncTypeExpr,
    pub body: RawStmt,
}

pub struct TypeExpr {
    pub span: Span,
    pub name: Rc<str>,
}

pub struct FuncTypeExpr {
    pub span: Span,
    pub parameters: Vec<(Rc<str>, TypeExpr)>,
    pub return_type: TypeExpr,
}

pub struct RawStmt {
    pub span: Span,
    pub data: RawStmtData,
}

pub enum RawStmtData {
    Block(Vec<RawStmt>),
    DeclVar(Rc<str>, Option<TypeExpr>, RawExpr),
    Return(RawExpr),
    Expr(RawExpr),
}

pub struct RawExpr {
    pub span: Span,
    pub data: RawExprData,
}

pub enum RawExprData {
    Void,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(Rc<str>),
    GetVar(Rc<str>),
    SetVar(Rc<str>, Box<RawExpr>),
    AugVar(Rc<str>, Binop, Box<RawExpr>),
    CallFunc(Rc<str>, Vec<RawExpr>),

    Unop(Unop, Box<RawExpr>),
    Binop(Binop, Box<RawExpr>, Box<RawExpr>),

    Asm(Vec<RawExpr>, TypeExpr, Rc<str>),

    Raw(Rc<str>),
    Char(char),

    Read(ByteCount, Box<RawExpr>, u32),
    Write(ByteCount, Box<RawExpr>, Box<RawExpr>, u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteCount {
    N1,
    N2,
    N4,
    N8,
}

#[derive(Debug)]
pub enum Unop {
    Negative,
    Positive,
}

impl Unop {
    pub fn from_token(token: Token) -> Option<Self> {
        Some(match token {
            Token::Plus => Self::Positive,
            Token::Minus => Self::Negative,
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Subtract,
    Multiply,
    Divide,
    TruncDivide,
    Remainder,

    Is,
    IsNot,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl Binop {
    pub fn from_token(token: Token) -> Option<Self> {
        Some(match token {
            Token::Plus => Self::Add,
            Token::Minus => Self::Subtract,
            Token::Star => Self::Multiply,
            Token::Slash => Self::Divide,
            Token::Slash2 => Self::TruncDivide,
            Token::Percent => Self::Remainder,

            Token::Lt => Self::LessThan,
            Token::Le => Self::LessThanOrEqual,
            Token::Gt => Self::GreaterThan,
            Token::Ge => Self::GreaterThanOrEqual,
            _ => return None,
        })
    }
}
