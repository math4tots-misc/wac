use crate::Span;
use std::rc::Rc;

pub struct File {
    pub span: Span,
    pub externs: Vec<RawExtern>,
    pub records: Vec<RawRecord>,
    pub globals: Vec<RawGlobal>,
    pub funcs: Vec<RawFunc>,
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
    GetVar(Rc<str>),
    SetVar(Rc<str>, Box<RawExpr>),
    CallFunc(Rc<str>, Vec<RawExpr>),

    Asm(Vec<RawExpr>, TypeExpr, Rc<str>),

    Char(char),

    Read1(Box<RawExpr>),
    Read2(Box<RawExpr>),
    Read4(Box<RawExpr>),
    Read8(Box<RawExpr>),

    Write1(Box<RawExpr>, Box<RawExpr>),
    Write2(Box<RawExpr>, Box<RawExpr>),
    Write4(Box<RawExpr>, Box<RawExpr>),
    Write8(Box<RawExpr>, Box<RawExpr>),
}
