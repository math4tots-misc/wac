use crate::Span;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::rc::Rc;

pub struct Program {
    pub span: Span,
    pub externs: Vec<Rc<Extern>>,
    pub records: Vec<Rc<Record>>,
    pub funcs: Vec<Rc<Func>>,
}

#[derive(Debug, Clone)]
pub enum ReturnState {
    NeverReturns,
    MaybeReturns,
    AlwaysReturns,
    Unreachable,
}

impl ReturnState {
    pub fn and_then(&self, other: &Self) -> Self {
        match (self, other) {
            (ReturnState::Unreachable, _)
            | (_, ReturnState::Unreachable)
            | (ReturnState::AlwaysReturns, _) => ReturnState::Unreachable,
            (ReturnState::NeverReturns, _) => other.clone(),
            (_, ReturnState::AlwaysReturns) => ReturnState::AlwaysReturns,
            (ReturnState::MaybeReturns, ReturnState::NeverReturns)
            | (ReturnState::MaybeReturns, ReturnState::MaybeReturns) => ReturnState::MaybeReturns,
        }
    }
}

#[derive(Clone)]
pub enum Item {
    Record(Rc<Record>),
    Func(Rc<Func>),
    Extern(Rc<Extern>),
    Local(Rc<Local>),
}

impl Item {
    pub fn span(&self) -> &Span {
        match self {
            Self::Record(r) => &r.span,
            Self::Func(r) => &r.span,
            Self::Extern(r) => &r.span,
            Self::Local(r) => &r.span,
        }
    }
}

pub enum Callable {
    Func(Rc<Func>),
    Extern(Rc<Extern>),
}

impl Callable {
    pub fn span(&self) -> &Span {
        match self {
            Self::Func(r) => &r.span,
            Self::Extern(r) => &r.span,
        }
    }
    pub fn name(&self) -> &Rc<str> {
        match self {
            Self::Func(r) => &r.name,
            Self::Extern(r) => &r.name,
        }
    }
    pub fn type_(&self) -> &FuncType {
        match self {
            Self::Func(r) => &r.type_,
            Self::Extern(r) => &r.type_,
        }
    }
}

pub struct Record {
    pub span: Span,
    pub name: Rc<str>,
    pub fields: RefCell<Vec<(Rc<str>, Type)>>,
}

impl fmt::Debug for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Record({})", self.name)
    }
}

impl cmp::PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    I32,
    I64,
    F32,
    F64,
    Record(Rc<Record>),
}

impl Type {
    pub fn wasm(&self) -> &str {
        match self {
            Self::Bool => "i32",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Record(_) => "i32",
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Record(rec) => write!(f, "{}", rec.name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReturnType {
    Type(Type),
    Void,
    NoReturn,
}

impl fmt::Display for ReturnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(t) => write!(f, "{}", t),
            Self::Void => write!(f, "void"),
            Self::NoReturn => write!(f, "noreturn"),
        }
    }
}

impl ReturnType {
    pub fn value(&self) -> Option<&Type> {
        match self {
            Self::Type(t) => Some(t),
            _ => None,
        }
    }
}

impl From<Type> for ReturnType {
    fn from(t: Type) -> Self {
        Self::Type(t)
    }
}

#[derive(PartialEq)]
pub struct FuncType {
    pub parameters: Vec<(Rc<str>, Type)>,
    pub return_type: ReturnType,
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, (name, typ)) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} {}", name, typ)?;
        }
        write!(f, "){}", self.return_type)
    }
}

pub struct Extern {
    pub span: Span,
    pub path: (Rc<str>, Rc<str>),
    pub name: Rc<str>,
    pub type_: FuncType,
}

pub struct Func {
    pub span: Span,
    pub name: Rc<str>,
    pub type_: FuncType,
    pub parameters: RefCell<Vec<Rc<Local>>>,
    pub locals: RefCell<Vec<Rc<Local>>>,
    pub body: RefCell<Option<Stmt>>,
}

pub struct Local {
    pub span: Span,
    pub name: Rc<str>,
    pub id: usize,
    pub type_: Type,
}

pub struct Stmt {
    pub span: Span,
    pub return_state: ReturnState,
    pub data: StmtData,
}

pub enum StmtData {
    Block(Vec<Stmt>),
    Return(Expr),
    Expr(Expr),
}

pub struct Expr {
    pub span: Span,
    pub type_: ReturnType,
    pub data: ExprData,
}

pub enum ExprData {
    Void,
    Bool(bool),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    GetLocal(Rc<Local>),
    SetLocal(Rc<Local>, Box<Expr>),
    CallFunc(Rc<Func>, Vec<Expr>),
    CallExtern(Rc<Extern>, Vec<Expr>),

    Asm(Vec<Expr>, Type, Rc<str>),

    Read1(Box<Expr>),
    Read2(Box<Expr>),
    Read4(Box<Expr>),
    Read8(Box<Expr>),

    Write1(Box<Expr>, Box<Expr>),
    Write2(Box<Expr>, Box<Expr>),
    Write4(Box<Expr>, Box<Expr>),
    Write8(Box<Expr>, Box<Expr>),

    DropPrimitive(Box<Expr>),
}
