use crate::Span;
use std::rc::Rc;

pub struct File {
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
}

pub struct FunctionImport {
    pub span: Span,
    pub module_name: Rc<str>,
    pub function_name: Rc<str>,
    pub alias: Rc<str>,
    pub type_: FunctionType,
}

pub enum Visibility {
    Private,
    Public,
}

pub struct Function {
    pub span: Span,
    pub visibility: Visibility,
    pub name: Rc<str>,
    pub parameters: Vec<(Rc<str>, Type)>,
    pub return_type: Option<Type>,
    pub body: Expr,
}

impl Function {
    pub fn type_(&self) -> FunctionType {
        let parameter_types = self.parameters.iter().map(|(_, t)| *t).collect();
        FunctionType {
            parameter_types,
            return_type: self.return_type,
        }
    }
}

pub enum Expr {
    Int(Span, i64),
    Float(Span, f64),
    GetVar(Span, Rc<str>),
    SetVar(Span, Rc<str>, Box<Expr>),
    DeclVar(Span, Rc<str>, Option<Type>, Box<Expr>),
    Block(Span, Vec<Expr>),
    FunctionCall(Span, Rc<str>, Vec<Expr>),
    If(Span, Box<Expr>, Box<Expr>, Box<Expr>),
    While(Span, Box<Expr>, Box<Expr>),

    // builtin operators
    LessThan(Span, Box<Expr>, Box<Expr>),
    Add(Span, Box<Expr>, Box<Expr>),

    // intrinsics
    CString(Span, Rc<str>),
}

pub enum Import {
    Function(FunctionImport),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WasmType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}

impl Type {
    pub fn wasm(self) -> WasmType {
        match self {
            Type::I32 => WasmType::I32,
            Type::I64 => WasmType::I64,
            Type::F32 => WasmType::F32,
            Type::F64 => WasmType::F64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub return_type: Option<Type>,
}
