use crate::SSpan;
use std::rc::Rc;

pub struct File {
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub globalvars: Vec<GlobalVariable>,
}

pub struct FunctionImport {
    pub span: SSpan,
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
    pub span: SSpan,
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

pub struct GlobalVariable {
    pub span: SSpan,
    pub visibility: Visibility,
    pub name: Rc<str>,
    pub type_: Option<Type>,
    pub init: Expr,
}

pub enum Expr {
    Bool(SSpan, bool),
    Int(SSpan, i64),
    Float(SSpan, f64),
    GetVar(SSpan, Rc<str>),
    SetVar(SSpan, Rc<str>, Box<Expr>),
    DeclVar(SSpan, Rc<str>, Option<Type>, Box<Expr>),
    Block(SSpan, Vec<Expr>),
    FunctionCall(SSpan, Rc<str>, Vec<Expr>),
    If(SSpan, Box<Expr>, Box<Expr>, Box<Expr>),
    While(SSpan, Box<Expr>, Box<Expr>),

    // builtin operators
    LessThan(SSpan, Box<Expr>, Box<Expr>),
    Add(SSpan, Box<Expr>, Box<Expr>),

    // intrinsics
    CString(SSpan, Rc<str>),
    Asm(SSpan, Vec<Expr>, Option<Type>, Rc<str>),
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
    Bool,
}

impl Type {
    pub fn wasm(self) -> WasmType {
        match self {
            Type::I32 => WasmType::I32,
            Type::I64 => WasmType::I64,
            Type::F32 => WasmType::F32,
            Type::F64 => WasmType::F64,
            Type::Bool => WasmType::I32,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub return_type: Option<Type>,
}
