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
    String(SSpan, Rc<str>),
    List(SSpan, Vec<Expr>),
    GetVar(SSpan, Rc<str>),
    SetVar(SSpan, Rc<str>, Box<Expr>),
    DeclVar(SSpan, Rc<str>, Option<Type>, Box<Expr>),
    Block(SSpan, Vec<Expr>),
    FunctionCall(SSpan, Rc<str>, Vec<Expr>),
    If(SSpan, Box<Expr>, Box<Expr>, Box<Expr>),
    While(SSpan, Box<Expr>, Box<Expr>),

    // builtin operators
    Binop(SSpan, Binop, Box<Expr>, Box<Expr>),
    Unop(SSpan, Unop, Box<Expr>),
    AssertType(SSpan, Type, Box<Expr>),

    // intrinsics
    CString(SSpan, Rc<str>),
    Asm(SSpan, Vec<Expr>, Option<Type>, Rc<str>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Binop {
    Add,
    Subtract,
    Multiply,
    Divide,
    TruncDivide,
    Remainder,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,

    Is,
    IsNot,
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Unop {
    Plus,
    Minus,
    Not,
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

impl WasmType {
    pub fn wac(self) -> Type {
        match self {
            Self::I32 => Type::I32,
            Self::I64 => Type::I64,
            Self::F32 => Type::F32,
            Self::F64 => Type::F64,
        }
    }
}

pub const TAG_I32: i32 = 1;
pub const TAG_I64: i32 = 2;
pub const TAG_F32: i32 = 3;
pub const TAG_F64: i32 = 4;
pub const TAG_BOOL: i32 = 5;
pub const TAG_STRING: i32 = 6;
pub const TAG_LIST: i32 = 7;
pub const TAG_ID: i32 = 8;

#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    I32 = TAG_I32,
    I64 = TAG_I64,
    F32 = TAG_F32,
    F64 = TAG_F64,
    Bool = TAG_BOOL,

    // Reference counted str type
    // i32 that points to:
    //   [refcnt i32][size i32][utf8...]
    String = TAG_STRING,

    // Reference counted list type
    // i32 that points to:
    //   [refcnt i32][size i32][capacity i32][utf8...]
    List = TAG_LIST,

    // i64 value that can represent all types except
    // other i64 types
    Id = TAG_ID,
}

impl Type {
    pub fn primitive(self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Bool => true,
            Type::String | Type::List | Type::Id => false,
        }
    }
    pub fn wasm(self) -> WasmType {
        match self {
            Type::I32 => WasmType::I32,
            Type::I64 => WasmType::I64,
            Type::F32 => WasmType::F32,
            Type::F64 => WasmType::F64,
            Type::Bool => WasmType::I32,
            Type::String => WasmType::I32,
            Type::List => WasmType::I32,
            Type::Id => WasmType::I64,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub return_type: Option<Type>,
}
