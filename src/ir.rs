use crate::get_name_for_enum_type_with_offset;
use crate::get_name_for_record_type_with_offset;
use crate::get_user_defined_type_from_name;
use crate::list_all_enum_types;
use crate::list_all_record_types;
use crate::llir::*;
use crate::SSpan;
use std::fmt;
use std::rc::Rc;

pub struct File {
    pub imports: Vec<Import>,
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub traits: Vec<Trait>,
    pub impls: Vec<Impl>,
    pub enums: Vec<Enum>,
    pub records: Vec<Record>,
    pub globalvars: Vec<GlobalVariable>,
}

pub struct FunctionImport {
    pub span: SSpan,
    pub module_name: Rc<str>,
    pub function_name: Rc<str>,
    pub alias: Rc<str>,
    pub type_: FunctionType,
}

pub struct Constant {
    pub span: SSpan,
    pub name: Rc<str>,
    pub value: ConstValue,
}

#[derive(Debug, Clone)]
pub enum ConstValue {
    I32(i32),
    Type(Type),
    Enum(Type, i32),
}

impl ConstValue {
    pub fn type_(&self) -> Type {
        match self {
            ConstValue::I32(_) => Type::I32,
            ConstValue::Type(_) => Type::Type,
            ConstValue::Enum(type_, _) => *type_,
        }
    }
}

pub enum Visibility {
    Private,
    Public,
}

pub struct Enum {
    pub span: SSpan,
    pub name: Rc<str>,
    pub members: Vec<Rc<str>>,
}

pub struct Record {
    pub span: SSpan,
    pub name: Rc<str>,
    pub fields: Vec<(Rc<str>, Type)>,
}

pub struct Trait {
    pub span: SSpan,
    pub name: Rc<str>,
    pub type_: FunctionType,
}

pub struct Impl {
    pub span: SSpan,
    pub receiver_type: Type,
    pub trait_name: Rc<str>,
    pub type_: FunctionType,
    pub body: Expr,
}

pub struct Function {
    pub span: SSpan,
    pub visibility: Visibility,
    pub name: Rc<str>,
    pub type_: FunctionType,
    pub body: Expr,
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
    AssociatedFunctionCall(SSpan, Box<Expr>, Rc<str>, Vec<Expr>),
    If(SSpan, Vec<(Expr, Expr)>, Box<Expr>),
    While(SSpan, Box<Expr>, Box<Expr>),
    For(SSpan, Rc<str>, Box<Expr>, Box<Expr>, Box<Expr>),

    GetAttr(SSpan, Box<Expr>, Rc<str>),
    GetItem(SSpan, Box<Expr>, Box<Expr>),
    SetItem(SSpan, Box<Expr>, Box<Expr>, Box<Expr>),

    Switch(
        SSpan,
        Box<Expr>,
        Vec<(Vec<ConstValue>, Expr)>,
        Option<Box<Expr>>,
    ),

    // Create a new record value
    New(SSpan, Type, Vec<Expr>),

    // builtin operators
    Binop(SSpan, Binop, Box<Expr>, Box<Expr>),
    Unop(SSpan, Unop, Box<Expr>),
    AscribeType(SSpan, Box<Expr>, Type),

    // intrinsics
    CString(SSpan, Rc<str>),
    Asm(SSpan, Vec<Expr>, ReturnType, Rc<str>),

    // memory reading intrinsics
    // reads/writes the number of bytes as specified in their names
    Read1(SSpan, Box<Expr>),
    Read2(SSpan, Box<Expr>),
    Read4(SSpan, Box<Expr>),
    Read8(SSpan, Box<Expr>),
    Write1(SSpan, Box<Expr>, Box<Expr>),
    Write2(SSpan, Box<Expr>, Box<Expr>),
    Write4(SSpan, Box<Expr>, Box<Expr>),
    Write8(SSpan, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn span(&self) -> &SSpan {
        match self {
            Expr::Bool(span, ..) => span,
            Expr::Int(span, ..) => span,
            Expr::Float(span, ..) => span,
            Expr::String(span, ..) => span,
            Expr::List(span, ..) => span,
            Expr::GetVar(span, ..) => span,
            Expr::SetVar(span, ..) => span,
            Expr::DeclVar(span, ..) => span,
            Expr::Block(span, ..) => span,
            Expr::FunctionCall(span, ..) => span,
            Expr::AssociatedFunctionCall(span, ..) => span,
            Expr::If(span, ..) => span,
            Expr::While(span, ..) => span,
            Expr::For(span, ..) => span,
            Expr::GetAttr(span, ..) => span,
            Expr::GetItem(span, ..) => span,
            Expr::SetItem(span, ..) => span,
            Expr::Switch(span, ..) => span,
            Expr::New(span, ..) => span,
            Expr::Binop(span, ..) => span,
            Expr::Unop(span, ..) => span,
            Expr::AscribeType(span, ..) => span,
            Expr::CString(span, ..) => span,
            Expr::Asm(span, ..) => span,
            Expr::Read1(span, ..) => span,
            Expr::Read2(span, ..) => span,
            Expr::Read4(span, ..) => span,
            Expr::Read8(span, ..) => span,
            Expr::Write1(span, ..) => span,
            Expr::Write2(span, ..) => span,
            Expr::Write4(span, ..) => span,
            Expr::Write8(span, ..) => span,
        }
    }
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

impl Import {
    pub fn span(&self) -> &SSpan {
        match self {
            Import::Function(i) => &i.span,
        }
    }
}

pub const TAG_I32: i32 = 1;
pub const TAG_I64: i32 = 2;
pub const TAG_F32: i32 = 3;
pub const TAG_F64: i32 = 4;
pub const TAG_BOOL: i32 = 5;
pub const TAG_TYPE: i32 = 6;
pub const TAG_BYTES: i32 = 7;
pub const TAG_STRING: i32 = 8;
pub const TAG_LIST: i32 = 9;
pub const TAG_ID: i32 = 10;

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    I32 = TAG_I32,
    I64 = TAG_I64,
    F32 = TAG_F32,
    F64 = TAG_F64,
    Bool = TAG_BOOL,

    /// a primitive i32 type that uniquely identifies
    /// a type (in practice, the type tag)
    Type = TAG_TYPE,

    Bytes = TAG_BYTES,

    /// Reference counted str type
    /// i32 that points to:
    ///   [refcnt i32][size i32][utf8...]
    String = TAG_STRING,

    /// Reference counted list type
    /// i32 that points to:
    ///   [refcnt i32][size i32][capacity i32][ptr i32]
    List = TAG_LIST,

    /// i64 value that can represent all types except
    /// other i64 types
    ///
    /// This always needs to be last one among the BuiltinTypes
    /// (i.e. TAG_ID has to be the largest among the builtin
    /// TAG_* values), because UserDefined types assume this
    /// to determine its tags
    Id = TAG_ID,
}

#[repr(i32)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    // builtin types
    I32,
    I64,
    F32,
    F64,
    Bool,
    Type,
    Bytes,
    String,
    List,
    Id,
    Enum(u16),
    Record(u16),
}

impl Type {
    pub fn from_name(name: &str) -> Option<Type> {
        match name {
            "i32" => Some(Type::I32),
            "i64" => Some(Type::I64),
            "f32" => Some(Type::F32),
            "f64" => Some(Type::F64),
            "bool" => Some(Type::Bool),
            "type" => Some(Type::Type),
            "bytes" => Some(Type::Bytes),
            "str" => Some(Type::String),
            "list" => Some(Type::List),
            "id" => Some(Type::Id),
            _ => get_user_defined_type_from_name(name),
        }
    }
    pub fn tag(self) -> i32 {
        match self {
            Type::I32 => TAG_I32,
            Type::I64 => TAG_I64,
            Type::F32 => TAG_F32,
            Type::F64 => TAG_F64,
            Type::Bool => TAG_BOOL,
            Type::Type => TAG_TYPE,
            Type::Bytes => TAG_BYTES,
            Type::String => TAG_STRING,
            Type::List => TAG_LIST,
            Type::Id => TAG_ID,
            // enums always have an odd tag
            Type::Enum(offset) => {
                if (TAG_ID + 1) % 2 == 1 {
                    (TAG_ID + 1) + 2 * (offset as i32)
                } else {
                    (TAG_ID + 2) + 2 * (offset as i32)
                }
            }
            // records always have an even tag
            Type::Record(offset) => {
                if (TAG_ID + 1) % 2 == 0 {
                    (TAG_ID + 1) + 2 * (offset as i32)
                } else {
                    (TAG_ID + 2) + 2 * (offset as i32)
                }
            }
        }
    }
    pub fn is_enum(self) -> bool {
        if let Type::Enum(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_record(self) -> bool {
        if let Type::Record(_) = self {
            true
        } else {
            false
        }
    }
    pub fn list_builtins() -> Vec<Type> {
        vec![
            Type::I32,
            Type::I64,
            Type::F32,
            Type::F64,
            Type::Bool,
            Type::Type,
            Type::Bytes,
            Type::String,
            Type::List,
            Type::Id,
        ]
    }
    /// list all known types
    pub fn list() -> Vec<Type> {
        let mut ret = Self::list_builtins();
        ret.extend(list_all_enum_types());
        ret.extend(list_all_record_types());
        ret
    }
    pub fn name(&self) -> Rc<str> {
        match self {
            Type::I32 => "i32".into(),
            Type::I64 => "i64".into(),
            Type::F32 => "f32".into(),
            Type::F64 => "f64".into(),
            Type::Bool => "bool".into(),
            Type::Type => "type".into(),
            Type::Bytes => "bytes".into(),
            Type::String => "str".into(),
            Type::List => "list".into(),
            Type::Id => "id".into(),
            Type::Enum(offset) => get_name_for_enum_type_with_offset(*offset),
            Type::Record(offset) => get_name_for_record_type_with_offset(*offset),
        }
    }
    pub fn builtin_primitive(self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Bool | Type::Type => true,
            Type::Bytes
            | Type::String
            | Type::List
            | Type::Id
            | Type::Enum(_)
            | Type::Record(_) => false,
        }
    }
    pub fn primitive(self) -> bool {
        match self {
            Type::I32
            | Type::I64
            | Type::F32
            | Type::F64
            | Type::Bool
            | Type::Type
            | Type::Enum(_) => true,
            Type::Bytes | Type::String | Type::List | Type::Id | Type::Record(_) => false,
        }
    }
    pub fn wasm(self) -> WasmType {
        match self {
            Type::I32 => WasmType::I32,
            Type::I64 => WasmType::I64,
            Type::F32 => WasmType::F32,
            Type::F64 => WasmType::F64,
            Type::Bool => WasmType::I32,
            Type::Type => WasmType::I32,
            Type::Bytes => WasmType::I32,
            Type::String => WasmType::I32,
            Type::List => WasmType::I32,
            Type::Id => WasmType::I64,
            Type::Enum(_) => WasmType::I32,
            Type::Record(_) => WasmType::I32,
        }
    }
}

impl From<BuiltinType> for Type {
    fn from(t: BuiltinType) -> Self {
        match t {
            BuiltinType::I32 => Self::I32,
            BuiltinType::I64 => Self::I64,
            BuiltinType::F32 => Self::F32,
            BuiltinType::F64 => Self::F64,
            BuiltinType::Bool => Self::Bool,
            BuiltinType::Type => Self::Type,
            BuiltinType::Bytes => Self::Bytes,
            BuiltinType::String => Self::String,
            BuiltinType::List => Self::List,
            BuiltinType::Id => Self::Id,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReturnType {
    // This branch is the most 'typical' case
    // where the function or expression returns a
    // real normal value
    Value(Type),

    // "Universal receiver" type
    //
    // Any expression may be used when a void type is
    // expected.
    // However, void value cannot be used in place of any other type
    //
    // void means that the function returns no value when
    // it returns
    // like 'void' in C
    Void,

    // "Universal donor" type
    //
    // An expression of type noreturn may be used
    // no matter what type is required.
    // However, when a NoReturn is expected, no other type may be
    // accepted
    //
    // NoReturn means that the function never actually
    // returns
    // like '!' in Rust
    NoReturn,
}

impl fmt::Display for ReturnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReturnType::Value(t) => t.fmt(f),
            ReturnType::Void => write!(f, "void"),
            ReturnType::NoReturn => write!(f, "noreturn"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub parameters: Vec<(Rc<str>, Type)>,
    pub return_type: ReturnType,

    /// Indicates whether filename, lineno should be stored in the stacktrace
    /// whenever this function is called.
    /// If the function is known to never panic or inspect the stack trace,
    /// it may be better for performance to set this to false.
    /// By default, this is true.
    pub trace: bool,
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.trace {
            write!(f, "[notrace]")?;
        }
        write!(f, "(")?;
        for (i, (name, typ)) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{} {}", name, typ)?;
        }
        write!(f, ") {}", self.return_type)?;
        Ok(())
    }
}

impl From<Type> for ReturnType {
    fn from(t: Type) -> Self {
        Self::Value(t)
    }
}
