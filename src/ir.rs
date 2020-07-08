use crate::SSpan;
use std::fmt;
use std::rc::Rc;

pub struct File {
    pub imports: Vec<Import>,
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
    pub traits: Vec<Trait>,
    pub impls: Vec<Impl>,
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
}

impl ConstValue {
    pub fn type_(&self) -> Type {
        match self {
            ConstValue::I32(_) => Type::I32,
            ConstValue::Type(_) => Type::Type,
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
    pub members: Vec<Rc<str>>,
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
    If(SSpan, Vec<(Expr, Expr)>, Box<Expr>),
    While(SSpan, Box<Expr>, Box<Expr>),

    // builtin operators
    Binop(SSpan, Binop, Box<Expr>, Box<Expr>),
    Unop(SSpan, Unop, Box<Expr>),
    AssertType(SSpan, Type, Box<Expr>),

    // intrinsics
    CString(SSpan, Rc<str>),
    Asm(SSpan, Vec<Expr>, ReturnType, Rc<str>),
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
            Expr::If(span, ..) => span,
            Expr::While(span, ..) => span,
            Expr::Binop(span, ..) => span,
            Expr::Unop(span, ..) => span,
            Expr::AssertType(span, ..) => span,
            Expr::CString(span, ..) => span,
            Expr::Asm(span, ..) => span,
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
pub const TAG_TYPE: i32 = 6;
pub const TAG_STRING: i32 = 7;
pub const TAG_LIST: i32 = 8;
pub const TAG_ID: i32 = 9;

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

    /// Reference counted str type
    /// i32 that points to:
    ///   [refcnt i32][size i32][utf8...]
    String = TAG_STRING,

    /// Reference counted list type
    /// i32 that points to:
    ///   [refcnt i32][size i32][capacity i32][utf8...]
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
    String,
    List,
    Id,

    /// User defined type
    /// The tag is (the given u16 value) + 1 + TAG_ID
    ///
    /// odd tag means it's an enum,
    /// even tag means it's a record
    UserDefined(u16),
}

impl Type {
    pub fn first_enum_tag() -> i32 {
        let first_user_type = Type::UserDefined(0);
        if first_user_type.is_enum() {
            first_user_type.tag()
        } else {
            let second_user_type = Type::UserDefined(1);
            assert!(second_user_type.is_enum());
            second_user_type.tag()
        }
    }
    pub fn first_record_tag() -> i32 {
        let first_user_type = Type::UserDefined(0);
        if first_user_type.is_record() {
            first_user_type.tag()
        } else {
            let second_user_type = Type::UserDefined(1);
            assert!(second_user_type.is_record());
            second_user_type.tag()
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
            Type::String => TAG_STRING,
            Type::List => TAG_LIST,
            Type::Id => TAG_ID,
            Type::UserDefined(offset) => TAG_ID + 1 + (offset as i32),
        }
    }
    pub fn is_enum(self) -> bool {
        match self {
            Type::UserDefined(_) if self.tag() % 2 == 1 => true,
            _ => false,
        }
    }
    pub fn is_record(self) -> bool {
        match self {
            Type::UserDefined(_) if !self.is_enum() => true,
            _ => false,
        }
    }
    pub fn name(&self) -> &'static str {
        match self {
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Bool => "bool",
            Type::Type => "type",
            Type::String => "str",
            Type::List => "list",
            Type::Id => "id",
            Type::UserDefined(_) => {
                if self.is_enum() {
                    "enum"
                } else {
                    "record"
                }
            }
        }
    }
    pub fn primitive(self) -> bool {
        match self {
            Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Bool | Type::Type => true,
            Type::String | Type::List | Type::Id | Type::UserDefined(_) => false,
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
            Type::String => WasmType::I32,
            Type::List => WasmType::I32,
            Type::Id => WasmType::I64,
            Type::UserDefined(_) => WasmType::I32,
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

    /// if true, this is a Trait,
    /// otherwise, this is a normal function
    pub dynamic: bool,
}

impl From<Type> for ReturnType {
    fn from(t: Type) -> Self {
        Self::Value(t)
    }
}
