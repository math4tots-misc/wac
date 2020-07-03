use crate::Span;
use std::rc::Rc;

/// The real underlying type in webassembly
/// LLTypes all resolve to a corresponding
/// LLValueType
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LLValueType {
    I32,
    I64,
    F32,
    F64,
}

/// Type as perceived by wac
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LLType {
    I32,
    I64,
    F32,
    F64,

    // String is an i32 that points to a location in memory
    // containing:
    // [i32 string-class][i32 ref-count] [i32 capacity (bytes)] [i32 size (bytes)] [utf8-chars...]
    // If capacity is 0, ref-count should also always just be 0, and indicates
    // that the string is immutable
    String,

    // Almost any type.
    // A 64-bit value that can represent any type except:
    //   * 64-bit values (f64, i64),
    //   * function types
    // the first i32 is a tag, indicating the type of value,
    // the last i32 is the actual value (this works, because
    // pointers are 32-bits here)
    Id,

    // Function is an i32 that points to the webassembly table
    Function(Box<LLFunctionType>),
}

impl LLType {
    pub fn to_value_type(&self) -> LLValueType {
        match self {
            LLType::I32 => LLValueType::I32,
            LLType::F32 => LLValueType::F32,
            LLType::I64 => LLValueType::I64,
            LLType::F64 => LLValueType::F64,
            LLType::String => LLValueType::I32,
            LLType::Id => LLValueType::I64,
            LLType::Function(_) => LLValueType::I32,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LLFunctionType {
    pub(crate) parameters: Vec<LLType>,
    pub(crate) return_type: LLType,
}

pub enum LLVisibility {
    Public,
    Private,
}

pub struct LLFunction {
    pub(crate) span: Span,
    pub(crate) visibility: LLVisibility,
    pub(crate) name: Rc<str>,
    pub(crate) parameters: Vec<(Rc<str>, LLType)>,
    pub(crate) return_type: LLType,
    pub(crate) locals: Vec<(Rc<str>, LLType)>,
    pub(crate) body: LLExpr,
}

#[derive(Debug)]
pub enum LLExpr {
    Int(Span, i64),
    Float(Span, f64),
    String(Span, Rc<str>),
    GetVar(Span, Rc<str>),
    SetVar(Span, Rc<str>, Box<LLExpr>),
    FunctionCall(Span, Rc<str>, Vec<LLExpr>),
    Block(Span, Vec<LLExpr>, Option<Box<LLExpr>>),

    /// Inline assembly
    ///     The expressions passed as arguments are assumed
    ///     to be consumed by the inline assembly.
    ///     It is assumed that a single value will be pushed
    ///     onto the stack once done.
    InlineAsm(Span, Vec<(LLType, LLExpr)>, LLType, Rc<str>),
}

impl LLExpr {
    pub fn span(&self) -> Span {
        match self {
            LLExpr::Int(span, ..) => *span,
            LLExpr::Float(span, ..) => *span,
            LLExpr::String(span, ..) => *span,
            LLExpr::GetVar(span, ..) => *span,
            LLExpr::SetVar(span, ..) => *span,
            LLExpr::FunctionCall(span, ..) => *span,
            LLExpr::Block(span, ..) => *span,
            LLExpr::InlineAsm(span, ..) => *span,
        }
    }
}

pub struct LLFunctionImport {
    pub(crate) span: Span,
    pub(crate) module_name: Rc<str>,
    pub(crate) function_name: Rc<str>,
    pub(crate) imported_name: Rc<str>,
    pub(crate) type_: LLFunctionType,
}

pub enum LLImport {
    Function(LLFunctionImport),
}

pub struct LLFile {
    pub(crate) name: Rc<str>,
    pub(crate) imports: Vec<LLImport>,
    pub(crate) functions: Vec<LLFunction>,
}
