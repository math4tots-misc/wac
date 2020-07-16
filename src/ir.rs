use crate::Binop;
use crate::Span;
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::rc::Rc;

pub struct Program {
    pub span: Span,
    pub externs: Vec<Rc<Extern>>,
    pub records: Vec<Rc<Record>>,
    pub globals: Vec<Rc<Global>>,
    pub funcs: Vec<Rc<Func>>,

    /// the local variables needed in the initialization of
    /// global variables
    pub gvar_init_locals: Vec<Rc<Local>>,
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
    Global(Rc<Global>),
}

impl Item {
    pub fn span(&self) -> &Span {
        match self {
            Self::Record(r) => &r.span,
            Self::Func(r) => &r.span,
            Self::Extern(r) => &r.span,
            Self::Local(r) => &r.span,
            Self::Global(r) => &r.span,
        }
    }
}

#[derive(Clone)]
pub enum Variable {
    Local(Rc<Local>),
    Global(Rc<Global>),
}

impl Variable {
    pub fn type_(&self) -> &Type {
        match self {
            Self::Local(var) => &var.type_,
            Self::Global(var) => &var.type_,
        }
    }

    pub fn wasm_kind(&self) -> &'static str {
        match self {
            Self::Local(_) => "local",
            Self::Global(_) => "global",
        }
    }

    pub fn wasm_name(&self) -> String {
        match self {
            Self::Local(var) => format!("$l/{}/{}", var.id, var.name),
            Self::Global(var) => format!("$g/{}", var.name),
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
    pub fn wasm(&self) -> WasmType {
        match self {
            Self::Bool => WasmType::i32,
            Self::I32 => WasmType::i32,
            Self::I64 => WasmType::i64,
            Self::F32 => WasmType::f32,
            Self::F64 => WasmType::f64,
            Self::Record(_) => WasmType::i32,
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

pub struct Global {
    pub span: Span,
    pub name: Rc<str>,
    pub type_: Type,
    pub init: Expr,
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
    GetVar(Variable),
    SetVar(Variable, Box<Expr>),
    AugVar(Variable, TypedWasmOp, Box<Expr>),
    CallFunc(Rc<Func>, Vec<Expr>),
    CallExtern(Rc<Extern>, Vec<Expr>),

    Op(TypedWasmOp, Vec<Expr>),

    Asm(Vec<Expr>, Type, Rc<str>),

    Read1(Box<Expr>, u32),
    Read2(Box<Expr>, u32),
    Read4(Box<Expr>, u32),
    Read8(Box<Expr>, u32),

    Write1(Box<Expr>, Box<Expr>, u32),
    Write2(Box<Expr>, Box<Expr>, u32),
    Write4(Box<Expr>, Box<Expr>, u32),
    Write8(Box<Expr>, Box<Expr>, u32),

    DropPrimitive(Box<Expr>),
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum WasmType {
    i32,
    i64,
    f32,
    f64,
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum UntypedWasmOp {
    clz,
    ctz,
    popcnt,
    add,
    sub,
    mul,
    div_s,
    div_u,
    div,
    rem_s,
    rem_u,
    and,
    or,
    xor,
    shl,
    shr_s,
    shr_u,
    rotl,
    rotr,
    abs,
    neg,
    sqrt,
    ceil,
    floor,
    trunc,
    nearest,
    min,
    max,
    copysign,
    eqz,
    eq,
    ne,
    lt,
    lt_s,
    lt_u,
    gt,
    gt_s,
    gt_u,
    le,
    le_s,
    le_u,
    ge,
    ge_s,
    ge_u,
    convert_i32_s,
    convert_i64_s,
}

impl UntypedWasmOp {
    pub fn from_binop_for_int(op: Binop) -> Option<Self> {
        Some(match op {
            // returns operand type
            Binop::Add => Self::add,
            Binop::Subtract => Self::sub,
            Binop::Multiply => Self::mul,
            Binop::Remainder => Self::rem_s,
            Binop::TruncDivide => Self::div_s,

            // returns bool
            Binop::LessThan => Self::lt_s,
            Binop::LessThanOrEqual => Self::le_s,
            Binop::GreaterThan => Self::gt_s,
            Binop::GreaterThanOrEqual => Self::ge_s,
            _ => return None,
        })
    }

    pub fn from_binop_for_float(op: Binop) -> Option<Self> {
        Some(match op {
            // returns operand type
            Binop::Add => Self::add,
            Binop::Subtract => Self::sub,
            Binop::Multiply => Self::mul,
            Binop::Divide => Self::div,

            // returns bool
            Binop::LessThan => Self::lt,
            Binop::LessThanOrEqual => Self::le,
            Binop::GreaterThan => Self::gt,
            Binop::GreaterThanOrEqual => Self::ge,
            _ => return None,
        })
    }
}

pub struct TypedWasmOp {
    pub type_: WasmType,
    pub op: UntypedWasmOp,
}

impl fmt::Display for TypedWasmOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{:?}", self.type_, self.op)
    }
}
