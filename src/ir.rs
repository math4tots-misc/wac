use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct Source {
    name: Rc<String>,
    data: Rc<String>,
}

#[derive(Debug, Clone)]
pub struct Span {
    source: Rc<Source>,
    main: usize,
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    I32(i32),
    F32(f32),
    String(Rc<String>),
    List(Rc<RefCell<Vec<Value>>>),
}

impl Value {
    pub fn type_(&self) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::I32(_) => Type::I32,
            Value::F32(_) => Type::F32,
            Value::String(_) => Type::String,
            Value::List(_) => Type::List,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Bool,
    I32,
    F32,
    String,
    List,

    // Id type is actually almost any type,
    // except:
    //   * other 64-bit values (i.e. f64, i64)
    // this is to enable use in wasm32, where
    // pointers are 32-bits
    Id,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    return_type: Type,
    parameter_types: Vec<Type>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

pub struct Function {
    pub span: Span,
    pub visibility: Visibility,
    pub name: Rc<String>,
    pub parameters: Vec<(Rc<String>, Type)>,
    pub return_type: Type,

    // this field is filled in by the annotate_types phase
    pub locals: Vec<(Rc<String>, Type)>,

    pub body: Expr,
}

impl Function {
    pub fn type_(&self) -> FunctionType {
        FunctionType {
            return_type: self.return_type,
            parameter_types: self.parameters.iter().map(|(_, t)| *t).collect(),
        }
    }
}

pub struct GlobalVar {
    pub span: Span,
    pub visibility: Visibility,
    pub name: Rc<String>,
    pub type_: Type,
    pub init: Expr,
}

pub enum Expr {
    Bool(Span, bool),
    Int(Span, i64),
    Float(Span, f64),
    String(Span, Rc<String>),
    List(Span, Vec<Expr>),
    FunctionCall(Span, Rc<String>, Vec<Expr>),
    Block(Span, Vec<Expr>),
    DeclVar(Span, Rc<String>, Type, Box<Expr>),
    SetVar(Span, Rc<String>, Box<Expr>),
    GetVar(Span, Rc<String>),
    If(Span, Box<Expr>, Box<Expr>, Box<Expr>),
    While(Span, Box<Expr>, Box<Expr>),

    Typed(TypedExpr),
    Flat(Vec<FlatExpr>),
}

pub enum TypedExpr {
    Void(Span),
    Const(Span, Value),
    List(Span, Vec<TypedExpr>),
    FunctionCall(Span, Rc<FunctionInfo>, Vec<TypedExpr>),
    Block(Span, Vec<TypedExpr>),

    DeclVar(Rc<LocalVarInfo>, Box<TypedExpr>),
    SetLocal(Span, Rc<LocalVarInfo>, Box<TypedExpr>),
    GetLocal(Span, Rc<LocalVarInfo>),
    SetGlobal(Span, Rc<GlobalVarInfo>, Box<TypedExpr>),
    GetGlobal(Span, Rc<GlobalVarInfo>),

    If(Span, Box<TypedExpr>, Box<TypedExpr>, Box<TypedExpr>),
    While(Span, Box<TypedExpr>, Box<TypedExpr>),

    IntToFloat(Span, Box<TypedExpr>),
    FloatToInt(Span, Box<TypedExpr>),

    /// downcasts always cast from Id
    Downcast(Span, Box<TypedExpr>, Type),

    /// Upcasts always cast up to Id
    Upcast(Span, Box<TypedExpr>),

    VoidCast(Span, Box<TypedExpr>),
}

impl TypedExpr {
    pub fn type_(&self) -> Type {
        match self {
            TypedExpr::Void(..) => Type::Void,
            TypedExpr::Const(_, value) => value.type_(),
            TypedExpr::List(..) => Type::List,
            TypedExpr::FunctionCall(_, info, ..) => info.type_.return_type,
            TypedExpr::Block(_, exprs) => match exprs.last() {
                Some(e) => e.type_(),
                None => Type::Void,
            },
            TypedExpr::DeclVar(..) => Type::Void,
            TypedExpr::SetLocal(..) => Type::Void,
            TypedExpr::GetLocal(_, info) => info.type_,
            TypedExpr::SetGlobal(..) => Type::Void,
            TypedExpr::GetGlobal(_, info) => info.type_,
            TypedExpr::If(_, _, body, _) => body.type_(),
            TypedExpr::While(..) => Type::Void,
            TypedExpr::IntToFloat(..) => Type::F32,
            TypedExpr::FloatToInt(..) => Type::I32,
            TypedExpr::Downcast(_, _, t) => *t,
            TypedExpr::Upcast(..) => Type::Id,
            TypedExpr::VoidCast(..) => Type::Void,
        }
    }

    fn rec_flatten(self, buf: &mut Vec<FlatExpr>) {
        match self {
            TypedExpr::Void(_) => {}
            TypedExpr::Const(span, value) => buf.push(FlatExpr::Const(span, value)),
            TypedExpr::List(span, exprs) => {
                let n = exprs.len();
                for expr in exprs {
                    expr.rec_flatten(buf);
                }
                buf.push(FlatExpr::List(span, n as u32));
            }
            TypedExpr::FunctionCall(span, info, argexprs) => {
                let n = argexprs.len();
                for argexpr in argexprs {
                    argexpr.rec_flatten(buf);
                }
                buf.push(FlatExpr::FunctionCall(span, info, n as u32));
            }
            TypedExpr::Block(_, exprs) => {
                for expr in exprs {
                    expr.rec_flatten(buf);
                }
            }
            TypedExpr::DeclVar(info, expr) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::DeclVar(info));
            }
            TypedExpr::SetLocal(span, info, expr) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::SetLocal(span, info));
            }
            TypedExpr::GetLocal(span, info) => {
                buf.push(FlatExpr::GetLocal(span, info));
            }
            TypedExpr::SetGlobal(span, info, expr) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::SetGlobal(span, info));
            }
            TypedExpr::GetGlobal(span, info) => {
                buf.push(FlatExpr::GetGlobal(span, info));
            }
            TypedExpr::If(span, cond, body, other) => {
                cond.rec_flatten(buf);
                let jmp1 = buf.len();
                buf.push(FlatExpr::JumpIfFalse(span.clone(), 0));
                body.rec_flatten(buf);
                let jmp2 = buf.len();
                buf.push(FlatExpr::Jump(span, 0));
                other.rec_flatten(buf);
                let end = buf.len() as u32;

                buf[jmp1].update_jump(end);
                buf[jmp2].update_jump(end);
            }
            TypedExpr::While(span, cond, body) => {
                let start = buf.len();
                cond.rec_flatten(buf);
                let jmp1 = buf.len();
                buf.push(FlatExpr::JumpIfFalse(span.clone(), 0));
                body.rec_flatten(buf);
                buf.push(FlatExpr::Jump(span.clone(), start as u32));
                let end = buf.len();

                buf[jmp1].update_jump(end as u32);
            }
            TypedExpr::IntToFloat(span, expr) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::IntToFloat(span));
            }
            TypedExpr::FloatToInt(span, expr) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::FloatToInt(span));
            }
            TypedExpr::Downcast(span, expr, type_) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::Downcast(span, type_));
            }
            TypedExpr::Upcast(span, expr) => {
                expr.rec_flatten(buf);
                buf.push(FlatExpr::Upcast(span));
            }
            TypedExpr::VoidCast(span, expr) => {
                assert_ne!(expr.type_(), Type::Void);
                expr.rec_flatten(buf);
                buf.push(FlatExpr::Pop(span));
            }
        }
    }

    pub fn flatten(self) -> Vec<FlatExpr> {
        let mut buf = Vec::new();
        self.rec_flatten(&mut buf);
        buf
    }
}

/// expressions flattened like bytecode
#[derive(Debug)]
pub enum FlatExpr {
    Pop(Span),
    Void(Span),
    Const(Span, Value),
    List(Span, u32),
    FunctionCall(Span, Rc<FunctionInfo>, u32),
    DeclVar(Rc<LocalVarInfo>),
    SetLocal(Span, Rc<LocalVarInfo>),
    TeeLocal(Span, Rc<LocalVarInfo>),
    GetLocal(Span, Rc<LocalVarInfo>),
    SetGlobal(Span, Rc<GlobalVarInfo>),
    TeeGlobal(Span, Rc<GlobalVarInfo>),
    GetGlobal(Span, Rc<GlobalVarInfo>),
    Jump(Span, u32),
    JumpIfTrue(Span, u32),
    JumpIfFalse(Span, u32),
    TeeJumpIfTrue(Span, u32),
    TeeJumpIfFalse(Span, u32),
    IntToFloat(Span),
    FloatToInt(Span),
    Downcast(Span, Type),
    Upcast(Span),
    VoidCast(Span),
}

impl FlatExpr {
    fn update_jump(&mut self, new_jump_loc: u32) {
        match self {
            FlatExpr::Jump(_, loc) => *loc = new_jump_loc,
            FlatExpr::JumpIfTrue(_, loc) => *loc = new_jump_loc,
            FlatExpr::JumpIfFalse(_, loc) => *loc = new_jump_loc,
            FlatExpr::TeeJumpIfTrue(_, loc) => *loc = new_jump_loc,
            FlatExpr::TeeJumpIfFalse(_, loc) => *loc = new_jump_loc,
            _ => panic!("update_jump on non-jump ({:?})", self),
        }
    }
}

pub enum Import {
    Function(FunctionImport),
}

pub struct FunctionImport {
    pub span: Span,
    pub module_name: Rc<String>,
    pub function_name: Rc<String>,
    pub alias: Rc<String>,
    pub type_: FunctionType,
}

pub struct Module {
    pub imports: Vec<Import>,
    pub globalvars: Vec<GlobalVar>,
    pub functions: Vec<Function>,
}

impl Module {
    pub fn merge(mut self, modules: Vec<Module>) -> Module {
        for module in modules {
            self.imports.extend(module.imports);
            self.functions.extend(module.functions);
        }
        self
    }
    pub fn resolve_all(&mut self) -> Result<(), Error> {
        self.annotate_types()?;
        self.flatten_expressions();
        Ok(())
    }
    fn annotate_types(&mut self) -> Result<(), Error> {
        use std::mem::replace;
        let gscope = GlobalScope::new(self)?;
        for gl in &mut self.globalvars {
            let mut lscope = gscope.local();
            let span = gl.span.clone();
            let dummyexpr = Expr::Int(span, 0);
            let etype = gl.type_;
            gl.init = Expr::Typed(annotate_expr(
                &mut lscope,
                etype,
                replace(&mut gl.init, dummyexpr),
            )?);
        }
        for func in &mut self.functions {
            let mut lscope = gscope.local();
            let span = func.span.clone();
            let dummyexpr = Expr::Int(span, 0);
            let etype = func.return_type;
            func.body = Expr::Typed(annotate_expr(
                &mut lscope,
                etype,
                replace(&mut func.body, dummyexpr),
            )?);
            for (i, decl) in lscope.decls.into_iter().enumerate() {
                assert_eq!(i, decl.id);
                func.locals.push((decl.name, decl.type_));
            }
        }
        Ok(())
    }
    fn flatten_expressions(&mut self) {
        use std::mem::replace;
        for gl in &mut self.globalvars {
            let dummyexpr = Expr::Int(gl.span.clone(), 0);
            gl.init = Expr::Flat(match replace(&mut gl.init, dummyexpr) {
                Expr::Typed(texpr) => texpr.flatten(),
                _ => panic!("annotate_types must be called before flatten_expressions"),
            });
        }
        for func in &mut self.functions {
            let dummyexpr = Expr::Int(func.span.clone(), 0);
            func.body = Expr::Flat(match replace(&mut func.body, dummyexpr) {
                Expr::Typed(texpr) => texpr.flatten(),
                _ => panic!("annotate_types must be called before flatten_expressions"),
            });
        }
    }
}

fn annotate_expr(scope: &mut LocalScope, etype: Type, e: Expr) -> Result<TypedExpr, Error> {
    Ok(match e {
        Expr::Bool(span, value) => {
            let rexpr = TypedExpr::Const(span.clone(), Value::Bool(value));
            match etype {
                Type::Void => TypedExpr::Void(span),
                Type::Bool => rexpr,
                Type::Id => TypedExpr::Upcast(span, rexpr.into()),
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "Bool".to_owned().into(),
                    })
                }
            }
        }
        Expr::Int(span, value) => {
            let rexpr = TypedExpr::Const(span.clone(), Value::I32(value as i32));
            match etype {
                Type::Void => TypedExpr::Void(span),
                Type::I32 => rexpr,
                Type::Id => TypedExpr::Upcast(span, rexpr.into()),
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "Int".to_owned().into(),
                    })
                }
            }
        }
        Expr::Float(span, value) => {
            let rexpr = TypedExpr::Const(span.clone(), Value::F32(value as f32));
            match etype {
                Type::Void => TypedExpr::Void(span),
                Type::F32 => rexpr,
                Type::Id => TypedExpr::Upcast(span, rexpr.into()),
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "Float".to_owned().into(),
                    })
                }
            }
        }
        Expr::String(span, value) => {
            let rexpr = TypedExpr::Const(span.clone(), Value::String(value));
            match etype {
                Type::Void => TypedExpr::Void(span),
                Type::String => rexpr,
                Type::Id => TypedExpr::Upcast(span, rexpr.into()),
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "String".to_owned().into(),
                    })
                }
            }
        }
        Expr::List(span, exprs) => {
            let mut texprs = Vec::new();
            for expr in exprs {
                let texpr = annotate_expr(scope, Type::List, expr)?;
                texprs.push(texpr);
            }
            let rexpr = TypedExpr::List(span.clone(), texprs);
            match etype {
                Type::Void => TypedExpr::VoidCast(span, rexpr.into()),
                Type::List => rexpr,
                Type::Id => TypedExpr::Upcast(span, rexpr.into()),
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "List".to_owned().into(),
                    })
                }
            }
        }
        Expr::FunctionCall(span, fname, argexprs) => match scope.getf(&fname) {
            Some(finfo) => {
                let finfo = finfo.clone();
                let ftype = &finfo.type_;
                if ftype.parameter_types.len() != argexprs.len() {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{} args", ftype.parameter_types.len()).into(),
                        got: format!("{} args", argexprs.len()).into(),
                    });
                }
                let mut targexprs = Vec::new();
                for (argexpr, ptype) in argexprs.into_iter().zip(&ftype.parameter_types) {
                    let targexpr = annotate_expr(scope, *ptype, argexpr)?;
                    targexprs.push(targexpr);
                }
                let rexpr = TypedExpr::FunctionCall(span.clone(), finfo.clone(), targexprs);
                auto_cast_to(span, rexpr, etype)?
            }
            None => {
                return Err(Error::TypeError {
                    span,
                    expected: "Function".to_owned().into(),
                    got: "NotFound".to_owned().into(),
                })
            }
        },
        Expr::Block(span, mut exprs) => {
            let last = exprs.pop();
            let mut texprs = Vec::new();
            scope.push();
            for expr in exprs {
                let texpr = annotate_expr(scope, Type::Void, expr)?;
                texprs.push(texpr);
            }
            match last {
                Some(last) => {
                    let last = annotate_expr(scope, etype, last)?;
                    scope.pop();
                    texprs.push(last);
                    TypedExpr::Block(span, texprs)
                }
                None => {
                    scope.pop();
                    let rexpr = TypedExpr::Block(span.clone(), texprs);
                    match etype {
                        Type::Void => rexpr,
                        _ => {
                            return Err(Error::TypeError {
                                span,
                                expected: format!("{:?}", etype).into(),
                                got: "empty-block".to_owned().into(),
                            })
                        }
                    }
                }
            }
        }
        Expr::DeclVar(span, name, type_, init) => {
            let info = scope.decl(span.clone(), name, type_);
            let init = annotate_expr(scope, type_, *init)?;
            let rexpr = TypedExpr::DeclVar(info, init.into());
            match etype {
                Type::Void => rexpr,
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "Void (declvar)".to_owned().into(),
                    })
                }
            }
        }
        Expr::SetVar(span, name, setexpr) => {
            let info = scope.get_err(span.clone(), &name)?;
            let setexpr = annotate_expr(scope, info.type_(), *setexpr)?;
            let rexpr = match info {
                VarInfo::Global(info) => TypedExpr::SetGlobal(span.clone(), info, setexpr.into()),
                VarInfo::Local(info) => TypedExpr::SetLocal(span.clone(), info, setexpr.into()),
            };
            match etype {
                Type::Void => rexpr,
                _ => {
                    return Err(Error::TypeError {
                        span,
                        expected: format!("{:?}", etype).into(),
                        got: "Void (setvar)".to_owned().into(),
                    })
                }
            }
        }
        Expr::GetVar(span, name) => {
            let info = scope.get_err(span.clone(), &name)?;
            let rexpr = match info {
                VarInfo::Global(info) => TypedExpr::GetGlobal(span.clone(), info),
                VarInfo::Local(info) => TypedExpr::GetLocal(span.clone(), info),
            };
            auto_cast_to(span, rexpr, etype)?
        }
        Expr::If(span, cond, body, other) => {
            let cond = annotate_expr(scope, Type::Bool, *cond)?;
            let body = annotate_expr(scope, etype, *body)?;
            let other = annotate_expr(scope, etype, *other)?;
            TypedExpr::If(span, cond.into(), body.into(), other.into())
        }
        Expr::While(span, cond, body) => {
            if etype != Type::Void {
                return Err(Error::TypeError {
                    span,
                    expected: format!("{:?}", etype).into(),
                    got: "Void (while)".to_owned().into(),
                })
            }
            let cond = annotate_expr(scope, Type::Bool, *cond)?;
            let body = annotate_expr(scope, Type::Void, *body)?;
            TypedExpr::While(span, cond.into(), body.into())
        }
        Expr::Typed(_) => panic!("annotate_expr: already annotated"),
        Expr::Flat(_) => panic!("annotate_expr: already flattened"),
    })
}

fn auto_cast_to(span: Span, expr: TypedExpr, type_: Type) -> Result<TypedExpr, Error> {
    match (expr.type_(), type_) {
        (src, target) if src == target => Ok(expr),
        (_, Type::Void) => Ok(TypedExpr::VoidCast(span, expr.into())),
        (Type::Id, _) => Ok(TypedExpr::Downcast(span, expr.into(), type_)),
        (_, Type::Id) => Ok(TypedExpr::Upcast(span, expr.into())),
        (Type::I32, Type::F32) => Ok(TypedExpr::IntToFloat(span, expr.into())),
        (src, target) => Err(Error::TypeError {
            span,
            got: format!("{:?}", src).into(),
            expected: format!("{:?}", target).into(),
        })
    }
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub span: Span,
    pub type_: FunctionType,
}

enum VarInfo {
    Global(Rc<GlobalVarInfo>),
    Local(Rc<LocalVarInfo>),
}

impl VarInfo {
    fn type_(&self) -> Type {
        match self {
            VarInfo::Global(info) => info.type_,
            VarInfo::Local(info) => info.type_,
        }
    }
}

#[derive(Debug)]
pub struct GlobalVarInfo {
    pub span: Span,
    pub name: Rc<String>,
    pub type_: Type,
}

#[derive(Debug)]
pub struct LocalVarInfo {
    pub span: Span,

    // the name of the local variable given by the programmer
    pub name: Rc<String>,

    // an id, unique within the function
    pub id: u32,

    // the variable's declared type
    pub type_: Type,
}

struct GlobalScope {
    functions: HashMap<Rc<String>, Rc<FunctionInfo>>,
    globals: HashMap<Rc<String>, Rc<GlobalVarInfo>>,
}

impl GlobalScope {
    fn new(module: &Module) -> Result<Self, Error> {
        let mut functions = HashMap::<Rc<String>, Rc<FunctionInfo>>::new();
        let mut globals = HashMap::<Rc<String>, Rc<GlobalVarInfo>>::new();
        for imp in &module.imports {
            match imp {
                Import::Function(FunctionImport {
                    span,
                    module_name: _,
                    function_name: _,
                    alias,
                    type_,
                }) => {
                    if let Some(old) = functions.get(alias) {
                        return Err(Error::ConflictingDefinitions {
                            span1: old.span.clone(),
                            span2: span.clone(),
                            name: alias.clone(),
                        });
                    }
                    functions.insert(
                        alias.clone(),
                        Rc::new(FunctionInfo {
                            span: span.clone(),
                            type_: type_.clone(),
                        }),
                    );
                }
            }
        }
        for func in &module.functions {
            if let Some(old) = functions.get(&func.name) {
                return Err(Error::ConflictingDefinitions {
                    span1: old.span.clone(),
                    span2: func.span.clone(),
                    name: func.name.clone(),
                });
            }
            functions.insert(
                func.name.clone(),
                Rc::new(FunctionInfo {
                    span: func.span.clone(),
                    type_: func.type_(),
                }),
            );
        }
        for gl in &module.globalvars {
            globals.insert(
                gl.name.clone(),
                Rc::new(GlobalVarInfo {
                    span: gl.span.clone(),
                    name: gl.name.clone(),
                    type_: gl.type_,
                }),
            );
        }
        Ok(Self { functions, globals })
    }

    /// creates a new local scope from this global one
    fn local(&self) -> LocalScope {
        LocalScope {
            g: self,
            locals: vec![HashMap::new()],
            decls: vec![],
        }
    }

    fn getf(&self, name: &Rc<String>) -> Option<&Rc<FunctionInfo>> {
        self.functions.get(name)
    }
}

struct LocalScope<'a> {
    g: &'a GlobalScope,
    locals: Vec<HashMap<Rc<String>, Rc<LocalVarInfo>>>,
    decls: Vec<Rc<LocalVarInfo>>,
}

impl<'a> LocalScope<'a> {
    fn getf(&self, name: &Rc<String>) -> Option<&Rc<FunctionInfo>> {
        self.g.getf(name)
    }
    fn get(&self, name: &Rc<String>) -> Option<VarInfo> {
        match self.g.globals.get(name) {
            Some(info) => Some(VarInfo::Global(info.clone())),
            None => {
                for map in self.locals.iter().rev() {
                    match map.get(name) {
                        Some(info) => return Some(VarInfo::Local(info.clone())),
                        None => {}
                    }
                }
                None
            }
        }
    }
    fn get_err(&self, span: Span, name: &Rc<String>) -> Result<VarInfo, Error> {
        match self.get(&name) {
            Some(info) => Ok(info),
            None => {
                Err(Error::TypeError {
                    span: span.clone(),
                    expected: "Variable".to_owned().into(),
                    got: "NotFound".to_owned().into(),
                })
            }
        }
    }
    fn decl(&mut self, span: Span, name: Rc<String>, type_: Type) -> Rc<LocalVarInfo> {
        let id = self.decls.len() as u32;
        let info = Rc::new(LocalVarInfo {
            span,
            name: name.clone(),
            type_,
            id,
        });
        self.locals.last_mut().unwrap().insert(name, info.clone());
        self.decls.push(info.clone());
        info
    }
    fn push(&mut self) {
        self.locals.push(HashMap::new());
    }
    fn pop(&mut self) {
        self.locals.pop().unwrap();
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    TypeError {
        span: Span,
        expected: Rc<String>,
        got: Rc<String>,
    },
    ConflictingDefinitions {
        span1: Span,
        span2: Span,
        name: Rc<String>,
    },
}
