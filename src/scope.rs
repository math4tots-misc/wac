use crate::ast::*;
use crate::ir::*;
use crate::Error;
use crate::Span;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub trait Scope {
    fn decl(&mut self, name: Rc<str>, item: Item) -> Result<(), Error>;
    fn get(&self, name: &str) -> Option<&Item>;
    fn declconst(&mut self, span: Span, name: Rc<str>, value: ConstVal) -> Result<(), Error> {
        let decl = Rc::new(Constant {
            span,
            name: name.clone(),
            value,
        });
        self.decl(name, Item::Constant(decl))?;
        Ok(())
    }
    fn get_return_type(&self, span: &Span, name: &str) -> Result<ReturnType, Error> {
        match self.get(name) {
            Some(item) => match item {
                Item::Record(rec) => Ok(ReturnType::Type(Type::Record(rec.clone()))),
                _ => Err(Error {
                    span: vec![span.clone(), item.span().clone()],
                    message: format!("{} is not a type", name),
                }),
            },
            None => match name {
                "bool" => Ok(ReturnType::Type(Type::Bool)),
                "i32" => Ok(ReturnType::Type(Type::I32)),
                "i64" => Ok(ReturnType::Type(Type::I64)),
                "f32" => Ok(ReturnType::Type(Type::F32)),
                "f64" => Ok(ReturnType::Type(Type::F64)),
                "str" => Ok(ReturnType::Type(Type::Str)),
                "id" => Ok(ReturnType::Type(Type::Id)),
                "void" => Ok(ReturnType::Void),
                "noreturn" => Ok(ReturnType::NoReturn),
                _ => Err(Error {
                    span: vec![span.clone()],
                    message: format!("type {} not found", name),
                }),
            },
        }
    }
    fn get_callable(&self, span: &Span, name: &str) -> Result<Callable, Error> {
        match self.get(name) {
            Some(Item::Func(func)) => Ok(Callable::Func(func.clone())),
            Some(Item::Extern(func)) => Ok(Callable::Extern(func.clone())),
            Some(item) => Err(Error {
                span: vec![span.clone(), item.span().clone()],
                message: format!("{} is not a function", name),
            }),
            None => Err(Error {
                span: vec![span.clone()],
                message: format!("function {} not found", name),
            }),
        }
    }
    fn get_constant(&self, span: &Span, name: &str) -> Result<&Rc<Constant>, Error> {
        match self.get(name) {
            Some(Item::Constant(constant)) => Ok(constant),
            Some(item) => Err(Error {
                span: vec![span.clone(), item.span().clone()],
                message: format!("{} is not a constant", name),
            }),
            None => Err(Error {
                span: vec![span.clone()],
                message: format!("constant {} not found", name),
            }),
        }
    }
    fn get_variable_or_constant(
        &self,
        span: &Span,
        name: &str,
    ) -> Result<VariableOrConstant, Error> {
        match self.get(name) {
            Some(Item::Local(var)) => {
                Ok(VariableOrConstant::Variable(Variable::Local(var.clone())))
            }
            Some(Item::Global(var)) => {
                Ok(VariableOrConstant::Variable(Variable::Global(var.clone())))
            }
            Some(Item::Constant(constant)) => Ok(VariableOrConstant::Constant(constant.clone())),
            Some(item) => Err(Error {
                span: vec![span.clone(), item.span().clone()],
                message: format!("{} is not a variable or constant", name),
            }),
            None => Err(Error {
                span: vec![span.clone()],
                message: format!("variable or constant {} not found", name),
            }),
        }
    }
    fn get_variable(&self, span: &Span, name: &str) -> Result<Variable, Error> {
        match self.get(name) {
            Some(Item::Local(var)) => Ok(Variable::Local(var.clone())),
            Some(Item::Global(var)) => Ok(Variable::Global(var.clone())),
            Some(item) => Err(Error {
                span: vec![span.clone(), item.span().clone()],
                message: format!("{} is not a variable", name),
            }),
            None => Err(Error {
                span: vec![span.clone()],
                message: format!("variable {} not found", name),
            }),
        }
    }
    fn resolve_return_type(&self, texpr: &TypeExpr) -> Result<ReturnType, Error> {
        self.get_return_type(&texpr.span, &texpr.name)
    }
    fn resolve_type(&self, texpr: &TypeExpr) -> Result<Type, Error> {
        match self.resolve_return_type(texpr)? {
            ReturnType::Type(t) => Ok(t),
            ReturnType::Void | ReturnType::NoReturn => Err(Error {
                span: vec![texpr.span.clone()],
                message: format!("void/noreturn types are not allowed here"),
            }),
        }
    }
    fn resolve_func_type(&self, ftexpr: &FuncTypeExpr) -> Result<FuncType, Error> {
        let mut parameters = Vec::new();
        for param in &ftexpr.parameters {
            parameters.push((param.0.clone(), self.resolve_type(&param.1)?));
        }
        let return_type = self.resolve_return_type(&ftexpr.return_type)?;
        Ok(FuncType {
            parameters,
            return_type,
        })
    }
}

pub struct GlobalScope {
    map: HashMap<Rc<str>, Item>,
    memory: Rc<RefCell<Memory>>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            memory: Rc::new(RefCell::new(Memory::new())),
        }
    }
    pub fn declvar(
        &mut self,
        span: Span,
        name: Rc<str>,
        type_: Type,
        init: Expr,
    ) -> Result<Rc<Global>, Error> {
        let global = Rc::new(Global {
            span,
            name: name.clone(),
            type_,
            init,
        });
        self.decl(name, Item::Global(global.clone()))?;
        Ok(global)
    }
    pub fn memory(&self) -> &Rc<RefCell<Memory>> {
        &self.memory
    }
}

impl Scope for GlobalScope {
    fn decl(&mut self, name: Rc<str>, item: Item) -> Result<(), Error> {
        if let Some(old_item) = self.map.get(&name) {
            return Err(Error {
                span: vec![old_item.span().clone(), item.span().clone()],
                message: format!("Redefinition of {}", name),
            });
        }
        self.map.insert(name, item);
        Ok(())
    }

    fn get(&self, name: &str) -> Option<&Item> {
        self.map.get(name)
    }
}

pub struct LocalScope<'a> {
    g: &'a mut GlobalScope,
    func: Option<&'a Rc<Func>>,
    stack: Vec<HashMap<Rc<str>, Item>>,
    locals: Vec<Rc<Local>>,
}

impl<'a> LocalScope<'a> {
    pub fn new(g: &'a mut GlobalScope, func: Option<&'a Rc<Func>>) -> Self {
        Self {
            g,
            func,
            stack: vec![HashMap::new()],
            locals: vec![],
        }
    }
    pub fn memory(&self) -> &Rc<RefCell<Memory>> {
        &self.g.memory
    }
    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn gscope(&mut self) -> &mut GlobalScope {
        self.g
    }

    pub fn declvar(&mut self, span: Span, name: Rc<str>, type_: Type) -> Result<Rc<Local>, Error> {
        let id = self.locals.len();
        let local = Rc::new(Local {
            span,
            name: name.clone(),
            type_,
            id,
        });
        self.decl(name, Item::Local(local.clone()))?;
        Ok(local)
    }

    pub fn return_type(&self) -> Option<&ReturnType> {
        self.func.map(|f| &f.type_.return_type)
    }

    pub fn locals(&self) -> &Vec<Rc<Local>> {
        &self.locals
    }
}

impl<'a> Scope for LocalScope<'a> {
    fn decl(&mut self, name: Rc<str>, item: Item) -> Result<(), Error> {
        if let Some(old_item) = self.stack.last().unwrap().get(&name) {
            return Err(Error {
                span: vec![old_item.span().clone(), item.span().clone()],
                message: format!("Redefinition of {}", name),
            });
        }
        if let Item::Local(local) = &item {
            self.locals.push(local.clone());
        }
        self.stack.last_mut().unwrap().insert(name, item);
        Ok(())
    }

    fn get(&self, name: &str) -> Option<&Item> {
        for map in self.stack.iter().rev() {
            if let Some(item) = map.get(name) {
                return Some(item);
            }
        }
        self.g.get(name)
    }
}
