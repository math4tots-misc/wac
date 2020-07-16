use crate::ast::*;
use crate::ir::*;
use crate::Error;
use crate::Span;
use std::collections::HashMap;
use std::rc::Rc;

pub trait Scope {
    fn decl(&mut self, name: Rc<str>, item: Item) -> Result<(), Error>;
    fn get(&self, name: &str) -> Option<&Item>;
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
}

impl GlobalScope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
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
    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn decl_local(
        &mut self,
        span: Span,
        name: Rc<str>,
        type_: Type,
    ) -> Result<Rc<Local>, Error> {
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
}

impl<'a> Scope for LocalScope<'a> {
    fn decl(&mut self, name: Rc<str>, item: Item) -> Result<(), Error> {
        if let Some(old_item) = self.stack.last().unwrap().get(&name) {
            return Err(Error {
                span: vec![old_item.span().clone(), item.span().clone()],
                message: format!("Redefinition of {}", name),
            });
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
