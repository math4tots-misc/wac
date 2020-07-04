use crate::Error;
use crate::FlatExpr;
use crate::FunctionImport;
use crate::Import;
use crate::Module;
use crate::Span;
use crate::Value;
use std::collections::HashMap;
use std::rc::Rc;

pub type ImportedFunction = fn(&mut Wac, Vec<Value>) -> Result<Value, Error>;

pub struct Wac {
    module: Module,
    imported_functions: Vec<ImportedFunction>,
    frames: Vec<Frame>,
}

impl Wac {
    pub fn new(module: Module, funcmap: &HashMap<Rc<String>, ImportedFunction>) -> Self {
        let mut imported_functions = Vec::new();

        for imp in &module.imports {
            match imp {
                Import::Function(FunctionImport {
                    span: _,
                    function_name,
                    alias: _,
                    type_: _,
                }) => match funcmap.get(function_name) {
                    Some(f) => imported_functions.push(*f),
                    None => {
                        panic!("Required imported function {} not found", function_name);
                    }
                },
            }
        }

        Self {
            module,
            imported_functions,
            frames: vec![],
        }
    }
    pub fn module(&self) -> &Module {
        &self.module
    }
    pub fn invoke_local(
        &mut self,
        span: Option<Span>,
        func_index: u32,
        mut args: Vec<Value>,
    ) -> Result<Value, Error> {
        let (ops, nlocals) = {
            let func = &self.module.functions[func_index as usize];
            let ops = func.ops().clone();
            (ops, func.locals.len())
        };
        args.resize_with(nlocals, || Value::I32(0));
        let frame = Frame {
            span,
            locals: args,
            stack: vec![],
        };
        let nframes = self.frames.len();
        self.frames.push(frame);
        let value = eval(self, &ops)?;
        self.frames.truncate(nframes);
        Ok(value)
    }
    pub fn invoke_imported(
        &mut self,
        span: Option<Span>,
        func_index: u32,
        args: Vec<Value>,
    ) -> Result<Value, Error> {
        let f = self.imported_functions[func_index as usize];
        let frame = Frame {
            span,
            locals: vec![],
            stack: vec![],
        };
        let nframes = self.frames.len();
        self.frames.push(frame);
        let value = f(self, args)?;
        self.frames.truncate(nframes);
        Ok(value)
    }
    pub fn get_local_func_index(&self, name: &Rc<String>) -> Option<u32> {
        for (i, func) in self.module.functions.iter().enumerate() {
            if &func.name == name {
                return Some(i as u32)
            }
        }
        None
    }
    pub fn main(&mut self) {
        let i = self.get_local_func_index(&"main".to_owned().into()).expect("main function not found");
        match self.invoke_local(None, i, vec![]) {
            Ok(_) => {},
        }
    }
}

pub struct Frame {
    span: Option<Span>,
    locals: Vec<Value>,
    stack: Vec<Value>,
}

fn eval(wac: &mut Wac, ops: &Vec<FlatExpr>) -> Result<Value, Error> {
    panic!("TODO")
}
