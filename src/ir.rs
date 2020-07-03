use std::rc::Rc;

pub enum Value {
    Int(i32),
    Float(f64),
    String(Rc<String>),
    List(Vec<Value>),
}

pub enum Expr {
    Int(i32),
    Float(f64),
    String(Rc<String>),
    List(Vec<Expr>),
    FunctionCall {
        name: Rc<String>,
        args: Vec<Expr>,
    },
    Block(Vec<Expr>),
}

pub struct Function {
    name: Rc<String>,
}

pub struct File {
    functions: Vec<Function>,
}
