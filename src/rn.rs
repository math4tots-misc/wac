use crate::make_import_object;
use crate::translate;
use crate::Error;
use std::rc::Rc;

/// given a list of (filename, wac-code) pairs,
/// runs it
pub fn run(sources: Vec<(Rc<str>, Rc<str>)>) -> Result<i32, Error> {
    let import_object = make_import_object();
    let wat_code = translate(sources)?;
    let wasm_code = wabt::wat2wasm(&wat_code)?;
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    let main: wr::Func<(), i32> = instance.exports.get("f_main")?;
    Ok(main.call()?)
}
