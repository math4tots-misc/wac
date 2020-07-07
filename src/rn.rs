use crate::make_import_object;
use crate::translate;
use crate::Error;
use std::rc::Rc;

/// given a list of (filename, wac-code) pairs,
/// runs it
pub fn run(sources: Vec<(Rc<str>, Rc<str>)>) -> Result<i32, Error> {
    let import_object = make_import_object();

    // let start = std::time::Instant::now();
    let wat_code = translate(sources)?;
    // println!("wac to wat time: {}s", start.elapsed().as_secs_f64());

    // let start = std::time::Instant::now();
    let wasm_code = wabt::wat2wasm(&wat_code)?;
    // println!("wat to wasm time: {}s", start.elapsed().as_secs_f64());

    // let start = std::time::Instant::now();
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    // println!("instantiate time: {}s", start.elapsed().as_secs_f64());

    // let start = std::time::Instant::now();
    let main: wr::Func<(), i32> = instance.exports.get("f___start")?;
    let ret = Ok(main.call()?);
    // println!("execution time: {}s", start.elapsed().as_secs_f64());

    ret
}
