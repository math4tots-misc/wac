use crate::make_import_object;
use crate::translate;
use crate::Error;
use crate::Source;
use std::rc::Rc;
use std::time::Instant;

pub fn run(sources: Vec<Rc<Source>>) -> Result<RunStats, Error> {
    let import_object = make_import_object();

    let start = Instant::now();
    let wat_code = translate(sources)?;
    let translate_sec = start.elapsed().as_secs_f64();
    let wat_code_size = wat_code.len();

    let start = Instant::now();
    let wasm_code = wabt::wat2wasm(&wat_code)?;
    let wat2wasm_sec = start.elapsed().as_secs_f64();
    let wasm_code_size = wasm_code.len();

    let start = Instant::now();
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    let instantiate_sec = start.elapsed().as_secs_f64();

    let start = Instant::now();
    let main: wr::Func<(), ()> = instance.exports.get("Main")?;
    main.call()?;
    let exec_sec = start.elapsed().as_secs_f64();

    Ok(RunStats {
        translate_sec,
        wat2wasm_sec,
        instantiate_sec,
        exec_sec,
        wat_code_size,
        wasm_code_size,
    })
}

pub struct RunStats {
    pub translate_sec: f64,
    pub wat2wasm_sec: f64,
    pub instantiate_sec: f64,
    pub exec_sec: f64,
    pub wat_code_size: usize,
    pub wasm_code_size: usize,
}

impl RunStats {
    pub fn format(&self) -> String {
        use std::fmt::Write;
        let mut out = String::new();
        writeln!(out, "translate time    : {}s", self.translate_sec).unwrap();
        writeln!(out, "wat2wasm time     : {}s", self.wat2wasm_sec).unwrap();
        writeln!(out, "instantiate time  : {}s", self.instantiate_sec).unwrap();
        writeln!(out, "execution time    : {}s", self.exec_sec).unwrap();
        writeln!(
            out,
            "wat code size     : {:.3}kb",
            (self.wat_code_size as f64) / 2.0f64.powi(10)
        )
        .unwrap();
        writeln!(
            out,
            "wasm code size    : {:.3}kb",
            (self.wasm_code_size as f64) / 2.0f64.powi(10)
        )
        .unwrap();
        out
    }
}
