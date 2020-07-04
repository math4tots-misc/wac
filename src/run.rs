use crate::compile;
use crate::Error;
// use crate::RESERVED_FOR_MALLOC;
use std::path::Path;
use std::rc::Rc;

pub fn run_files<'a, P: Into<&'a Path>, I: IntoIterator<Item = P>>(ip: I) -> Result<i32, Error> {
    let mut name_data_pairs = Vec::new();
    for p in ip {
        let path = p.into();
        let data = std::fs::read_to_string(&path)?;
        name_data_pairs.push((format!("{:?}", path), data));
    }
    run(name_data_pairs)
}

pub fn run_or_panic<N: Into<Rc<str>>, D: AsRef<str>>(name_data_pairs: Vec<(N, D)>) -> i32 {
    match run(name_data_pairs) {
        Ok(x) => x,
        Err(error) => {
            panic!("{:?}", error);
        }
    }
}

pub fn run<N: Into<Rc<str>>, D: AsRef<str>>(name_data_pairs: Vec<(N, D)>) -> Result<i32, Error> {
    let import_object = crate::exc::make_import_object();
    let wat_module_string = compile(name_data_pairs)?;
    let wasm_code = wabt::wat2wasm(&wat_module_string)?;
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    let main: wr::Func<(), i32> = instance.exports.get("f_main")?;
    Ok(main.call()?)
}

pub fn compile_files<'a, P: Into<&'a Path>, I: IntoIterator<Item = P>>(
    ip: I,
) -> Result<String, Error> {
    let mut name_data_pairs = Vec::new();
    for p in ip {
        let path = p.into();
        let data = std::fs::read_to_string(&path)?;
        name_data_pairs.push((format!("{:?}", path), data));
    }
    compile_only(name_data_pairs)
}

pub fn compile_only<N: Into<Rc<str>>, D: AsRef<str>>(
    name_data_pairs: Vec<(N, D)>,
) -> Result<String, Error> {
    let wat_module_string = compile(name_data_pairs)?;
    Ok(wat_module_string)
}

pub fn main() {
    match main0() {
        Ok(()) => {}
        Err(error) => panic!("{:?}", error),
    }
}

fn main0() -> Result<(), Error> {
    enum State {
        Normal,
        Mode,
    }
    let mut state = State::Normal;

    enum Mode {
        Run,
        Compile,
    }
    let mut mode = Mode::Run;

    let mut paths = Vec::<std::path::PathBuf>::new();

    for arg in std::env::args().skip(1) {
        let s: &str = &arg;
        match state {
            State::Normal => match s {
                "-m" | "--mode" => state = State::Mode,
                _ => paths.push(arg.into()),
            },
            State::Mode => {
                match s {
                    "run" | "r" => mode = Mode::Run,
                    "compile" | "c" => mode = Mode::Compile,
                    _ => panic!("Mode must be 'run' or 'compile' but got {}", s),
                }
                state = State::Normal;
            }
        }
    }
    let paths: Vec<&Path> = paths.iter().map(|p| p.as_ref()).collect();

    match mode {
        Mode::Run => {
            run_files(paths)?;
        }
        Mode::Compile => {
            let text = compile_files(paths)?;
            println!("{}", text);
        }
    }
    Ok(())
}
