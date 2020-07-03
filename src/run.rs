use crate::compile;
use crate::Error;
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
    let import_object = wr::imports! {
        "lang" => {
            "print_i64" => wr::func!(|x: i64| -> i64 {
                println!("{}", x);
                0
            }),
        }
    };
    let wat_module_string = compile(name_data_pairs)?;
    let wasm_code = wabt::wat2wasm(&wat_module_string)?;
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    let main: wr::Func<(), i32> = instance.exports.get("f_main")?;
    Ok(main.call()?)
}
