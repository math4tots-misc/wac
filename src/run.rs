use crate::compile;
use crate::parse;
use crate::Error;
use crate::CompileError;
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

const PRELUDE_STR: &'static str = include_str!("prelude.wac");

pub fn run<N: Into<Rc<str>>, D: AsRef<str>>(name_data_pairs: Vec<(N, D)>) -> Result<i32, Error> {
    let import_object = wr::imports! {
        "lang" => {
            "print_i64" => wr::func!(|x: i64| -> i64 {
                println!("{}", x);
                0
            }),
        }
    };

    let mut files = Vec::new();
    for (name, data) in name_data_pairs {
        let name = name.into();
        files.push(match parse(name.clone(), data.as_ref()) {
            Ok(result) => result,
            Err(error) => {
                return Err(CompileError::Parse(name, error).into());
            }
        });
    }
    files.push(match parse("<prelude>".into(), PRELUDE_STR) {
        Ok(result) => result,
        Err(error) => return Err(CompileError::Parse("<prelude>".into(), error).into()),
    });
    let file_refs: Vec<_> = files.iter().collect();
    let wat_module_string = compile(file_refs)?;
    let wasm_code = wabt::wat2wasm(&wat_module_string)?;
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    let main: wr::Func<(), i32> = instance.exports.get("main")?;
    Ok(main.call()?)
}
