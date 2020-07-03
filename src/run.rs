use crate::compile;
use crate::Error;
use crate::RESERVED_FOR_MALLOC;
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
            "retain_str" => wr::func!(|ctx: &mut wr::Ctx, str_ptr: wr::WasmPtr<i32>| -> i32 {
                0
            }),
            "release_str" => wr::func!(|ctx: &mut wr::Ctx, str_ptr: wr::WasmPtr<i32>| -> i32 {
                0
            }),
            "puts" => wr::func!(|ctx: &mut wr::Ctx, str_ptr: wr::WasmPtr<i32>| -> i32 {
                let memory = ctx.memory(0);
                let offset = str_ptr.offset();
                let refcnt_cell = str_ptr.deref(memory).unwrap();
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

fn retain_str(ctx: &mut wr::Ctx, str_ptr: wr::WasmPtr<i32>) {
}

fn release_str(ctx: &mut wr::Ctx, str_ptr: wr::WasmPtr<i32>) {
}

/// dead simple freelist implementation. also see free.
///
/// First RESERVED_FOR_MALLOC bytes are reserved for malloc (except
/// the first 4-bytes, which is used for error handling).
fn malloc(ctx: &mut wr::Ctx, size: i32) -> i32 {
    0
}

/// dead simple freelist implementation. also see malloc.
fn free(ctx: &mut wr::Ctx, ptr: wr::WasmPtr<u8>, size: i32) -> i32 {
    0
}
