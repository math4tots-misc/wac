//! Just testing out wasmer_runtime
extern crate anyhow;
extern crate wabt;
extern crate wasmer_runtime as wr;

fn main() -> Result<(), Error> {
    let import_object = wr::imports! {};

    let code = wabt::wat2wasm(
        r##"
    (module
        (func $add_one (param $x i64) (result i64)
            get_local $x
            i64.const 1
            i64.add
        )
        (export "add_one" (func $add_one))
    )
    "##,
    )?;
    let instance = wr::instantiate(&code, &import_object)?;
    let add_one: wr::Func<i64, i64> = instance.exports.get("add_one")?;
    let value = add_one.call(42)?;
    println!("value = {:?}", value);

    Ok(())
}

#[derive(Debug)]
enum Error {
    Wabt(wabt::Error),
    Wasmer(wr::error::Error),
}

impl From<wabt::Error> for Error {
    fn from(e: wabt::Error) -> Self {
        Self::Wabt(e)
    }
}

impl From<wr::error::Error> for Error {
    fn from(e: wr::error::Error) -> Self {
        Self::Wasmer(e.into())
    }
}

impl From<wr::error::ResolveError> for Error {
    fn from(e: wr::error::ResolveError) -> Self {
        Self::Wasmer(e.into())
    }
}

impl From<wr::error::RuntimeError> for Error {
    fn from(e: wr::error::RuntimeError) -> Self {
        Self::Wasmer(e.into())
    }
}
