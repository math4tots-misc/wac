use crate::make_import_object;
use crate::parse_files;
use crate::translate;
use crate::translate_files;
use crate::Error;
use crate::Visibility;
use crate::MALLOC_CHECK;
use std::io::Write;
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

/// given a list of (filename, wac-code) pairs,
/// runs all tests
pub fn run_tests(sources: Vec<(Rc<str>, Rc<str>)>, test_prefix: &str) -> Result<(), Error> {
    let test_func_prefix = format!("__test_{}", test_prefix);
    let base_prefix_len = "__test_".len();

    println!("Test filter: __test_{}", test_prefix);

    let start = std::time::Instant::now();
    let mut test_names = Vec::new();
    let mut files = parse_files(sources)?;
    for (_filename, file) in &mut files {
        for func in &mut file.functions {
            if func.name.starts_with(&test_func_prefix) {
                // force test functions to be public, even if it wasn't originally
                // declared so
                let test_name = (&func.name[base_prefix_len..]).to_owned();
                test_names.push(test_name);
                func.visibility = Visibility::Public;
            }
        }
    }
    let file_loading_time = start.elapsed().as_secs_f64();
    let start = std::time::Instant::now();
    let wat_code = translate_files(files)?;
    let wat_code_size = wat_code.len();
    let wac_to_wat_time = start.elapsed().as_secs_f64();

    let start = std::time::Instant::now();
    let wasm_code = wabt::wat2wasm(&wat_code)?;
    let wat_to_wasm_time = start.elapsed().as_secs_f64();
    let wasm_code_size = wasm_code.len();

    let start = std::time::Instant::now();
    let import_object = make_import_object();
    let instance = wr::instantiate(&wasm_code, &import_object)?;
    let instantiate_time = start.elapsed().as_secs_f64();

    let start = std::time::Instant::now();
    println!("Running {} tests", test_names.len());
    for test_name in &test_names {
        print!("  test {}... ", test_name);
        std::io::stdout().flush().unwrap();
        let f: wr::Func<(), ()> = instance.exports.get(&format!("f___test_{}", test_name))?;
        f.call()?;
        println!("ok");
    }
    print!("  releasing global vars... ");
    // releasing
    let f: wr::Func<(), ()> = instance.exports.get("rt_release_globals")?;
    f.call()?;
    println!("ok");
    let exec_time = start.elapsed().as_secs_f64();

    println!("All tests passed");

    println!("file loading time  : {}s", file_loading_time);
    println!("wac to wat time    : {}s", wac_to_wat_time);
    println!("wat to wasm time   : {}s", wat_to_wasm_time);
    println!("instantiate time   : {}s", instantiate_time);
    println!("execution time     : {}s", exec_time);
    println!(
        "wat code size      : {:.3}kb",
        (wat_code_size as f64) / (2.0f64.powi(10))
    );
    println!(
        "wasm code size     : {:.3}kb",
        (wasm_code_size as f64) / (2.0f64.powi(10))
    );

    MALLOC_CHECK.with(|check| {
        let check = check.borrow();
        println!(
            "max heap usage     : {:.3}kb",
            (check.max_heap_usage as f64) / (2.0f64.powi(10))
        );
        println!("malloc cnt         : {}", check.malloc_cnt);
        println!("free cnt           : {}", check.free_cnt);
        if check.map.len() != 0 {
            println!("REMAINING ALLOCATIONS: ");
            let mut pairs: Vec<_> = check.map.iter().collect();
            pairs.sort_by(|a, b| a.0.cmp(&b.0));
            for (ptr, len) in pairs {
                println!("    {}, {}", ptr, len);
            }
        }
    });

    Ok(())
}
