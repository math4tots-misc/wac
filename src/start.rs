use crate::run;
use crate::translate;
use crate::translate_to_wasm;
use crate::RunConfig;
use crate::Source;
use std::path::Path;
use std::rc::Rc;

pub fn main() {
    let mut mode = Mode::Run;
    let mut sources = Vec::<Rc<Source>>::new();
    let mut run_config = RunConfig::default();
    // let mut test_prefix = String::new();
    for arg in std::env::args().skip(1) {
        let arg: &str = &arg;
        match arg {
            "-c" => {
                mode = Mode::CompileOnly;
            }
            "-w" => {
                mode = Mode::Wasm;
            }
            _ if arg.starts_with("-O") => {
                let text = &arg["-O".len()..];
                let optlevel: Result<u32, _> = text.parse();
                match optlevel {
                    Ok(0) => run_config.optimize = None,
                    Ok(1) => run_config.optimize = Some(1),
                    Ok(2) => run_config.optimize = Some(2),
                    _ => panic!("Invalid opt level {:?} (must be 0, 1, or 2)", optlevel),
                }
            }
            // _ if arg.starts_with("-t") => {
            //     mode = Mode::Test;
            //     test_prefix = arg["-t".len()..].to_owned();
            // }
            _ => {
                add_sources(&mut sources, arg).unwrap();
            }
        }
    }

    match mode {
        Mode::Run => match run(sources, run_config) {
            Ok(stats) => eprintln!("{}", stats.format()),
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        },
        // Mode::Test => match run_tests(sources, &test_prefix) {
        //     Ok(()) => {}
        //     Err(error) => {
        //         eprintln!("{}", error.format());
        //         std::process::exit(1);
        //     }
        // },
        Mode::Wasm => match translate_to_wasm(sources, run_config) {
            Ok((code, stats)) => {
                use std::io::Write;
                std::io::stdout().write(&code).unwrap();
                eprintln!("{}", stats.format());
            }
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        }
        Mode::CompileOnly => match translate(sources) {
            Ok(string) => print!("{}", string),
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        },
    }
}

enum Mode {
    Run,
    Wasm,
    // Test,
    CompileOnly,
}

fn add_sources(out: &mut Vec<Rc<Source>>, path_str: &str) -> Result<(), std::io::Error> {
    let path = Path::new(path_str);
    if path.is_dir() {
        add_sources_rec(out, "", path)?;
    } else if path.is_file() {
        // at the top level, if a user specifies a file, even if it has the wrong file
        // extension, accept it as a source
        let data = std::fs::read_to_string(path).unwrap();
        out.push(Rc::new(Source {
            name: path_str.into(),
            data: data.into(),
        }));
    }
    Ok(())
}

fn add_sources_rec(
    out: &mut Vec<Rc<Source>>,
    base: &str,
    path: &Path,
) -> Result<(), std::io::Error> {
    let file_name = path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("[??]")
        .to_owned();
    let base: &str = &format!("{}/{}", base, file_name);
    if path.is_dir() {
        // ensure that the order in which we read the sources
        // is deterministic
        let mut subpaths = Vec::new();
        for entry in path.read_dir()? {
            let entry = entry?;
            subpaths.push(entry.path());
        }
        subpaths.sort();
        for subpath in subpaths {
            add_sources_rec(out, base, &subpath)?;
        }
    } else if path.is_file() {
        if file_name.ends_with(".wac") {
            let data = std::fs::read_to_string(path).unwrap();
            out.push(Rc::new(Source {
                name: base.into(),
                data: data.into(),
            }));
        }
    }
    Ok(())
}
