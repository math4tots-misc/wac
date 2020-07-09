use crate::run;
use crate::run_tests;
use crate::translate;
use std::path::Path;
use std::rc::Rc;

pub fn main() {
    let mut mode = Mode::Run;
    let mut sources = Vec::<(Rc<str>, Rc<str>)>::new();
    let mut test_prefix = String::new();
    for arg in std::env::args().skip(1) {
        let arg: &str = &arg;
        match arg {
            "-c" => {
                mode = Mode::CompileOnly;
            }
            _ if arg.starts_with("-t") => {
                mode = Mode::Test;
                test_prefix = arg["-t".len()..].to_owned();
            }
            _ => {
                add_sources(&mut sources, arg).unwrap();
            }
        }
    }

    match mode {
        Mode::Run => match run(sources) {
            Ok(exitcode) => {
                std::process::exit(exitcode);
            }
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        },
        Mode::Test => match run_tests(sources, &test_prefix) {
            Ok(()) => {}
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        },
        Mode::CompileOnly => match translate(sources) {
            Ok(string) => println!("{}", string),
            Err(error) => {
                eprintln!("{}", error.format());
                std::process::exit(1);
            }
        },
    }
}

enum Mode {
    Run,
    Test,
    CompileOnly,
}

fn add_sources(out: &mut Vec<(Rc<str>, Rc<str>)>, path_str: &str) -> Result<(), std::io::Error> {
    let path = Path::new(path_str);
    if path.is_dir() {
        add_sources_rec(out, "", path)?;
    } else if path.is_file() {
        // at the top level, if a user specifies a file, even if it has the wrong file
        // extension, accept it as a source
        let data = std::fs::read_to_string(path).unwrap();
        out.push((path_str.into(), data.into()));
    }
    Ok(())
}

fn add_sources_rec(
    out: &mut Vec<(Rc<str>, Rc<str>)>,
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
        for entry in path.read_dir()? {
            let entry = entry?;
            let file_path = entry.path();
            add_sources_rec(out, base, &file_path)?;
        }
    } else if path.is_file() {
        if file_name.ends_with(".wac") {
            let data = std::fs::read_to_string(path).unwrap();
            out.push((base.into(), data.into()));
        }
    }
    Ok(())
}
