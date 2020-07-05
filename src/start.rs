use crate::run;
use crate::translate;
use std::path::Path;
use std::rc::Rc;

pub fn main() {
    let mut mode = Mode::Run;
    let mut sources = Vec::<(Rc<str>, Rc<str>)>::new();
    for arg in std::env::args().skip(1) {
        let arg: &str = &arg;
        match arg {
            "-c" => {
                mode = Mode::CompileOnly;
            }
            _ => {
                let path = Path::new(arg);
                let data = std::fs::read_to_string(path).unwrap();
                sources.push((arg.into(), data.into()));
            }
        }
    }

    match mode {
        Mode::Run => match run(sources) {
            Ok(exitcode) => {
                std::process::exit(exitcode);
            }
            Err(error) => {
                panic!("{}", error.format());
            }
        },
        Mode::CompileOnly => match translate(sources) {
            Ok(string) => println!("{}", string),
            Err(error) => {
                panic!("{}", error.format());
            }
        },
    }
}

enum Mode {
    Run,
    CompileOnly,
}
