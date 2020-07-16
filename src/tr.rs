use crate::parse;
use crate::solve;
use crate::Error;
use crate::Source;
use std::rc::Rc;

pub fn translate(sources: Vec<Rc<Source>>) -> Result<String, Error> {
    let mut files = Vec::new();
    for source in &sources {
        files.push(parse(source)?);
    }
    let program = solve(&files)?;
    program.wat()
}
