mod er;
mod imp;
mod ir;
mod lexer;
mod parsef;
mod parser;
mod rn;
mod sink;
mod start;
mod tr;
mod prelude;

extern crate wasmer_runtime as wr;

pub use er::*;
pub use imp::*;
pub use ir::*;
pub use lexer::*;
pub use parsef::*;
pub use parser::*;
pub use rn::*;
pub use sink::*;
pub use start::*;
pub use tr::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
