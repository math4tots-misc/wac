mod ir;
mod lexer;
mod rt;
mod wc;

pub use ir::*;
pub use lexer::*;
pub use rt::*;
pub use wc::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
