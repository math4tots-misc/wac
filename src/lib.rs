mod ir;
mod rt;
mod exec;

pub use ir::*;
pub use rt::*;
pub use exec::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
