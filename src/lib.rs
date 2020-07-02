extern crate wabt;

mod lexer;
mod llcompiler;
mod llir;
mod llparser;

pub use lexer::lex;
pub use lexer::LexError;
pub use lexer::Span;
pub use lexer::Token;
pub use llcompiler::compile;
pub use llir::LLExpr;
pub use llir::LLFile;
pub use llir::LLFunction;
pub use llir::LLFunctionImport;
pub use llir::LLFunctionType;
pub use llir::LLImport;
pub use llir::LLType;
pub use llir::LLValueType;
pub use llir::LLVisibility;
pub use llparser::parse;
pub use llparser::ParseError;

#[cfg(test)]
mod tests {
    use wabt::wat2wasm;

    #[test]
    fn wat2wasm_works() {
        assert_eq!(
            wat2wasm("(module)").unwrap(),
            &[
                0, 97, 115, 109, // \0ASM - magic
                1, 0, 0, 0 //  0x01 - version
            ]
        );
    }
}
