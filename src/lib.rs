extern crate wabt;
extern crate wasmer_runtime as wr;

mod lexer;
mod llcompiler;
mod llir;
mod llparser;
mod run;

pub use lexer::lex;
pub use lexer::LexError;
pub use lexer::Span;
pub use lexer::Token;
pub use llcompiler::compile;
pub use llcompiler::CompileError;
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
pub use run::run;
pub use run::run_files;
pub use run::run_or_panic;

pub(crate) use llcompiler::RESERVED_FOR_MALLOC;

#[derive(Debug)]
pub enum Error {
    IO(std::io::Error),
    Compile(CompileError),
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

impl From<CompileError> for Error {
    fn from(e: CompileError) -> Self {
        Self::Compile(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::IO(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wat2wasm_works() {
        assert_eq!(
            wabt::wat2wasm("(module)").unwrap(),
            &[
                0, 97, 115, 109, // \0ASM - magic
                1, 0, 0, 0 //  0x01 - version
            ]
        );
    }

    #[test]
    fn sample_e2e() {
        let retcode = run(vec![(
            "main",
            r####"
            import fn "lang" "print_i64" print_i64(i64) i64;

            fn[pub] main() i32 {
                print_i64(42);
                print_i64(47);
                print_twice(888);
                742
            }

            fn print_twice(i i64) [
                # local variables go here
                x i32,
            ] {
                print_i64(i);
                print_i64(i);
            }

                "####,
        )])
        .unwrap();
        assert_eq!(retcode, 742);
    }

    #[test]
    fn func_call() {
        let retcode = run(vec![(
            "main",
            r####"
            import fn "lang" "print_i64" print_i64(i64) i64;

            fn[pub] main() i32 {
                foo(6)
            }

            fn foo(x i32) i32 {
                425
            }

                "####,
        )])
        .unwrap();
        assert_eq!(retcode, 425);
    }

    #[test]
    fn inline_asm() {
        let retcode = run_or_panic(vec![(
            "main",
            r####"
            fn[pub] main() i32 {
                $asm(
                    [
                        i32; 44,
                    ],
                    i32,
                    r###"
                    i32.const 2
                    i32.mul
                    "###,
                )
            }
                "####,
        )]);
        assert_eq!(retcode, 88);
    }

    #[test]
    fn string() {
        // TODO: More extensive test that actually tests
        // the value of the string
        let retcode = run_or_panic(vec![(
            "main",
            r####"
            fn[pub] main() {
                somestr();
            }

            fn somestr() str {
                "hello world"
            }
                "####,
        )]);
        assert_eq!(retcode, 0);
    }

    #[test]
    fn list() {
        // TODO: More extensive test that actually tests
        // the value of the list
        let retcode = run_or_panic(vec![(
            "main",
            r####"
            fn[pub] main() {
                somelist();
            }

            fn somelist() list {
                [1, 2, 3]
            }
                "####,
        )]);
        assert_eq!(retcode, 0);
    }
}
