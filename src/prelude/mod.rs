//! module where all the prelude *.wac sources are
//!
//! Overview:
//!     Most files contain 'guts'
//!     That is, functions that start with '__', '__WAC_', or even '__WAC_UNSAFE'.
//!     These are functions that should not generally be called directly,
//!     but are mostly there to support the more public functions
//!
//!     lang.wac:
//!         this file contains the imports
//!         so when figuring out what the host needs to provide
//!         this would be the place to go
//!
//!     ops.wac:
//!         this is where most of the public stuff is implemented
//!         all the standard language traits are listed here
//!         and impl'd for builtin types
//!
//!     {malloc,str,id,list,type,assert,stack,trait,bool}.wac:
//!         these are mostly implementation details with some functions
//!         meant to only be called by the runtime.
//!         e.g. stack.wac contains functions for managing the stack-trace
//!             malloc.wac has __malloc/__realloc/__free for managing memory
//!             more directly
//!

pub const LANG: &'static str = include_str!("lang.wac");
pub const MALLOC: &'static str = include_str!("malloc.wac");
pub const STR: &'static str = include_str!("str.wac");
pub const ID: &'static str = include_str!("id.wac");
pub const LIST: &'static str = include_str!("list.wac");
pub const TYPE: &'static str = include_str!("type.wac");
pub const ASSERT: &'static str = include_str!("assert.wac");
pub const PANIC: &'static str = include_str!("panic.wac");
pub const STACK: &'static str = include_str!("stack.wac");
pub const OPS: &'static str = include_str!("ops.wac");
pub const TRAIT: &'static str = include_str!("trait.wac");
pub const PRINT: &'static str = include_str!("print.wac");
pub const BOOL: &'static str = include_str!("bool.wac");
pub const INT: &'static str = include_str!("int.wac");
pub const INDEX: &'static str = include_str!("index.wac");
pub const RECORD: &'static str = include_str!("record.wac");
pub const BYTES: &'static str = include_str!("bytes.wac");
