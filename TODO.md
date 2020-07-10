# TODO

* char type
* chars type (buffer of chars)
* refactor to use an llir layer for manipulating wasm instructions
    WIP see llir.rs, and sink.rs
* floating point math (log, sin, cos, pow, etc)
* simplify the imported print functions
* some kind of for loops
    Specialized to just integer ranges
* Format function for formatted strings
* auto impl enums for 'trait str' (maybe also records?)
* Make '__read/__write' into intrinsics in stead of function calls
    Maybe $read/$write?
* Forbid at least some special identifiers from being used as variable/function names
    Maybe hook into parser's 'expect_name'?
* Syntax highlighting VSCode plugin

* record type
    While record types are kinda implemented
    There needs to be a corresponding 'constructor' function
* reference counted mstr type (or maybe call it 'buffer'?)
    A mutable buffer storing UCS4 data.
    Maybe also for storing a simple array of i32 values?
    This way indexing, changing a char at location, etc
    is easy and fast
* namespaces/modules
* control flow (return)
* control flow (break, continue)
* refrence counted map type
* refrence counted set type
* some sort of switch or match (may depend on first
    having some sort of global constants mechanism
    to be useful)
* function calls by keyword parameters
    This is part of the reason why a function's type
    includes the parameters' names

# DONE

* local variable declarations
    instead of requiring them upfront
* reference counted string type
* refrence counted list type
* global variables
* global constants
* peek and poke
    NOTE: in wac they're named __read/__read_i64 and __write/__write_i64
* memory management (some sort of malloc and free)
* 'id' type
    it should be able to represent all pointer
    and <64-bit primitives
* stack trace
* trait functions
    something that's implemented like

        # define new trait function
        trait fn Len(self)

        # define its implementation for various
        # types
        impl Len(self list) {
            ...
        }
        impl Len(self str) {
            ...
        }

        # then just use it like any other function
        fn Main() {
            Len("hello")
        }

    Since this would be pretty heavy on dispatch,
    some sort of inline caching (like how interface dispatch is
    implemented for Java/Smalltalk) may be sorely needed here
* struct/record types
    grouping bits of data together should be possible
    to keep things simple, they should be dumb structs
    behind reference counted pointers
* enum types
* Call trait funcs directly when receiver type is known
* 0.0.1 version on crates.io
