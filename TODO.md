# TODO

* peek and poke
* control flow (return)
* control flow (break, continue)
* global variables
* reference counted string type
* refrence counted list type
* refrence counted map type
* refrence counted set type
* global constants
* some sort of switch or match (may depend on first
    having some sort of global constants mechanism
    to be useful)
* memory management (some sort of malloc and free)
* 'id' type
    it should be able to represent all pointer
    and <64-bit primitives
* struct/record types
    grouping bits of data together should be possible
    to keep things simple, they should be dumb structs
    behind reference counted pointers
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

# DONE

* local variable declarations
    instead of requiring them upfront
