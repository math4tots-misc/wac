# TODO2

Secondary todos

These are things that are not immediately pressing, but I want to get to
at some point.

* stricter global variable initialization rules
* at least for the main expression parsing, use
    some sort of bottom-up thing instead of pure
    recursive descent
* Allow calling functions with arguments specified
    with keywords
* Figure out a story for how to get type names
    in contexts where only the typetag is available
    (in Rust code)
    * Currently, I'm working around this by using a global variable
        TODO: Fix this
* Right now == and != are aliased to 'Eq'
    TODO: avoid the call when argument types are known and
    direct instructions can be used (i.e. for primitives)
    or functions (e.g. for str -> __str_eq, list -> __list_eq)
