//! wasm functions
//! to be inserted alongside everything else in the generated wat

pub const CODE: &'static str = r###"
(func $rt_stack_push (param $filename i32) (param $lineno i32)
    ;; check for stack overflow
    global.get $rt_stack_top
    global.get $rt_stack_end
    i32.ge_s
    if
        call $f___WAC_stack_overflow
    else
    end

    ;; record the file the function call comes from
    ;; (filename is a cstr)
    global.get $rt_stack_top
    local.get $filename
    i32.store

    ;; record the line number of the function call
    global.get $rt_stack_top
    local.get $lineno
    i32.store offset=4

    ;; increment the stack pointer
    global.get $rt_stack_top
    i32.const 8
    i32.add
    global.set $rt_stack_top
)

(func $rt_stack_pop
    global.get $rt_stack_top
    i32.const -8
    i32.add
    global.set $rt_stack_top
)
"###;
