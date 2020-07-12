pub fn make_import_object() -> wr::ImportObject {
    wr::imports! {
        "lang" => {
            "print0_str_raw" => wr::func!(|ctx: &mut wr::Ctx, len: i32, ptr: i32| {
                let memory = ctx.memory(0);
                let s = read_str(memory, len, ptr);
                print!("{}", s);
            }),
            "eprint0_str_raw" => wr::func!(|ctx: &mut wr::Ctx, len: i32, ptr: i32| {
                let memory = ctx.memory(0);
                let s = read_str(memory, len, ptr);
                eprint!("{}", s);
            }),
        }
    }
}

// fn read(memory: &wr::Memory, ptr: i32) -> i32 {
//     wr::WasmPtr::<i32>::new(ptr as u32).deref(memory).unwrap().get()
// }

// fn write(memory: &wr::Memory, ptr: i32) -> i32 {
//     wr::WasmPtr::<i32>::new(ptr as u32).deref(memory).unwrap().get()
// }

// fn read_cstr(memory: &wr::Memory, ptr: i32) -> &str {
//     wr::WasmPtr::<i32, wr::Array>::new(ptr as u32)
//         .get_utf8_string_with_nul(memory)
//         .unwrap()
// }

fn read_str(memory: &wr::Memory, len: i32, ptr: i32) -> &str {
    wr::WasmPtr::<i32, wr::Array>::new(ptr as u32)
        .get_utf8_string(memory, len as u32)
        .unwrap()
}
