pub fn make_import_object() -> wr::ImportObject {
    wr::imports! {
        "lang" => {
            "hello" => wr::func!(|_: &mut wr::Ctx| {
                println!("hello");
            }),
            "print_i32" => wr::func!(|_: &mut wr::Ctx, i: i32| {
                println!("{}", i);
            }),
            "print_f32" => wr::func!(|_: &mut wr::Ctx, i: f32| {
                println!("{}", i);
            }),
            "cstrlen" => wr::func!(|ctx: &mut wr::Ctx, ptr: i32| -> i32 {
                read_cstr(ctx.memory(0), ptr).len() as u32 as i32
            }),
            "print_cstr" => wr::func!(|ctx: &mut wr::Ctx, ptr: i32| {
                let memory = ctx.memory(0);
                let s = read_cstr(memory, ptr);
                println!("{}", s);
            }),
            "print_str" => wr::func!(|ctx: &mut wr::Ctx, len: i32, ptr: i32| {
                let memory = ctx.memory(0);
                let s = read_str(memory, len, ptr);
                println!("{}", s);
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

fn read_cstr(memory: &wr::Memory, ptr: i32) -> &str {
    wr::WasmPtr::<i32, wr::Array>::new(ptr as u32)
        .get_utf8_string_with_nul(memory)
        .unwrap()
}

fn read_str(memory: &wr::Memory, len: i32, ptr: i32) -> &str {
    wr::WasmPtr::<i32, wr::Array>::new(ptr as u32)
        .get_utf8_string(memory, len as u32)
        .unwrap()
}
