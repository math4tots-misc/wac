use std::cell::Cell;
use std::io::Write;

pub fn make_import_object() -> wr::ImportObject {
    wr::imports! {
        "lang" => {
            "write_raw" => wr::func!(|ctx: &mut wr::Ctx, fd: i32, len: i32, ptr: i32| -> i32 {
                let memory = ctx.memory(0);
                let bytes = read_bytes_from_memory(memory, len, ptr);
                match fd {
                    0 => panic!("Cannot write to stdin"),
                    1 => std::io::stdout().write(&bytes).unwrap() as i32,
                    2 => std::io::stderr().write(&bytes).unwrap() as i32,
                    _ => panic!("Invalid file descriptor {}", fd),
                }
            }),
        }
    }
}

fn read_bytes_from_memory(memory: &wr::Memory, len: i32, ptr: i32) -> Vec<u8> {
    let ptr = ptr as usize;
    let len = len as usize;
    let view: wr::memory::MemoryView<u8> = memory.view();
    view[ptr..ptr + len].iter().map(Cell::get).collect()
}
