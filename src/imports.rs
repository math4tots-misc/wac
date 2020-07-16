use std::cell::Cell;
use std::io::Write;

pub fn make_import_object() -> wr::ImportObject {
    wr::imports! {
        "wac" => {
            "stdout_write" => wr::func!(|ctx: &mut wr::Ctx, len: i32, ptr: i32| -> i32 {
                let memory = ctx.memory(0);
                let buf = read_bytes_from_memory(&memory, len, ptr);
                std::io::stdout().write(&buf).unwrap() as i32
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
