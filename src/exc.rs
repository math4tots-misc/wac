use crate::PAGE_SIZE;
use crate::LLType;

/// memory location 0 (4-bytes) contains the error pointer
pub const ERROR_PTR: u32 = 0;

/// memory location 4 (4-bytes) contains the alloc-so-far ptr
/// indicating how much of the memory has been reserved so far
pub const ALLOC_SO_FAR_PTR: u32 = 4;

pub(crate) fn make_import_object() -> wr::ImportObject {
    wr::imports! {
        "lang" => {
            "print_i64" => wr::func!(|x: i64| -> i64 {
                println!("{}", x);
                0
            }),
            "print_i32" => wr::func!(|x: i32| -> i32 {
                println!("{}", x);
                0
            }),
            "str" => wr::func!(|x: i64| -> i32 {
            }),
            "puts" => wr::func!(|ctx: &mut wr::Ctx, ptr: wr::WasmPtr<u32>| -> i32 {
                println!("{}", get_str(ctx.memory(0), ptr).unwrap());
                0
            }),
            "malloc" => wr::func!(|ctx: &mut wr::Ctx, size: u32| -> i32 {
                malloc(ctx.memory(0), size).unwrap() as i32
            }),
            "free" => wr::func!(|ctx: &mut wr::Ctx, size: u32, ptr: u32| -> i32 {
                free(ctx.memory(0), size, ptr);
                0
            }),
        }
    }
}

/// dead simple freelist implementation
fn malloc(memory: &wr::Memory, size: u32) -> Option<u32> {
    // memory location 0 (32bit) is used for error handling
    // memory location 4 (32bit) contains the amount allocated so far
    // memory location 8, 12, 16, ... contain freelist ptrs
    let freelist_ptr = freelist_loc(size);
    let freelist_val = read(memory, freelist_ptr);
    if freelist_val == 0 {
        Some(alloc(memory, size))
    } else {
        let next_ptr = read(memory, freelist_val);
        write(memory, freelist_ptr, next_ptr);
        Some(freelist_val)
    }
}

fn free(memory: &wr::Memory, size: u32, ptr: u32) {
    let freelist_ptr = freelist_loc(size);
    let current_freelist_val = read(memory, freelist_ptr);
    write(memory, ptr, current_freelist_val);
    write(memory, freelist_ptr, ptr);
}

fn alloc(memory: &wr::Memory, size: u32) -> u32 {
    let size = (size + 8 - 1) / 8 * 8;

    let old_alloc_size = read(memory, ALLOC_SO_FAR_PTR);
    let old_page_count = (old_alloc_size + PAGE_SIZE - 1) / PAGE_SIZE;
    let new_alloc_size = old_alloc_size + size;
    let new_page_count = (new_alloc_size + PAGE_SIZE - 1) / PAGE_SIZE;
    write(memory, ALLOC_SO_FAR_PTR, new_alloc_size);

    if old_page_count < new_page_count {
        memory.grow(wr::units::Pages(new_page_count - old_page_count)).unwrap();
    }

    old_alloc_size
}

/// given an 'id' value, return its type
fn typeof_(x: i64) -> Option<LLType> {
    num_traits::FromPrimitive::from_u32((x >> 32) as u32)
}

fn read(memory: &wr::Memory, ptr: u32) -> u32 {
    wr::WasmPtr::<u32>::new(ptr).deref(memory).unwrap().get()
}

fn write(memory: &wr::Memory, ptr: u32, val: u32) {
    wr::WasmPtr::<u32>::new(ptr).deref(memory).unwrap().set(val);
}

fn write_buffer(memory: &wr::Memory, ptr: u32, arr: &[u8]) {
    let cells = wr::WasmPtr::<u8, wr::Array>::new(ptr).deref(memory, ptr, arr.len() as u32).unwrap();
    for (cell, x) in cells.iter().zip(arr) {
        cell.set(*x);
    }
}

/// returns pointer to freelist pointer for blocks of given size
fn freelist_loc(size: u32) -> u32 {
    let size = std::cmp::max(8, size.next_power_of_two());

    // minimum size is 8 leading to 3 * 4
    // which is good, since the first 2 pointer locations are reserved
    (size - 1).count_ones() * 4
}

fn get_str(memory: &wr::Memory, ptr: wr::WasmPtr<u32>) -> Option<&str> {
    let offset = ptr.offset();
    let len_ptr: wr::WasmPtr<u32> = wr::WasmPtr::new(offset + 4);
    let len = len_ptr.deref(memory).unwrap().get();
    let buffer_ptr = wr::WasmPtr::<u8, wr::Array>::new(offset + 8);
    buffer_ptr.get_utf8_string(memory, len)
}

fn new_str(memory: &wr::Memory, s: &str) -> Option<u32> {
    let len = s.len() as u32;
    let buffer_size = len + 8;
    let ptr = malloc(memory, buffer_size)?;
    write(memory, ptr, 1);  // refcnt
    write(memory, ptr + 4, len); // strlen
    write_buffer(memory, ptr + 8, s.as_bytes()); // str
    Some(ptr)
}
