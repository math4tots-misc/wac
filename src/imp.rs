use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;

std::thread_local! {
    pub static MALLOC_CHECK: RefCell<MallocCheck> = RefCell::new(MallocCheck::new());
}

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
            "eprint0_i32" => wr::func!(|_ctx: &mut wr::Ctx, x: i32| {
                eprint!("{}", x);
            }),

            "record_malloc" => wr::func!(move |_ctx: &mut wr::Ctx, len: i32, ptr: i32| -> i32 {
                MALLOC_CHECK.with(|check| {
                    check.borrow_mut().record_malloc(len, ptr)
                })
            }),
            "record_free" => wr::func!(move |_ctx: &mut wr::Ctx, len: i32, ptr: i32| -> i32 {
                MALLOC_CHECK.with(|check| {
                    check.borrow_mut().record_free(len, ptr)
                })
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

pub struct MallocCheck {
    pub map: HashMap<i32, i32>,
    pub malloc_cnt: usize,
    pub free_cnt: usize,
    pub invalid_frees: Vec<(i32, i32)>,
    pub max_heap_usage: usize,
}

impl MallocCheck {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            malloc_cnt: 0,
            free_cnt: 0,
            invalid_frees: vec![],
            max_heap_usage: 0,
        }
    }

    fn record_malloc(&mut self, len: i32, ptr: i32) -> i32 {
        // eprintln!("MALLOC: {}, {}", len, ptr);
        assert!(!self.map.contains_key(&ptr));
        self.map.insert(ptr, len);
        self.max_heap_usage = std::cmp::max(self.max_heap_usage, (ptr + len) as usize);
        self.malloc_cnt += 1;
        // if ptr == SUSPECT {
        //     2
        // } else { 1 }
        1
    }

    fn record_free(&mut self, len: i32, ptr: i32) -> i32 {
        // eprintln!("FREE: {}, {}", len, ptr);
        if self.map.get(&ptr) != Some(&len) {
            eprintln!("Invalid free: {}, {}, {:?}", ptr, len, self.map.get(&ptr));
            0
        } else {
            self.free_cnt += 1;
            self.map.remove(&ptr);
            std::io::stderr().flush().unwrap();
            1
        }
    }
}
