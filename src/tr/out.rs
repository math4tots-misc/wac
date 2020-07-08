use super::*;

pub(super) struct Out {
    pub(super) main: Rc<Sink>,
    pub(super) imports: Rc<Sink>,
    pub(super) memory: Rc<Sink>,
    pub(super) data: Rc<Sink>,
    pub(super) gvars: Rc<Sink>,
    pub(super) funcs: Rc<Sink>,
    pub(super) start: Rc<Sink>,
    pub(super) table: Rc<Sink>,
    pub(super) exports: Rc<Sink>,

    pub(super) data_len: Cell<usize>,
    pub(super) intern_cstr_map: HashMap<Rc<str>, u32>,
    pub(super) intern_str_map: HashMap<Rc<str>, u32>,
}

impl Out {
    pub(super) fn new() -> Self {
        assert!(RESERVED_BYTES % 16 == 0);
        assert!(STACK_BYTES % 16 == 0);

        let main = Sink::new();
        let imports = main.spawn();
        let memory = main.spawn();
        let data = main.spawn();
        let gvars = main.spawn();
        let funcs = main.spawn();
        main.write(crate::wfs::CODE);
        main.writeln("(func $__rt_start");
        let start = main.spawn();
        main.writeln(")");
        main.writeln("(start $__rt_start)");
        let table = main.spawn();
        let exports = main.spawn();
        Self {
            main,
            imports,
            memory,
            data,
            gvars,
            funcs,
            start,
            table,
            exports,
            data_len: Cell::new(RESERVED_BYTES + STACK_BYTES),
            intern_cstr_map: HashMap::new(),
            intern_str_map: HashMap::new(),
        }
    }

    pub(super) fn get(self) -> String {
        let len = self.data_len.get();
        let page_len = (len + (PAGE_SIZE - 1)) / PAGE_SIZE;
        self.memory
            .writeln(format!("(memory $rt_mem {})", page_len));
        self.gvars
            .writeln(format!("(global $rt_heap_start i32 (i32.const {}))", len,));
        self.gvars.writeln(format!(
            "(global $rt_stack_top (mut i32) (i32.const {}))",
            STACK_START
        ));
        self.gvars.writeln(format!(
            "(global $rt_stack_start i32 (i32.const {}))",
            STACK_START
        ));
        self.gvars.writeln(format!(
            "(global $rt_stack_end i32 (i32.const {}))",
            STACK_END
        ));
        self.main.get()
    }

    pub(super) fn reserve(&self, len: usize) -> u32 {
        // data is reserved with 16-byte alignment
        let reserve_len = (len + 16 - 1) / 16 * 16;
        let ptr = self.data_len.get();
        self.data_len.set(reserve_len + ptr);
        ptr as u32
    }

    pub(super) fn data(&self, data: &[u8]) -> u32 {
        let ptr = self.reserve(data.len());
        self.data.write(format!("(data (i32.const {}) \"", ptr));
        for byte in data {
            self.data.write(format!("\\{:0>2X}", byte));
        }
        self.data.writeln("\")");
        ptr as u32
    }

    pub(super) fn intern_cstr(&mut self, s: &Rc<str>) -> u32 {
        if !self.intern_cstr_map.contains_key(s) {
            let mut buffer = s.as_bytes().to_vec();
            buffer.push(0);
            let ptr = self.data(&buffer);
            self.intern_cstr_map.insert(s.clone(), ptr);
        }
        *self.intern_cstr_map.get(s).unwrap()
    }

    pub(super) fn intern_str(&mut self, s: &Rc<str>) -> u32 {
        if !self.intern_str_map.contains_key(s) {
            let mut buffer = Vec::<u8>::new();
            // refcnt
            buffer.extend(&1i32.to_le_bytes());
            // len
            buffer.extend(&(s.len() as i32).to_le_bytes());
            // utf8
            buffer.extend(s.as_bytes().to_vec());
            let ptr = self.data(&buffer);
            self.intern_str_map.insert(s.clone(), ptr);
        }
        *self.intern_str_map.get(s).unwrap()
    }
}
