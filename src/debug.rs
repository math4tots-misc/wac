pub const TRACE_MODE: TraceMode = TraceMode::Default;
pub const MALLOC_CHECK_MODE: MallocCheckMode = MallocCheckMode::Debug;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceMode {
    /// Add to the trace when calling any functions
    /// except those marked notrace
    Default,

    /// Always add to trace on any function call,
    /// even for notrace functions
    Debug,

    /// Never add any information to the stack trace
    None,
}

impl TraceMode {
    pub fn check(&self, is_trace_function: bool) -> bool {
        match self {
            TraceMode::Default => is_trace_function,
            TraceMode::Debug => true,
            TraceMode::None => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MallocCheckMode {
    /// basic check to make sure that free
    /// is never called on values that have
    /// already been freed
    /// NOTE: empirically, this checks seems to be pretty
    /// expensive. While this is incredibly useful for
    /// debugging malloc/free, for release builds, I'd
    /// suggest disabling these checks
    Debug,

    /// disable all malloc checks
    /// this will also mean that malloc/free will skip
    /// calling the __record_* callbacks
    None,
}

impl MallocCheckMode {
    pub fn enabled(&self) -> bool {
        match self {
            MallocCheckMode::Debug => true,
            MallocCheckMode::None => false,
        }
    }
}
