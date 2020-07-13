/// enables stack traces even on 'notrace' functions
/// to see where some kinds of errors are coming from
pub const TRACE_MODE: TraceMode = TraceMode::Default;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceMode {
    /// Add to the trace when calling any functions
    /// not marked notrace
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
