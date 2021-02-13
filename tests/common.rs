#![allow(unused_macros)]

macro_rules! eval_file {
    ($path:expr) => {{
        let text = include_str!($path);
        Interpreter::new().eval(text)
    }};
}

macro_rules! eval {
    ($text:expr) => {
        Interpreter::new().eval($text)
    };
}
