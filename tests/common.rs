#![allow(unused_macros)]

macro_rules! eval_file {
    ($path:expr) => {
        Interpreter::new().eval_file($path)
    };
}

macro_rules! eval {
    ($text:expr) => {
        Interpreter::new().eval($text)
    };
}
