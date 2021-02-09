#[allow(unused_macros)]
macro_rules! eval {
    ($path:expr) => {
        Interpreter::new().eval_file($path)
    };
}
