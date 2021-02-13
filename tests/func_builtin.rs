use lime::*;

#[macro_use]
mod common;

#[test]
fn test_builtin() {
    eval_file!("res/func_builtin.lm").unwrap();
}
