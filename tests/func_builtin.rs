use lime::*;

#[macro_use]
mod common;

#[test]
fn test_builtin() {
    eval_file!("tests/res/func_builtin.lm").unwrap();
}
