use lime::*;

#[macro_use]
mod common;

#[test]
fn test_vec() {
    eval_file!("res/vec.lm").unwrap();
}
