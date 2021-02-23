use lime::*;

#[macro_use]
mod common;

#[test]
fn test_primitive() {
    eval_file!("res/primitive.lm").unwrap();
}
