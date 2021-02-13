use lime::*;

#[macro_use]
mod common;

#[test]
fn test_example() {
    eval_file!("res/example.lm").unwrap();
}
