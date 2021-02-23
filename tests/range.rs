use lime::*;

#[macro_use]
mod common;

#[test]
fn test_range() {
    eval_file!("res/range.lm").unwrap();
}
