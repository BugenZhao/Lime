use lime::*;

#[macro_use]
mod common;

#[test]
fn test_range() {
    eval_file!("res/range.lm").unwrap();
}

#[test]
fn test_range_parse() {
    eval!(
        "
        var v = [1, [2, 3]];
        var range = 0..v.len();
        println(range);
        "
    )
    .unwrap();
}
