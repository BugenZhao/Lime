use lime::*;

#[macro_use]
mod common;

#[test]
fn test_obj_comparison_1() {
    eval!(
        "
    var a = Pair{a: 1, b: 2};
    var b = Pair{a: 1, b: 2};
    assert a == b;
    assert a &== b;
    assert a !== b;
    "
    )
    .unwrap();
}

#[test]
fn test_obj_comparison_2() {
    eval!(
        "
    class MyPair {a, b}
    var a = MyPair{a: 1, b: 2};
    var b = Pair{a: 1, b: 2};
    assert a &!= b;
    "
    )
    .unwrap();
}

#[test]
fn test_obj_comparison_3() {
    let r = eval!(
        "
    class MyPair {a, b}
    var a = MyPair{a: 1, b: 2};
    var b = Pair{a: 1, b: 2};
    a == b;
    "
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotApplyBinaryOp(..)));
}

#[test]
fn test_obj_comparison_4() {
    let r = eval!(
        "
    class MyPair {a, b}
    var a = MyPair{a: 1, b: 2};
    var b = Pair{a: 1, b: 2};
    a === b;
    "
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotApplyBinaryOp(..)));
}
