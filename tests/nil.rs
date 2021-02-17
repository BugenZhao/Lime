use lime::*;

#[macro_use]
mod common;

#[test]
fn test_nil_1() {
    eval!(
        r#"
    class Foo { bar? }
    var foo = Foo { bar? : nil };
    foo.bar? = 1;
    foo.bar? = nil;
    "#
    )
    .unwrap();
}

#[test]
fn test_nil_2() {
    let r = eval!(
        r#"
    class Foo { bar }
    var foo = Foo { bar : nil };
    "#
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(..)))
}

#[test]
fn test_nil_3() {
    let r = eval!(
        r#"
    class Foo { bar }
    var foo = Foo { bar : 1 };

    foo.bar = nil;
    "#
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(..)))
}


#[test]
fn test_nil_4() {
    eval!(
        r#"
    class Foo {}
    impl Foo {
        assoc bar? = 1;
    }
    Foo.bar? = nil;
    var r? = Foo.bar?;
    "#
    )
    .unwrap();
}

#[test]
fn test_nil_5() {
    let r = eval!(
        r#"
    class Foo {}
    impl Foo {
        assoc bar = 1;
    }
    Foo.bar = nil;
    "#
    )
    .unwrap_err();
    assert!(matches!(r.tp, ErrType::CannotHaveValue(..)))
}
