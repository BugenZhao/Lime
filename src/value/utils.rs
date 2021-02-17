#[macro_export]
macro_rules! ba_rc {
    ($w:expr) => {
        by_address::ByAddress(std::rc::Rc::new($w))
    };
}

#[macro_export]
macro_rules! rc_refcell {
    ($w:expr) => {
        std::rc::Rc::new(std::cell::RefCell::new($w))
    };
}
