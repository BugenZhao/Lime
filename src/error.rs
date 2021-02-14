use crate::{
    parser::{BinaryOp, UnaryOp},
    Func, Value,
};
use std::ops::RangeInclusive;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),

    #[error("Parse error: expect {} at {}", .0.expected, .0.location)]
    ParseError(peg::error::ParseError<peg::str::LineCol>),

    #[error("Break outside of a loop")]
    Break(Value),
    #[error("Continue outside of a loop")]
    Continue(Value),
    #[error("Return outside of a function")]
    Return(Value),

    #[error("Cannot find value `{0}` in this scope")]
    CannotFindValue(String),
    #[error("Invalid left-hand side `{0}` of assignment")]
    _InvalidLhsAssignment(String),
    #[error("Cannot apply binary operation `{0:?}` on `{1:?}` and `{2:?}`")]
    CannotApplyBinaryOp(BinaryOp, Value, Value),
    #[error("Cannot apply binary operation `{0:?}` on `{1:?}` and `<short-circuited>`")]
    CannotApplyBinaryOpSc(BinaryOp, Value),
    #[error("Cannot apply unary operation `{0:?}` on `{1:?}`")]
    CannotApplyUnaryOp(UnaryOp, Value),
    #[error("Cannot cast `{0:?}` to type `{1}`")]
    CannotCast(Value, String),
    #[error("Mismatched type: expected `{expected}`, found `{found}`")]
    MismatchedTypes { expected: String, found: String },
    #[error("`{0:?}` cannot be a condition")]
    CannotBeCondition(Value),
    #[error("Assertion failed: `{0}` is `{1:?}`, while `{2:?}` expected")]
    AssertionFailed(String, Value, Value),
    #[error("Variable `{0}` cannot have the value `{1:?}`")]
    CannotHaveValue(String, Value),
    #[error("`{0:?}` is not callable")]
    NotCallable(Value),
    #[error("Function `{f:?}` takes {take:?} arguments but {supp} were supplied")]
    WrongArguments {
        f: Func,
        take: RangeInclusive<usize>,
        supp: usize,
    },
    #[error("The name `{0}` is defined multiple times")]
    DefinedMutlipleTimes(String),
    #[error("`{0}` is not a class")]
    NotAClass(Value),
    #[error("Some fields are wrong or missing while constructing an object of `{0}`")]
    WrongFields(Value),
    #[error("There's no field `{1}` in `{0:?}`")]
    NoField(Value, String),

    #[error(transparent)]
    Lime(#[from] LimeError),
}

#[derive(thiserror::Error, Debug)]
pub enum LimeError {
    #[error("Panic: {0}")]
    Panic(String),
}

pub type Result<T> = std::result::Result<T, Error>;
