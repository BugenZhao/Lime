use std::io;

use crate::parser::{BinaryOp, UnaryOp, Value};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] io::Error),

    #[error("Parse error: expect {} at {}", .0.expected, .0.location)]
    ParseError(peg::error::ParseError<peg::str::LineCol>),

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
    #[error("Assertion failed: `{0}` is `{1:?}`, while `{2:?}` expected")]
    AssertionFailed(String, Value, Value),
}

pub type Result<T> = std::result::Result<T, Error>;
