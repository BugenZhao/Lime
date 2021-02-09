use std::io;

use crate::parser::{Op, Value};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] io::Error),

    #[error("Parse error: expect {} at {}", .0.expected, .0.location)]
    ParseError(peg::error::ParseError<peg::str::LineCol>),

    #[error("Cannot find value `{0}` in this scope")]
    CannotFindValue(String),
    // #[error("Invalid left-hand side `{0}` of assignment")]
    // InvalidLhsAssignment(String),
    #[error("Cannot apply binary operation `{0:?}` on `{1:?}` and `{2:?}` ")]
    CannotApplyBinaryOp(Op, Value, Value),
}

pub type Result<T> = std::result::Result<T, Error>;
