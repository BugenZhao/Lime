#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Parse error: expect {} at {}", .0.expected, .0.location)]
    ParseError(peg::error::ParseError<peg::str::LineCol>),

    #[error("Cannot find value `{0}` in this scope")]
    CannotFindValue(String),
    #[error("invalid left-hand side `{0}` of assignment")]
    InvalidLhsAssignment(String),
}

pub type Result<T> = std::result::Result<T, Error>;
