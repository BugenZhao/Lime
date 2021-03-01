use crate::{
    ast::{BinaryOp, UnaryOp},
    value::WrFunc,
    Value,
};
use itertools::Itertools;
use peg::{str::LineCol, Parse};
use source_span::{
    fmt::{Formatter, Style},
    Position, Span, DEFAULT_METRICS,
};
use std::ops::RangeInclusive;

#[derive(Debug)]
struct LimeBacktrace(Vec<String>);

impl std::fmt::Display for LimeBacktrace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.0.iter().map(|s| format!("`{}`", s)).join(", ")
        )
    }
}

#[derive(thiserror::Error, Debug)]
pub struct Error {
    #[source]
    pub tp: ErrType,
    bt: LimeBacktrace,
    pub span: Option<(usize, usize)>,
}

impl Error {
    pub fn new(tp: ErrType) -> Self {
        Self {
            tp,
            bt: LimeBacktrace(vec![]),
            span: None,
        }
    }

    pub fn push_func(&mut self, func: &WrFunc) {
        self.bt.0.push(
            func.name()
                .clone()
                .unwrap_or_else(|| "<unknown>".to_owned()),
        )
    }

    pub fn set_span(&mut self, span: (usize, usize)) {
        if self.span.is_none() {
            self.span = Some(span);
        }
    }

    pub fn error_fmt(&self, text: &str) -> String {
        macro_rules! position {
            ($offset:expr) => {{
                let LineCol { line, column, .. } = Parse::position_repr(text, $offset);
                Position::new(line - 1, column - 1)
            }};
        }

        if self.span.is_none() {
            return "".to_owned();
        }
        let self_span = self.span.unwrap();

        let start = position!(self_span.0);
        let last = position!(self_span.1 - 1);
        let end = position!(self_span.1);
        let span = Span::new(start, last, end);

        let text_start = position!(0);
        let text_last = position!(text.len() - 1);
        let text_end = position!(text.len());
        let text_span = Span::new(text_start, text_last, text_end);

        let mut fmt = Formatter::with_margin_color(source_span::fmt::Color::Blue);
        fmt.add(span, Some(self.tp.to_string()), Style::Error);

        let fmtted = fmt
            .render(
                text.chars()
                    .map(|c| -> std::result::Result<char, ()> { Ok(c) }),
                text_span,
                &DEFAULT_METRICS,
            )
            .unwrap();

        format!("{}", fmtted)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // writeln!(f, "In `{}`:", self.text.as_deref().unwrap_or("<unknown>"))?;
        write!(f, "{}", self.tp)?;
        if !self.bt.0.is_empty() {
            write!(f, "\nBacktrace:\n  {}", self.bt)?;
        }
        Ok(())
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::new(ErrType::IoError(e))
    }
}

impl From<peg::error::ParseError<peg::str::LineCol>> for Error {
    fn from(e: peg::error::ParseError<peg::str::LineCol>) -> Self {
        Self::new(ErrType::ParseError(e))
    }
}

#[macro_export]
macro_rules! err {
    ($tp:expr) => {
        crate::error::Error::new($tp)
    };
}

#[derive(thiserror::Error, Debug)]
pub enum ErrType {
    #[error(transparent)]
    IoError(std::io::Error),

    #[error("Parse error: expect {} at {}", .0.expected, .0.location)]
    ParseError(peg::error::ParseError<peg::str::LineCol>),

    #[error("The `{0}` statement should only appear at the top level")]
    OnlyTopLevel(String),

    #[error("Break outside of a loop")]
    Break(Value),
    #[error("Continue outside of a loop")]
    Continue(Value),
    #[error("Return outside of a function")]
    Return(Value),
    #[error("Built-in expect: {0}")]
    Expect(Value),
    #[error("Uncaught: {0}")]
    ErrorReturn(Value),

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
        f: WrFunc,
        take: RangeInclusive<usize>,
        supp: usize,
    },
    #[error("Cannot partial apply function `{0:?}` since it takes fewer arguments")]
    CannotPartialApply(WrFunc),
    #[error("The name `{0}` is defined multiple times")]
    DefinedMutlipleTimes(String),
    #[error("`{0}` is not a class")]
    NotAClass(Value),
    #[error("Some fields are wrong or missing while constructing an object of `{0}`")]
    WrongFields(Value),
    #[error("There's no field `{1}` in `{0:?}`")]
    NoField(Value, String),
    #[error("There's no settable field `{1}` in `{0:?}`")]
    NoFieldToSet(Value, String),
    #[error("Type error: expect `{0}`, find `{1:?}`")]
    TypeError(String, Value),

    #[error("Panic: {0}")]
    LimePanic(String),
}

pub type Result<T> = std::result::Result<T, Error>;
