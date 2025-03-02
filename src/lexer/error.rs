use std::error::Error;
use std::num::{ParseFloatError, ParseIntError};
use std::{char, fmt};

#[derive(Debug, PartialEq, Clone)]
pub struct TokenizationError {
    kind: TokenizationErrorKind,
    backtrace: Backtrace,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenizationErrorKind {
    InvalidChar(char),
    UnexpectedEndOfInput,
    ParseInteger(ParseIntError),
    ParseFloat(ParseFloatError),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Backtrace {
    position: usize,
}

impl TokenizationError {
    pub fn new(kind: TokenizationErrorKind, position: usize) -> Self {
        Self {
            kind,
            backtrace: Backtrace { position },
        }
    }

    pub fn kind(&self) -> &TokenizationErrorKind {
        &self.kind
    }
}

impl fmt::Display for TokenizationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.description().fmt(f)
    }
}

impl Error for TokenizationError {
    fn description(&self) -> &str {
        match &self.kind {
            TokenizationErrorKind::InvalidChar(_) => "Invalid character in code block",
            TokenizationErrorKind::UnexpectedEndOfInput => "End of input in middle of code block",
            TokenizationErrorKind::ParseInteger(error) => error.description(),
            TokenizationErrorKind::ParseFloat(error) => error.description(),
        }
    }
}
