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
        match &self.kind {
            TokenizationErrorKind::InvalidChar(c) => {
                write!(f, "Invalid character in code block: '{}'", c)
            }
            TokenizationErrorKind::UnexpectedEndOfInput => {
                write!(f, "End of input in middle of code block")
            }
            TokenizationErrorKind::ParseInteger(err) => {
                write!(f, "Failed to parse integer: {}", err)
            }
            TokenizationErrorKind::ParseFloat(err) => write!(f, "Failed to parse float: {}", err),
        }
    }
}

impl Error for TokenizationError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            TokenizationErrorKind::ParseInteger(err) => Some(err),
            TokenizationErrorKind::ParseFloat(err) => Some(err),
            _ => None,
        }
    }
}
