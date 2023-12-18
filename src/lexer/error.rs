use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub struct TokenizationError {
    kind: TokenizationErrorKind,
    backtrace: Backtrace
}

#[derive(Debug)]
pub enum TokenizationErrorKind {
    InvalidChar(char),
    UnexpectedEndOfInput
}

#[derive(Debug)]
pub struct Backtrace {
    position: usize
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
        match self.kind {
            TokenizationErrorKind::InvalidChar(char) => "Invalid character in code block",
            TokenizationErrorKind::UnexpectedEndOfInput => "End of input in middle of code block"
        }
    }
}