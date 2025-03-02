use crate::lexer::error::TokenizationError;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum GeneralError {
    Lexer(TokenizationError),
    Parser(String),
}

pub trait Generalize {
    fn generalize(self) -> GeneralError;
}

pub trait MapGeneralize<T> {
    fn map_generalize(self) -> Result<T, GeneralError>;
}

impl Generalize for TokenizationError {
    fn generalize(self) -> GeneralError {
        GeneralError::Lexer(self)
    }
}

impl Display for GeneralError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GeneralError::Lexer(err) => std::fmt::Display::fmt(&err, f),
            GeneralError::Parser(v) => std::fmt::Display::fmt(&v, f),
        }
    }
}

impl<E: Generalize, T> MapGeneralize<T> for Result<T, E> {
    fn map_generalize(self) -> Result<T, GeneralError> {
        self.map_err(E::generalize)
    }
}

impl From<String> for GeneralError {
    fn from(err: String) -> GeneralError {
        GeneralError::Parser(err)
    }
}
