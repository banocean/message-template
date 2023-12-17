use std::iter::{Enumerate, Peekable};
use std::str::Chars;
use crate::lexer::error::{TokenizationError, TokenizationErrorKind};
use crate::lexer::tokens::Token;
use crate::utils::Second;

struct Lexer<'a> {
    input_data: &'a str,
    input: Peekable<Enumerate<Chars<'a>>>,
    is_code_block_open: bool
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input_data: input,
            input: input.chars().enumerate().peekable(),
            is_code_block_open: false,
        }
    }

    fn get_content(&self, position: usize, offset: usize) -> &'a str {
        &self.input_data[position..position+offset]
    }

    fn look_ahead(&mut self) -> Option<char> {
        self.input.peek().map(|d| d.1)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, TokenizationError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (position, char) = self.input.next()?;
        let mut offset = 0usize;

        Some(Ok(if self.is_code_block_open {
            if char == '{' && self.input.next().second() == Some('{') {
                self.is_code_block_open = true;
                Token::CodeBlockOpen
            } else {
                offset += 1;
                while let Some((_, char)) = self.input.next() {
                    if char == '{' && self.look_ahead() == Some('{') {
                        self.next();
                        return Some(Ok(Token::Content(self.get_content(position, offset))))
                    }
                    offset += 1;
                }

                Token::Content(self.get_content(position, offset))
            }
        } else {
            match char {
                _ => {
                    return Some(Err(
                        TokenizationError::new(
                            TokenizationErrorKind::InvalidChar(char),
                            position
                        )
                    ))
                }
            }
        }))
    }
}