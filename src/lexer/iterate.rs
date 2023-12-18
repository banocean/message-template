use std::iter::{Enumerate, Peekable};
use std::str::Chars;
use crate::lexer::error::{TokenizationError, TokenizationErrorKind};
use crate::lexer::tokens::Token;
use crate::utils::Second;

macro_rules! chars_to_tokens {
    (
        $self: expr, $s: expr, $position: expr,
        { $( $x: pat => $y: ident, )* },
        { $( $x2: pat => { $n: expr => $y2: ident else $z2: ident } )* },
        { $( $x3: expr => $y3: ident, )* },
        { $( $d: pat => $r: expr )* }
    ) => {
        match $s {
            $(
                $x => {
                    Token::$y
                }
            )*
            $(
                $x2 => {
                    if $self.input.next().second() == Some($n) {
                        Token::$y2
                    } else {
                        Token::$z2
                    }
                }
            )*
            $(
                $x3 => {
                    if $self.input.next().second() != Some($x3) {
                        return Some(Err(
                            TokenizationError::new(TokenizationErrorKind::InvalidChar($x3), $position)
                        ))
                    }
                    Token::$y3
                }
            )*
            $(
                $d => $r
            )*
        }
    };
}

pub struct Lexer<'a> {
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

    pub fn try_collect(&mut self) -> Result<Vec<Token>, TokenizationError> {
        let mut accumulator = vec![];
        while let Some(result) = self.next() {
            accumulator.push(result?)
        }
        Ok(accumulator)
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

        Some(Ok(if !self.is_code_block_open {
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
            chars_to_tokens!(self, char, position, {
                '/' => Division,
                '%' => Remainder,
                '^' => Exponent,
                '(' => LeftBracket,
                ')' => RightBracket,
                ';' => Semicolon,
                ',' => Coma,
                '.' => Dot,
            }, {
                '+' => { '+' => Increase else Addition }
                '-' => { '-' => Decrease else Subtraction }
                '*' => { '*' => Exponent else Multiplication }
                '=' => { '=' => Equal else Assign }
                '!' => { '=' => NotEqual else Not }
                '>' => { '=' => GreaterOrEqual else Greater }
                '<' => { '=' => LessOrEqual else Less }
            }, {
                '&' => And,
                '|' => Or,
            }, {
                '}' => {
                    if self.input.next().second() != Some('}') {
                        return Some(Err(
                            TokenizationError::new(
                                TokenizationErrorKind::InvalidChar(char), position
                            )
                        ))
                    }

                    self.next();
                    self.is_code_block_open = false;

                    Token::CodeBlockClose
                }
                ' ' | '\t' | '\n' => {
                    return self.next()
                }
                'a'..='z' | 'A'..='Z' => {
                    offset += 1;
                    while let Some((_, char)) = self.input.next() {
                        if !char.is_alphanumeric() { break; }
                        offset += 1;
                    }

                    let name = self.get_content(position, offset);
                    match name {
                        "true" => Token::Bool(true),
                        "false" => Token::Bool(false),
                        "end" => Token::End,
                        "for" => Token::For,
                        "let" => Token::Let,
                        "in" => Token::In,
                        "if" => Token::If,
                        "else" => Token::Else,
                        _ => Token::Ident(name)
                    }
                }
                _ => {
                    return Some(Err(
                        TokenizationError::new(
                            TokenizationErrorKind::InvalidChar(char),
                            position
                        )
                    ))
                }
            })
        }))
    }
}