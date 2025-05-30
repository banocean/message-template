use crate::lexer::error::{TokenizationError, TokenizationErrorKind};
use crate::lexer::tokens::Token;
use crate::utils::{DoublePeekable, Second};
use std::iter::Enumerate;
use std::str::Chars;

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
                    if $self.input.peek().map(|(_, v)| v) == Some(&$n) {
                        $self.input.next();
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

macro_rules! parse_or_error {
    ($number: expr, $position: expr, $a: ident, $b: ident) => {
        Token::$a(
            match $number.parse() {
                Ok(number) => number,
                Err(err) => {
                    return Some(Err(
                        TokenizationError::new(TokenizationErrorKind::$b(err), $position)
                    ));
                }
            }
        )
    };
}

pub struct Lexer<'a> {
    input_data: &'a str,
    input: DoublePeekable<Enumerate<Chars<'a>>>,
    is_code_block_open: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input_data: input,
            input: DoublePeekable::new(input.chars().enumerate()),
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
        &self.input_data[position..position + offset]
    }

    fn look_ahead(&mut self) -> Option<char> {
        self.input.peek().map(|d| d.1)
    }

    fn double_look_ahead(&mut self) -> Option<char> {
        self.input.peek2().map(|d| d.1)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, TokenizationError>;

    fn next(&mut self) -> Option<Self::Item> {
        let (mut position, mut char) = self.input.next()?;
        let mut offset = 0usize;

        Some(Ok(if !self.is_code_block_open {
            if char == '{' && self.input.next().second() == Some('{') {
                self.is_code_block_open = true;
                Token::CodeBlockOpen
            } else {
                offset += 1;
                while let Some(_) = self.input.next() {
                    if self.look_ahead() == Some('{') && self.double_look_ahead() == Some('{') {
                        return Some(Ok(Token::Content(self.get_content(position, offset + 1))));
                    }
                    offset += 1;
                }

                Token::Content(self.get_content(position, offset))
            }
        } else {
            while char.is_whitespace() {
                (position, char) = self.input.next()?;
            }

            chars_to_tokens!(self, char, position, {
                '/' => Division,
                '%' => Remainder,
                '^' => Exponent,
                '(' => LeftBracket,
                ')' => RightBracket,
                '[' => LeftSquareBracket,
                ']' => RightSquareBracket,
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

                    self.is_code_block_open = false;

                    Token::CodeBlockClose
                }
                ' ' | '\t' | '\n' => {
                    return self.next()
                }
                'a'..='z' | 'A'..='Z' => {
                    offset += 1;
                    while let Some(char) = self.look_ahead() {
                        if !char.is_alphanumeric() { break; }
                        self.input.next();
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
                        "return" => Token::Return,
                        "else" => Token::Else,
                        "break" => Token::Break,
                        "continue" => Token::Continue,
                        _ => Token::Ident(name)
                    }
                }
                '"' => {
                    while let Some(char) = self.look_ahead() {
                        self.input.next();
                        if char == '"' {
                            break;
                        }
                        offset += 1;
                    }
                    Token::String(self.get_content(position + 1, offset))
                }
                '0'..='9' => {
                    offset += 1;
                    let mut is_float = false;
                    while let Some(char) = self.look_ahead() {
                        if !char.is_ascii_digit() {
                            if char == '.' {
                                is_float = true
                            } else { break }
                        }
                        self.input.next();
                        offset += 1;
                    }

                    let number = self.get_content(position, offset);
                    if is_float {
                        parse_or_error!(number, position, Float, ParseFloat)
                    } else {
                        parse_or_error!(number, position, Integer, ParseInteger)
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

#[cfg(test)]
mod tests {
    use crate::lexer::iterate::Lexer;
    use crate::lexer::tokens::Token;

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("{{ !true }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![
                Token::CodeBlockOpen,
                Token::Not,
                Token::Bool(true),
                Token::CodeBlockClose
            ]
        );
        let mut lexer = Lexer::new("{{ false }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![
                Token::CodeBlockOpen,
                Token::Bool(false),
                Token::CodeBlockClose
            ]
        );
        let mut lexer = Lexer::new("{{ end }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![Token::CodeBlockOpen, Token::End, Token::CodeBlockClose]
        );
        let mut lexer = Lexer::new("{{ for }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![Token::CodeBlockOpen, Token::For, Token::CodeBlockClose]
        );
        let mut lexer = Lexer::new("{{ let }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![Token::CodeBlockOpen, Token::Let, Token::CodeBlockClose]
        );
        let mut lexer = Lexer::new("{{ in }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![Token::CodeBlockOpen, Token::In, Token::CodeBlockClose]
        );
        let mut lexer = Lexer::new("{{ if }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![Token::CodeBlockOpen, Token::If, Token::CodeBlockClose]
        );
        let mut lexer = Lexer::new("{{ else }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![Token::CodeBlockOpen, Token::Else, Token::CodeBlockClose]
        );
    }

    #[test]
    fn test_idents() {
        let mut lexer = Lexer::new("{{ test }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![
                Token::CodeBlockOpen,
                Token::Ident("test"),
                Token::CodeBlockClose
            ]
        )
    }

    #[test]
    fn test_numbers() {
        let mut lexer = Lexer::new("{{ 1.1 + 256 }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![
                Token::CodeBlockOpen,
                Token::Float(1.1),
                Token::Addition,
                Token::Integer(256),
                Token::CodeBlockClose
            ]
        )
    }

    #[test]
    fn test_strings() {
        let mut lexer = Lexer::new("{{ \"test\" }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![
                Token::CodeBlockOpen,
                Token::String("test"),
                Token::CodeBlockClose
            ]
        );
        let mut lexer = Lexer::new("{{ \"\" }}");
        assert_eq!(
            lexer.try_collect().unwrap(),
            vec![
                Token::CodeBlockOpen,
                Token::String(""),
                Token::CodeBlockClose
            ]
        );
    }
}
