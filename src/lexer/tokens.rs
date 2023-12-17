#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Content(&'a str),
    Ident(&'a str),

    CodeBlockOpen,
    CodeBlockClose,

    String(&'a str),
    Integer(i64),
    Float(f64),
    Bool(bool),

    And,
    Or,
    Not,

    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,

    Addition,
    Subtraction,
    Multiplication,
    Division,
    Remainder,
    Exponent,

    Assign,
    Increase,
    Decrease,

    LeftBracket,
    RightBracket,
    Semicolon,
    Coma,
    Dot,

    End,
    For,
    Let,
    In,

    If,
    Else,

    Continue,
    Return,
    Break,
}
