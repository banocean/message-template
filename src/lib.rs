pub mod error;
mod utils;

pub mod parser {
    pub mod ast;
    pub mod iterate;
    #[cfg(test)]
    mod tests;
}

pub mod lexer {
    pub mod error;
    pub mod iterate;
    pub mod tokens;
}
