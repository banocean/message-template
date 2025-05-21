use crate::error::{GeneralError, Generalize};
use crate::lexer::iterate::Lexer;
use crate::lexer::tokens::{Token, TokenType};
use crate::parser::ast::*;
use std::iter::Peekable;

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
enum ScopeEnding<'a> {
    End,
    EOF,
    ElseIf(Expression<'a>),
    Else,
}

#[derive(Debug)]
enum Enable<'a, T> {
    Continue(T),
    End(ScopeEnding<'a>),
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Scope<'a>, GeneralError> {
        self.parse_scope().map(|(s, _)| s)
    }

    fn parse_scope<'b>(&mut self) -> Result<(Scope<'a>, ScopeEnding<'a>), GeneralError> {
        let mut program_flow = Vec::new();
        while let Some(flow) = self.parse_program_flow() {
            match flow? {
                Enable::Continue(flow) => program_flow.push(flow),
                Enable::End(ScopeEnding::End) => return Ok((Scope(program_flow), ScopeEnding::End)),
                Enable::End(ScopeEnding::Else) => return Ok((Scope(program_flow), ScopeEnding::Else)),
                Enable::End(ScopeEnding::ElseIf(statement)) => {
                    return Ok((Scope(program_flow), ScopeEnding::ElseIf(statement.clone())))
                }
                _ => break
            }
        }
        Ok((Scope(program_flow), ScopeEnding::EOF))
    }

    fn parse_program_flow(&mut self) -> Option<Result<Enable<'a, ProgramFlow<'a>>, GeneralError>> {
        if let Some(result_token) = self.tokens.peek() {
            let token = match result_token {
                Ok(token) => token,
                Err(err) => return Some(Err(err.clone().generalize()))
            };

            match token {
                Token::CodeBlockOpen => {
                    self.tokens.next();
                    let result = match self.parse_statement() {
                        Ok(Enable::Continue(statement)) => {
                            Ok(Enable::Continue(ProgramFlow::Statement(statement)))
                        }
                        Ok(Enable::End(ending)) => Ok(Enable::End(ending)),
                        Err(err) => return Some(Err(err)),
                    };
                    Some(result)
                }
                Token::Content(_) => {
                    if let Some(Ok(Token::Content(content))) = self.tokens.next() {
                        Some(Ok(Enable::Continue(ProgramFlow::Content(content))))
                    } else {
                        Some(Err(GeneralError::Parser(
                            "Unexpected error reading content".to_string(),
                        )))
                    }
                }
                _ => Some(Err(GeneralError::Parser(format!(
                    "Unexpected token for program flow: {:?}",
                    token
                )))),
            }
        } else {
            None
        }
    }

    fn parse_statement(&mut self) -> Result<Enable<'a, Statement<'a>>, GeneralError> {
        Ok(Enable::Continue(match self.peek_token_type()? {
            TokenType::End => {
                self.tokens.next();
                self.consume_token(TokenType::CodeBlockClose)?;
                return Ok(Enable::End(ScopeEnding::End));
            }
            TokenType::Else => {
                self.tokens.next();
                let token = match self.tokens.next() {
                    Some(Ok(token)) => token,
                    Some(Err(err)) => return Err(err.generalize()),
                    None => return Err(GeneralError::Parser(
                        "Unexpected end of input after else statement has been opened".to_string(),
                    ))
                };

                return if token == Token::If {
                    let expr = self.parse_expression()?;
                    self.consume_token(TokenType::CodeBlockClose)?;
                    Ok(Enable::End(ScopeEnding::ElseIf(expr)))
                } else if token == Token::CodeBlockClose {
                    Ok(Enable::End(ScopeEnding::Else))
                } else {
                    Err(GeneralError::Parser(
                        "Unexpected token after else, expected '}}' or 'if'".to_string(),
                    ))
                }
            }
            TokenType::Let => self.parse_let_statement()?,
            TokenType::For => self.parse_for_statement()?,
            TokenType::If => self.parse_if_statement()?,
            TokenType::Return => self.parse_return_statement()?,
            TokenType::Break => self.parse_break_statement()?,
            TokenType::Continue => self.parse_continue_statement()?,
            TokenType::Ident
            | TokenType::String
            | TokenType::Integer
            | TokenType::Float
            | TokenType::Bool
            | TokenType::LeftBracket
            | TokenType::Not
            | TokenType::Subtraction => self.parse_display_statement()?,
            _ => {
                return Err(GeneralError::Parser(format!(
                    "Unexpected token at start of statement: {:?}",
                    self.peek_token_type()?
                )))
            }
        }))
    }

    fn parse_let_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        self.consume_token(TokenType::Let)?;
        let identifier = self.parse_identifier()?;
        self.consume_token(TokenType::Assign)?;
        let expression = self.parse_expression()?;
        self.consume_token(TokenType::CodeBlockClose)?;
        Ok(Statement::Let {
            identifier,
            expression,
        })
    }

    fn parse_for_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        self.consume_token(TokenType::For)?;
        let identifier = self.parse_identifier()?;
        self.consume_token(TokenType::In)?;
        let iterable = self.parse_expression()?;
        self.consume_token(TokenType::CodeBlockClose)?;
        let (body, _) = self.parse_scope()?;
        Ok(Statement::For {
            identifier,
            iterable,
            body,
        })
    }

    fn parse_if_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        self.consume_token(TokenType::If)?;
        let condition = self.parse_expression()?;
        self.consume_token(TokenType::CodeBlockClose)?;
        let (then_block, ending) = self.parse_scope()?;

        let mut else_if_blocks = vec![];
        let mut else_block = None;

        if let ScopeEnding::ElseIf(expression) = ending {
            let mut expression = expression;
            loop {
                let (scope, ending) = self.parse_scope()?;
                else_if_blocks.push((expression, scope));

                if let ScopeEnding::ElseIf(expr) = ending {
                    expression = expr
                } else if let ScopeEnding::Else = ending {
                    let (scope, _) = self.parse_scope()?;
                    else_block = Some(scope);
                    break;
                } else {
                    break;
                };

            }
        } else if let ScopeEnding::Else = ending {
            let (scope, _) = self.parse_scope()?;
            else_block = Some(scope)
        };

        Ok(Statement::If {
            condition,
            else_if_blocks,
            then_block,
            else_block,
        })
    }

    fn parse_return_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        self.consume_token(TokenType::Return)?;
        let expression = if self.peek_expression_start() {
            Some(self.parse_expression()?)
        } else {
            None
        };
        self.consume_token(TokenType::CodeBlockClose)?;
        Ok(Statement::Return(expression))
    }

    fn parse_break_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        self.consume_token(TokenType::Break)?;
        self.consume_token(TokenType::CodeBlockClose)?;
        Ok(Statement::Break)
    }

    fn parse_continue_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        self.consume_token(TokenType::Continue)?;
        self.consume_token(TokenType::CodeBlockClose)?;
        Ok(Statement::Continue)
    }

    fn parse_display_statement(&mut self) -> Result<Statement<'a>, GeneralError> {
        let expression = self.parse_expression()?;
        self.consume_token(TokenType::CodeBlockClose)?;
        Ok(Statement::Display(expression))
    }

    fn parse_expression(&mut self) -> Result<Expression<'a>, GeneralError> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> Result<Expression<'a>, GeneralError> {
        let mut left_expr = self.parse_unary_expression()?;

        while let Some(op) = self.peek_binary_operator() {
            let current_precedence = Parser::binary_operator_precedence(&op);
            if current_precedence <= precedence {
                break;
            }

            let operator = op;
            self.tokens.next();
            let right_expr = self.parse_binary_expression(current_precedence)?;
            left_expr = Expression::Binary(BinaryExpression {
                operator,
                left: Box::new(left_expr),
                right: Box::new(right_expr),
            });
        }

        Ok(left_expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression<'a>, GeneralError> {
        match self.peek_token_type()? {
            TokenType::Not => {
                self.tokens.next();
                let expression = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOperator::Not,
                    expression: Box::new(expression),
                }))
            }
            TokenType::Subtraction => {
                self.tokens.next();
                let expression = self.parse_unary_expression()?;
                Ok(Expression::Unary(UnaryExpression {
                    operator: UnaryOperator::Negative,
                    expression: Box::new(expression),
                }))
            }
            _ => self.parse_primary_expression(),
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression<'a>, GeneralError> {
        let token_type = self.peek_token_type()?;
        let mut expression = match token_type {
            TokenType::Ident => {
                let identifier = self.parse_identifier()?;
                if self.peek_token_type() == Ok(TokenType::LeftBracket) {
                    self.parse_function_call(identifier)
                } else {
                    Ok(Expression::Identifier(identifier))
                }
            }
            TokenType::String | TokenType::Integer | TokenType::Float | TokenType::Bool => {
                Ok(Expression::Literal(self.parse_literal()?))
            }
            TokenType::LeftBracket => {
                self.consume_token(TokenType::LeftBracket)?;
                let expression = self.parse_expression()?;
                self.consume_token(TokenType::RightBracket)?;
                Ok(expression)
            }
            _ => Err(GeneralError::Parser(format!(
                "Unexpected token for expression: {:?}",
                token_type
            ))),
        }?;

        loop {
            match self.peek_token_type() {
                Ok(TokenType::Dot) => {
                    self.tokens.next();
                    let member = self.parse_identifier()?;
                    expression = Expression::MemberAccess(MemberAccessExpression {
                        base: Box::new(expression),
                        member,
                    });
                }
                Ok(TokenType::LeftSquareBracket) => {
                    self.tokens.next();
                    let index_expression = self.parse_expression()?;
                    self.consume_token(TokenType::RightSquareBracket)?;
                    expression = Expression::Index(IndexExpression {
                        base: Box::new(expression),
                        index: Box::new(index_expression),
                    });
                }
                _ => break,
            }
        }

        Ok(expression)
    }

    fn parse_function_call(
        &mut self,
        function_name: Identifier<'a>,
    ) -> Result<Expression<'a>, GeneralError> {
        self.consume_token(TokenType::LeftBracket)?;
        let mut arguments = Vec::new();
        if self.peek_token_type() != Ok(TokenType::RightBracket) {
            loop {
                arguments.push(self.parse_expression()?);
                if self.peek_token_type() == Ok(TokenType::Coma) {
                    self.consume_token(TokenType::Coma)?;
                } else {
                    break;
                }
            }
        }
        self.consume_token(TokenType::RightBracket)?;
        Ok(Expression::FunctionCall(FunctionCall {
            function_name,
            arguments,
        }))
    }

    fn parse_identifier(&mut self) -> Result<Identifier<'a>, GeneralError> {
        if let Some(Ok(Token::Ident(name))) = self.tokens.next() {
            Ok(Identifier { name })
        } else {
            Err(GeneralError::Parser("Expected identifier".to_string()))
        }
    }

    fn parse_literal(&mut self) -> Result<Literal<'a>, GeneralError> {
        if let Some(Ok(token)) = self.tokens.next() {
            match token {
                Token::String(value) => Ok(Literal::String(value)),
                Token::Integer(value) => Ok(Literal::Integer(value)),
                Token::Float(value) => Ok(Literal::Float(value)),
                Token::Bool(value) => Ok(Literal::Bool(value)),
                _ => Err(GeneralError::Parser(format!(
                    "Expected literal, found {:?}",
                    token
                ))),
            }
        } else {
            Err(GeneralError::Parser(
                "Expected literal, but found end of input".to_string(),
            ))
        }
    }

    fn consume_token(&mut self, expected_type: TokenType) -> Result<Token<'a>, GeneralError> {
        if let Some(result_token) = self.tokens.next() {
            let token = result_token.map_err(|err| err.generalize())?;
            if TokenType::from(&token) == expected_type {
                Ok(token)
            } else {
                Err(GeneralError::Parser(format!(
                    "Expected token of type {:?}, but found {:?}",
                    expected_type, token
                )))
            }
        } else {
            Err(GeneralError::Parser(format!(
                "Expected token of type {:?}, but found end of input",
                expected_type
            )))
        }
    }

    fn peek_token_type(&mut self) -> Result<TokenType, GeneralError> {
        if let Some(result_token) = self.tokens.peek() {
            let token = result_token
                .as_ref()
                .map_err(|err| err.clone().generalize())?;
            Ok(TokenType::from(token))
        } else {
            Err(GeneralError::Parser("Unexpected end of input".to_string()))
        }
    }

    fn peek_binary_operator(&mut self) -> Option<BinaryOperator> {
        match self.peek_token_type() {
            Ok(TokenType::Addition) => Some(BinaryOperator::Addition),
            Ok(TokenType::Subtraction) => Some(BinaryOperator::Subtraction),
            Ok(TokenType::Multiplication) => Some(BinaryOperator::Multiplication),
            Ok(TokenType::Division) => Some(BinaryOperator::Division),
            Ok(TokenType::Remainder) => Some(BinaryOperator::Remainder),
            Ok(TokenType::Exponent) => Some(BinaryOperator::Exponent),
            Ok(TokenType::Equal) => Some(BinaryOperator::Equal),
            Ok(TokenType::NotEqual) => Some(BinaryOperator::NotEqual),
            Ok(TokenType::Greater) => Some(BinaryOperator::Greater),
            Ok(TokenType::Less) => Some(BinaryOperator::Less),
            Ok(TokenType::GreaterOrEqual) => Some(BinaryOperator::GreaterOrEqual),
            Ok(TokenType::LessOrEqual) => Some(BinaryOperator::LessOrEqual),
            Ok(TokenType::And) => Some(BinaryOperator::And),
            Ok(TokenType::Or) => Some(BinaryOperator::Or),
            _ => None,
        }
        .filter(|_| self.tokens.peek().is_some())
    }

    fn binary_operator_precedence(operator: &BinaryOperator) -> u8 {
        match operator {
            BinaryOperator::Or => 1,
            BinaryOperator::And => 2,
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::Greater
            | BinaryOperator::Less
            | BinaryOperator::GreaterOrEqual
            | BinaryOperator::LessOrEqual => 3,
            BinaryOperator::Addition | BinaryOperator::Subtraction => 4,
            BinaryOperator::Multiplication
            | BinaryOperator::Division
            | BinaryOperator::Remainder => 5,
            BinaryOperator::Exponent => 6,
        }
    }

    fn peek_expression_start(&mut self) -> bool {
        match self.peek_token_type() {
            Ok(TokenType::Ident)
            | Ok(TokenType::String)
            | Ok(TokenType::Integer)
            | Ok(TokenType::Float)
            | Ok(TokenType::Bool)
            | Ok(TokenType::LeftBracket)
            | Ok(TokenType::Not)
            | Ok(TokenType::Subtraction) => true,
            _ => false,
        }
    }
}
