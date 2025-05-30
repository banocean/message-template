use crate::{
    error::GeneralError,
    lexer::iterate::Lexer,
    parser::ast::{
        BinaryExpression, BinaryOperator, Expression, FunctionCall, Identifier,
        Literal, ProgramFlow, Statement, UnaryExpression,
        UnaryOperator,
    },
};
use super::{ast::Scope, iterate::Parser};

pub fn parse(lexer: Lexer) -> Result<Scope, GeneralError> {
    Parser::new(lexer).parse()
}

fn parse_program(input: &str) -> Result<Scope, GeneralError> {
    let lexer = Lexer::new(input);
    parse(lexer)
}

#[test]
fn test_parse_content() {
    let input = "Hello, world!";
    let expected_scope = Scope(vec![ProgramFlow::Content("Hello, world!")]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_let_statement() {
    let input = "{{ let x = 122 }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Let {
        identifier: Identifier { name: "x" },
        expression: Expression::Literal(Literal::Integer(122)),
    })]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_display_statement_identifier() {
    let input = "{{ x }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::Identifier(Identifier { name: "x" }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_display_statement_literal() {
    let input = "{{ \"hello\" }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::Literal(Literal::String("hello")),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_binary_expression() {
    let input = "{{ 0 + 2 }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::Literal(Literal::Integer(0))),
            right: Box::new(Expression::Literal(Literal::Integer(2))),
        }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_unary_expression() {
    let input = "{{ !true }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::Unary(UnaryExpression {
            operator: UnaryOperator::Not,
            expression: Box::new(Expression::Literal(Literal::Bool(true))),
        }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_if_statement() {
    let input = "{{ if true }}{{ 0 }}{{ end }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::If {
        condition: Expression::Literal(Literal::Bool(true)),
        else_if_blocks: vec![],
        then_block: Scope(vec![ProgramFlow::Statement(Statement::Display(
            Expression::Literal(Literal::Integer(0)),
        ))]),
        else_block: None,
    })]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_if_else_statement() {
    let input = "{{ if false }}{{ 0 }}{{ else }}{{ 2 }}{{ end }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::If {
        condition: Expression::Literal(Literal::Bool(false)),
        else_if_blocks: vec![],
        then_block: Scope(vec![ProgramFlow::Statement(Statement::Display(
            Expression::Literal(Literal::Integer(0)),
        ))]),
        else_block: Some(Scope(vec![ProgramFlow::Statement(Statement::Display(
            Expression::Literal(Literal::Integer(2)),
        ))])),
    })]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_if_else_if_statement() {
    let input = "{{ if false }}{{ 0 }}{{ else if true }}{{ 1 }}{{ else if !true }}{{ 2 }}{{ end }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::If {
        condition: Expression::Literal(Literal::Bool(false)),
        else_if_blocks: vec![
            (
                Expression::Literal(Literal::Bool(true)),
                Scope(vec![ProgramFlow::Statement(Statement::Display(
                    Expression::Literal(Literal::Integer(1)),
                ))])
            ),
            (
                Expression::Unary(
                    UnaryExpression {
                        operator: UnaryOperator::Not,
                        expression: Box::new(Expression::Literal(Literal::Bool(true)))
                    },
                ),
                Scope(vec![ProgramFlow::Statement(Statement::Display(
                    Expression::Literal(Literal::Integer(2)),
                ))])
            )
        ],
        then_block: Scope(vec![ProgramFlow::Statement(Statement::Display(
            Expression::Literal(Literal::Integer(0)),
        ))]),
        else_block: None,
    })]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_for_statement() {
    let input = "{{ for item in items }}{{ item }}{{ end }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::For {
        identifier: Identifier { name: "item" },
        iterable: Expression::Identifier(Identifier { name: "items" }),
        body: Scope(vec![ProgramFlow::Statement(Statement::Display(
            Expression::Identifier(Identifier { name: "item" }),
        ))]),
    })]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_return_statement_with_value() {
    let input = "{{ return 122 }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Return(Some(
        Expression::Literal(Literal::Integer(122)),
    )))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_return_statement_no_value() {
    let input = "{{ return }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Return(None))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_break_statement() {
    let input = "{{ break }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Break)]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_continue_statement() {
    let input = "{{ continue }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Continue)]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_function_call_no_args() {
    let input = "{{ func() }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::FunctionCall(FunctionCall {
            function_name: Identifier { name: "func" },
            arguments: vec![],
        }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_function_call_with_args() {
    let input = "{{ func(0, \"hello\") }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::FunctionCall(FunctionCall {
            function_name: Identifier { name: "func" },
            arguments: vec![
                Expression::Literal(Literal::Integer(0)),
                Expression::Literal(Literal::String("hello")),
            ],
        }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_operator_precedence() {
    let input = "{{ 0 + 2 * 3 }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::Binary(BinaryExpression {
            operator: BinaryOperator::Addition,
            left: Box::new(Expression::Literal(Literal::Integer(0))),
            right: Box::new(Expression::Binary(BinaryExpression {
                operator: BinaryOperator::Multiplication,
                left: Box::new(Expression::Literal(Literal::Integer(2))),
                right: Box::new(Expression::Literal(Literal::Integer(3))),
            })),
        }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_nested_scopes_with_end() {
    let input = "{{ if true }}{{ if false }}{{ 0 }}{{ end }}{{ end }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::If {
        condition: Expression::Literal(Literal::Bool(true)),
        else_if_blocks: vec![],
        then_block: Scope(vec![ProgramFlow::Statement(Statement::If {
            condition: Expression::Literal(Literal::Bool(false)),
            else_if_blocks: vec![],
            then_block: Scope(vec![ProgramFlow::Statement(Statement::Display(
                Expression::Literal(Literal::Integer(0)),
            ))]),
            else_block: None,
        })]),
        else_block: None,
    })]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}

#[test]
fn test_parse_negative_number() {
    let input = "{{ -11 }}";
    let expected_scope = Scope(vec![ProgramFlow::Statement(Statement::Display(
        Expression::Unary(UnaryExpression {
            operator: UnaryOperator::Negative,
            expression: Box::new(Expression::Literal(Literal::Integer(11))),
        }),
    ))]);
    assert_eq!(parse_program(input).unwrap(), expected_scope);
}
