use message_template::*;

#[tokio::test]
async fn test_e2e_basic_interpolation() {
    let mut context = Context::new();
    context.insert_value("name", Value::String("Alice".to_string()));
    context.insert_value("count", Value::Integer(3));

    let template = "Hello {{ name }}, you have {{ count }} new messages!";
    let result = run(template, Some(&context)).await.unwrap();

    assert_eq!(
        result,
        ExecutionResult::Template("Hello Alice, you have 3 new messages!".to_string())
    );
}

#[tokio::test]
async fn test_e2e_run_as_text() {
    let mut context = Context::new();
    context.insert_value("val", Value::Integer(42));

    let result = run_as_text("Value: {{ val }}", Some(&context)).await;
    assert_eq!(result, "Value: 42");
}

#[tokio::test]
async fn test_e2e_let_statement() {
    let mut context = Context::new();
    context.insert_value("x", Value::Integer(10));

    let template = "{{ let y = x * 2 }}Double x is {{ y }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Double x is 20");
}

#[tokio::test]
async fn test_e2e_multiple_let_statements() {
    let template = "{{ let a = 5 }}{{ let b = 10 }}{{ let c = a + b }}Result: {{ c }}";
    let result = run_as_text(template, None).await;
    assert_eq!(result, "Result: 15");
}

#[tokio::test]
async fn test_e2e_undefined_variable_error() {
    let result = run("{{ unknownvar }}", None).await;
    assert!(result.is_err());
    let err_msg = result.unwrap_err();
    assert_eq!(err_msg, "Undefined variable: unknownvar");
}

#[tokio::test]
async fn test_e2e_syntax_error() {
    let result = run("{{ if true ", None).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_e2e_run_ast() {
    let lexer = Lexer::new("Ast test: {{ 100 }}");
    let parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();

    let text_result = run_ast_as_text(&ast, None).await;
    assert_eq!(text_result, "Ast test: 100");

    let exec_result = run_ast(&ast, None).await.unwrap();
    assert_eq!(
        exec_result,
        ExecutionResult::Template("Ast test: 100".to_string())
    );
}

#[tokio::test]
async fn test_e2e_empty_template() {
    let result = run("", None).await.unwrap();
    assert_eq!(result, ExecutionResult::Template("".to_string()));
}
