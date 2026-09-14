use message_template::*;

#[tokio::test]
async fn test_e2e_arithmetic_expressions() {
    let mut context = Context::new();
    context.insert_value("a", Value::Integer(10));
    context.insert_value("b", Value::Integer(5));

    let result = run_as_text(
        "Sum: {{ a + b }}, Prod: {{ a * b }}, Div: {{ a / b }}, Mod: {{ a % b }}",
        Some(&context),
    )
    .await;

    assert_eq!(result, "Sum: 15, Prod: 50, Div: 2, Mod: 0");
}

#[tokio::test]
async fn test_e2e_exponentiation() {
    let mut context = Context::new();
    context.insert_value("base", Value::Integer(2));
    context.insert_value("exp", Value::Integer(10));

    let result = run_as_text("2^10 = {{ base ** exp }}", Some(&context)).await;
    assert_eq!(result, "2^10 = 1024");
}

#[tokio::test]
async fn test_e2e_float_arithmetic() {
    let mut context = Context::new();
    context.insert_value("x", Value::Float(5.5));
    context.insert_value("y", Value::Float(2.0));

    let result = run_as_text("Result: {{ x * y + 1.5 }}", Some(&context)).await;
    assert_eq!(result, "Result: 12.5");
}

#[tokio::test]
async fn test_e2e_string_concatenation() {
    let mut context = Context::new();
    context.insert_value("first", Value::String("John".to_string()));
    context.insert_value("last", Value::String("Doe".to_string()));
    context.insert_value("age", Value::Integer(30));

    let result = run_as_text(
        "Full name: {{ first + \" \" + last }}, Age: {{ age }}",
        Some(&context),
    )
    .await;
    assert_eq!(result, "Full name: John Doe, Age: 30");
}

#[tokio::test]
async fn test_e2e_comparison_operators() {
    let mut context = Context::new();
    context.insert_value("a", Value::Integer(10));
    context.insert_value("b", Value::Integer(20));

    assert_eq!(run_as_text("{{ a < b }}", Some(&context)).await, "true");
    assert_eq!(run_as_text("{{ a > b }}", Some(&context)).await, "false");
    assert_eq!(run_as_text("{{ a <= 10 }}", Some(&context)).await, "true");
    assert_eq!(run_as_text("{{ b >= 20 }}", Some(&context)).await, "true");
    assert_eq!(run_as_text("{{ a == 10 }}", Some(&context)).await, "true");
    assert_eq!(run_as_text("{{ a != b }}", Some(&context)).await, "true");
}

#[tokio::test]
async fn test_e2e_logical_operators() {
    let mut context = Context::new();
    context.insert_value("t", Value::Bool(true));
    context.insert_value("f", Value::Bool(false));

    assert_eq!(run_as_text("{{ t && f }}", Some(&context)).await, "false");
    assert_eq!(run_as_text("{{ t || f }}", Some(&context)).await, "true");
    assert_eq!(run_as_text("{{ !f }}", Some(&context)).await, "true");
}

#[tokio::test]
async fn test_e2e_unary_operators() {
    let mut context = Context::new();
    context.insert_value("num", Value::Integer(42));
    context.insert_value("flag", Value::Bool(true));

    assert_eq!(run_as_text("{{ -num }}", Some(&context)).await, "-42");
    assert_eq!(run_as_text("{{ !flag }}", Some(&context)).await, "false");
}

#[tokio::test]
async fn test_e2e_operator_precedence() {
    let result = run_as_text("{{ 2 + 3 * 4 }}", None).await;
    assert_eq!(result, "14");

    let result_parentheses = run_as_text("{{ (2 + 3) * 4 }}", None).await;
    assert_eq!(result_parentheses, "20");
}

#[tokio::test]
async fn test_e2e_division_by_zero_error() {
    let result = run("{{ 10 / 0 }}", None).await;
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Can't divide 10 by zero");
}
