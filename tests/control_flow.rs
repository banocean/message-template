use message_template::*;

#[tokio::test]
async fn test_e2e_if_else_statement() {
    let template = "{{ if score >= 80 }}PASS{{ else }}FAIL{{ end }}";

    let mut ctx1 = Context::new();
    ctx1.insert_value("score", Value::Integer(85));
    assert_eq!(run_as_text(template, Some(&ctx1)).await, "PASS");

    let mut ctx2 = Context::new();
    ctx2.insert_value("score", Value::Integer(70));
    assert_eq!(run_as_text(template, Some(&ctx2)).await, "FAIL");
}

#[tokio::test]
async fn test_e2e_if_else_if_statement() {
    let template = r#"
{{ if score >= 90 }}
Grade A
{{ else if score >= 80 }}
Grade B
{{ else if score >= 70 }}
Grade C
{{ else }}
Grade F
{{ end }}
"#
    .trim();

    let mut ctx = Context::new();
    ctx.insert_value("score", Value::Integer(85));
    let result = run_as_text(template, Some(&ctx)).await;
    assert!(result.contains("Grade B"));
}

#[tokio::test]
async fn test_e2e_for_loop() {
    let items = Value::Array(vec![
        Value::String("Apple".to_string()),
        Value::String("Banana".to_string()),
        Value::String("Cherry".to_string()),
    ]);

    let mut context = Context::new();
    context.insert_value("items", items);

    let template = "{{ for item in items }}{{ item }}, {{ end }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Apple, Banana, Cherry, ");
}

#[tokio::test]
async fn test_e2e_for_loop_with_break() {
    let numbers = Value::Array(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
        Value::Integer(4),
        Value::Integer(5),
    ]);

    let mut context = Context::new();
    context.insert_value("numbers", numbers);

    let template =
        "{{ for n in numbers }}{{ if n > 3 }}{{ break }}{{ end }}{{ n }}{{ end }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "123");
}

#[tokio::test]
async fn test_e2e_for_loop_with_continue() {
    let numbers = Value::Array(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
        Value::Integer(4),
        Value::Integer(5),
    ]);

    let mut context = Context::new();
    context.insert_value("numbers", numbers);

    let template =
        "{{ for n in numbers }}{{ if n % 2 == 0 }}{{ continue }}{{ end }}{{ n }}{{ end }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "135");
}

#[tokio::test]
async fn test_e2e_nested_for_loops() {
    let outer = Value::Array(vec![Value::Integer(1), Value::Integer(2)]);
    let inner = Value::Array(vec![
        Value::String("a".to_string()),
        Value::String("b".to_string()),
    ]);

    let mut context = Context::new();
    context.insert_value("outer", outer);
    context.insert_value("inner", inner);

    let template =
        "{{ for i in outer }}{{ for j in inner }}[ {{ i }}{{ j }} ]{{ end }}{{ end }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "[ 1a ][ 1b ][ 2a ][ 2b ]");
}

#[tokio::test]
async fn test_e2e_return_value() {
    let mut context = Context::new();
    context.insert_value("a", Value::Integer(7));
    context.insert_value("b", Value::Integer(6));

    let template = "{{ return a * b }}";
    let result = run(template, Some(&context)).await.unwrap();
    assert_eq!(result, ExecutionResult::Value(Value::Integer(42)));
}

#[tokio::test]
async fn test_e2e_return_early() {
    let template = "Header\n{{ return \"stop here\" }}\nFooter";
    let result = run(template, None).await.unwrap();
    assert_eq!(
        result,
        ExecutionResult::Value(Value::String("stop here".to_string()))
    );
}

#[tokio::test]
async fn test_e2e_for_loop_non_array_error() {
    let mut context = Context::new();
    context.insert_value("notarray", Value::Integer(123));

    let template = "{{ for item in notarray }}{{ item }}{{ end }}";
    let result = run(template, Some(&context)).await;
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("can only iterate over arrays"));
}
