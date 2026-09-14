use message_template::*;

#[tokio::test]
async fn test_e2e_sync_function_call() {
    let mut context = Context::new();
    context.register_function("square", |args| {
        if let Some(Value::Integer(n)) = args.first() {
            Ok(Value::Integer(n * n))
        } else {
            Err("Expected an integer argument".to_string())
        }
    });

    let template = "Square of 5 is {{ square(5) }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Square of 5 is 25");
}

#[tokio::test]
async fn test_e2e_async_function_call() {
    let mut context = Context::new();
    context.register_async_function("greet", |args| async move {
        let name = match args.first() {
            Some(Value::String(s)) => s.as_str(),
            _ => "guest",
        };
        Ok(Value::String(format!("Welcome, {}!", name)))
    });

    let template = r#"{{ greet("Sam") }}"#;
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Welcome, Sam!");
}

#[tokio::test]
async fn test_e2e_function_multiple_arguments() {
    let mut context = Context::new();
    context.register_function("concatWith", |args| {
        if args.len() != 3 {
            return Err("Expected 3 arguments".to_string());
        }
        let s1 = match &args[0] {
            Value::String(s) => s.clone(),
            _ => return Err("arg1 string expected".to_string()),
        };
        let sep = match &args[1] {
            Value::String(s) => s.clone(),
            _ => return Err("arg2 string expected".to_string()),
        };
        let s2 = match &args[2] {
            Value::String(s) => s.clone(),
            _ => return Err("arg3 string expected".to_string()),
        };
        Ok(Value::String(format!("{}{}{}", s1, sep, s2)))
    });

    let template = r#"{{ concatWith("Hello", " -> ", "World") }}"#;
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Hello -> World");
}

#[tokio::test]
async fn test_e2e_function_returning_error() {
    let mut context = Context::new();
    context.register_function("failFn", |_| Err("Custom function error".to_string()));

    let template = "{{ failFn() }}";
    let result = run(template, Some(&context)).await;
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Custom function error");
}

#[tokio::test]
async fn test_e2e_unregistered_function_call() {
    let context = Context::new();
    let template = "{{ unknownFn() }}";
    let result = run(template, Some(&context)).await;
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Function 'unknownFn' not found");
}

#[tokio::test]
async fn test_e2e_nested_function_calls() {
    let mut context = Context::new();
    context.register_function("addOne", |args| {
        if let Some(Value::Integer(n)) = args.first() {
            Ok(Value::Integer(n + 1))
        } else {
            Err("Expected integer".to_string())
        }
    });

    context.register_function("double", |args| {
        if let Some(Value::Integer(n)) = args.first() {
            Ok(Value::Integer(n * 2))
        } else {
            Err("Expected integer".to_string())
        }
    });

    let template = "{{ double(addOne(4)) }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "10");
}
