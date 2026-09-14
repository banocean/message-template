use message_template::*;
use std::collections::HashMap;

#[tokio::test]
async fn test_e2e_array_indexing() {
    let mut context = Context::new();
    let arr = Value::Array(vec![
        Value::String("First".to_string()),
        Value::String("Second".to_string()),
        Value::String("Third".to_string()),
    ]);
    context.insert_value("list", arr);

    let template = "0: {{ list[0] }}, 2: {{ list[2] }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "0: First, 2: Third");
}

#[tokio::test]
async fn test_e2e_array_index_out_of_bounds_error() {
    let mut context = Context::new();
    let arr = Value::Array(vec![Value::Integer(1)]);
    context.insert_value("list", arr);

    let template = "{{ list[5] }}";
    let result = run(template, Some(&context)).await;
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err(),
        "Index 5 out of bounds for array of length 1"
    );
}

#[tokio::test]
async fn test_e2e_member_access() {
    let mut context = Context::new();
    let mut user = HashMap::new();
    user.insert("name".to_string(), Value::String("Bob".to_string()));
    user.insert("age".to_string(), Value::Integer(25));
    context.insert_value("user", Value::Object(user));

    let template = "Name: {{ user.name }}, Age: {{ user.age }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Name: Bob, Age: 25");
}

#[tokio::test]
async fn test_e2e_missing_property_error() {
    let mut context = Context::new();
    let user = HashMap::new();
    context.insert_value("user", Value::Object(user));

    let template = "{{ user.nonexistent }}";
    let result = run(template, Some(&context)).await;
    assert!(result.is_err());
    assert_eq!(
        result.unwrap_err(),
        "There is no property named 'nonexistent'"
    );
}

#[tokio::test]
async fn test_e2e_nested_data_structures() {
    let mut context = Context::new();

    let mut user1 = HashMap::new();
    user1.insert("name".to_string(), Value::String("Alice".to_string()));

    let mut user2 = HashMap::new();
    user2.insert("name".to_string(), Value::String("Bob".to_string()));

    let users = Value::Array(vec![Value::Object(user1), Value::Object(user2)]);
    context.insert_value("users", users);

    let template = "First user: {{ users[0].name }}, Second user: {{ users[1].name }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "First user: Alice, Second user: Bob");
}
