# message-template


> [!WARNING]
> This project is maintained for usage in [custom-fail/custom](https://github.com/custom-fail/custom). Contributions outside of that scope won't be merged.

A simple templating language for rust.

## Features

- **Expression Support**: Support for arithmetic, comparisons, logical operators, exponentiation, and unary expressions.
- **Control Flow Statements**: `if` / `else if` / `else` conditionals, `for` loops over arrays, `break`, `continue`, and `return` statements.
- **Data Structure Navigation**: Object property access (`user.name`) and array indexing (`items[0]`).
- **Outside Function Calling**: Easily call sync and async rust functions from templates.
- **Serde Context Integration**: Optional feature (`context`) allows adding structs to context that implement `serde::Serialize`.

## Quickstart

Add `message-template` to your `Cargo.toml`:

```toml
message-template = { git = "https://github.com/banocean/message-template.git" }
```

### Basic Example

```rust
use message_template::*;

#[tokio::main]
async fn main() {
    let mut context = Context::new();
    context.insert_value("ping", Value::Integer(42));

    let template = "Current ping is `{{ ping }}ms`";

    assert_eq!(
        run_as_text(template, Some(&context)).await,
        "Current ping is `42ms`".to_string()
    );
}
```

## Syntax Guide

Code blocks are enclosed in double curly braces `{{ ... }}`. Plain text outside of code blocks is emitted as template content directly.

### Variable Interpolation & Display

Expressions placed inside `{{ ... }}` are evaluated and converted to strings in the output:

```text
Hello {{ name }}, you have {{ unreadCount }} unread messages!
```

### Local Variables (`let`)

Declare local variables inside a template scope using `let`:

```text
{{ let multiplier = 2 }}
{{ let doublePing = ping * multiplier }}
Double ping: {{ doublePing }}
```

### Conditionals (`if` / `else if` / `else`)

Branch execution based on boolean conditions. End conditional blocks with `{{ end }}`:

```text
{{ if score >= 90 }}
    Grade: A
{{ else if score >= 80 }}
    Grade: B
{{ else }}
    Grade: F
{{ end }}
```

### Loops (`for`)

Iterate over array structures using `for ... in ...`. Use `{{ break }}` to exit early or `{{ continue }}` to skip to the next iteration:

```text
Users list:
{{ for user in users }}
    {{ if user.status == "banned" }}
        {{ continue }}
    {{ end }}
    - {{ user.name }}
{{ end }}
```

### Return Statement (`return`)

Interrupt template evaluation and return a specific value or short-circuit rendering:

```text
{{ if total == 0 }}
    {{ return "No results found" }}
{{ end }}
```

## Operators & Expressions

| Operator Category | Operators | Examples |
| --- | --- | --- |
| **Arithmetic** | `+`, `-`, `*`, `/`, `%`, `**` | `{{ a + b * 2 }}`, `{{ 2 ** 10 }}`, `{{ 10 % 3 }}` |
| **Comparison** | `==`, `!=`, `>`, `<`, `>=`, `<=` | `{{ age >= 18 }}`, `{{ status == "active" }}` |
| **Logical** | `&&`, `||`, `!`, `and`, `or`, `not` | `{{ isReady && !hasError }}` |
| **Unary** | `-`, `!` | `{{ -price }}`, `{{ !flag }}` |
| **String Concatenation** | `+` | `{{ firstName + " " + lastName }}` |

## Data Structures

### Object Property Access

Access nested fields using dot notation (`.`):

```text
User: {{ user.profile.name }} (Role: {{ user.role }})
```

### Array Indexing

Access elements by zero-based index using `[index]`:

```text
First item: {{ items[0] }}
Second item: {{ items[1] }}
Nested lookup: {{ categories[0].products[2].title }}
```

## Registering Custom Functions

### Synchronous Functions

Register synchronous functions into [`Context`] using [`Context::register_function`]:

```rust
use message_template::*;

#[tokio::main]
async fn main() {
    let mut context = Context::new();
    context.register_function("square", |args| {
        if let Some(Value::Integer(n)) = args.first() {
            Ok(Value::Integer(n * n))
        } else {
            Err("Expected integer".to_string())
        }
    });
    
    assert_eq!(
        run_as_text("5 squared is {{ square(5) }}", Some(&context)).await,
        "5 squared is 25".to_string()
    );
}
```

### Asynchronous Functions

Register asynchronous functions using [`Context::register_async_function`]:

```rust
use message_template::*;

#[tokio::main]
async fn main() {
    let mut context = Context::new();
    context.register_async_function("fetchUser", |args| async move {
        let name = match args.first() {
            Some(Value::String(s)) => s.as_str(),
            _ => "Guest"
        };
        Ok(Value::String(format!("User: {}", name)))
    });

    assert_eq!(
        run_as_text("{{ fetchUser(\"Alice\") }}", Some(&context)).await,
        "User: Alice".to_string()
    );
    
    assert_eq!(
        run_as_text("{{ fetchUser() }}", Some(&context)).await,
        "User: Guest".to_string()
    );
}
```

## Serde & Context Feature (`context`)

Enable the `context` feature in your `Cargo.toml`:

```toml
message-template = { git = "https://github.com/banocean/message-template.git", features = ["context"] }
```

### Inserting Serde Structs

Directly insert any `serde::Serialize` struct into the context using `Context::insert`:

```rust
use message_template::*;

#[cfg(feature = "context")]
async fn execute() {
    #[derive(serde::Serialize)]
    struct User {
        name: String,
        age: u32,
    }

    let mut context = Context::new();
    context.insert("user", User { name: "Alice".to_string(), age: 30 });

    assert_eq!(
        run_as_text("{{ user.age }}", Some(&context)).await,
        "30".to_string()
    );
}

#[tokio::main]
async fn main() {
    #[cfg(feature = "context")]
    execute().await
}
```

### `context!` Macro

Construct contexts using a JSON-like syntax with the `context!` macro:

```rust
use message_template::*;

#[cfg(feature = "context")]
async fn execute() {
    let context = context! {
        "siteName": "My Store",
        "item": {
            "name": "Laptop",
            "price": 999.99
        }
    };
    
    assert_eq!(
        run_as_text("{{ item.name }}", Some(&context)).await, 
        "Laptop".to_string()
    );
}

#[tokio::main]
async fn main() {
    #[cfg(feature = "context")]
    execute().await
}
```

## API Overview

- [`run`]: Parses and executes a template string, returning `Result<`[`ExecutionResult`]`, String>`.
- [`run_as_text`]: Evaluates a template string and returns the rendered output directly as a `String`.
- [`run_ast`]: Executes a pre-parsed AST ([`ast::Scope`]), useful for reusing compiled template structures.
- [`run_ast_as_text`]: Executes a pre-parsed AST ([`ast::Scope`]) and returns the rendered output as a `String`.
- [`Context`]: Context structure holding variable bindings and registered custom functions for template execution.
  - [`Context::new`]: Instantiates a new empty context.
  - [`Context::insert_value`]: Inserts a key and a [`Value`] into the context.
  - `Context::insert`: *(feature `context`)* Inserts any type implementing `serde::Serialize`.
  - [`Context::register_function`]: Registers a synchronous function closure into the context.
  - [`Context::register_async_function`]: Registers an asynchronous function into the context.
- [`context!`]: Macro for initializing a [`Context`] using JSON-like key-value mapping syntax. (Supports `serde::Serialize` types when feature `context` is enabled).
- [`Value`]: Primitive data type enum (`String`, `Integer`, `Float`, `Bool`, `Array`, `Object`, `Null`).
- [`ExecutionResult`]: Outcome enum containing either [`ExecutionResult::Template`] or [`ExecutionResult::Value`].

## Contributing & Testing

For information on the test suite and contributing guidelines, please see [`CONTRIBUTING.md`](CONTRIBUTING.md).