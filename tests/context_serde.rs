#[cfg(feature = "serde")]
use message_template::*;

#[cfg(feature = "serde")]
#[tokio::test]
async fn test_e2e_serde_context_integration() {
    #[derive(serde::Serialize)]
    struct User {
        username: String,
        active: bool,
    }

    let user = User {
        username: "serdeuser".to_string(),
        active: true,
    };

    let mut context = Context::new();
    context.insert("user", user);

    let template =
        "User {{ user.username }} is {{ if user.active }}active{{ else }}inactive{{ end }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "User serdeuser is active");
}

#[cfg(feature = "serde")]
#[tokio::test]
async fn test_e2e_context_macro() {
    #[derive(serde::Serialize)]
    struct Item {
        name: String,
        price: f64,
    }

    let item = Item {
        name: "Widget".to_string(),
        price: 19.99,
    };

    let context = context! {
        "title": "Store Catalog",
        "item": item
    };

    let template = "{{ title }}: {{ item.name }} costs ${{ item.price }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Store Catalog: Widget costs $19.99");
}

#[cfg(feature = "serde")]
#[tokio::test]
async fn test_e2e_serde_nested_structs() {
    #[derive(serde::Serialize)]
    struct Role {
        title: String,
    }

    #[derive(serde::Serialize)]
    struct Account {
        id: u64,
        role: Role,
    }

    let account = Account {
        id: 101,
        role: Role {
            title: "Admin".to_string(),
        },
    };

    let mut context = Context::new();
    context.insert("account", account);

    let template = "Account ID {{ account.id }} has role {{ account.role.title }}";
    let result = run_as_text(template, Some(&context)).await;
    assert_eq!(result, "Account ID 101 has role Admin");
}
