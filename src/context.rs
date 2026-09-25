use crate::Value;
#[cfg(feature = "serde")]
use serde::Serialize;
#[cfg(feature = "serde")]
use serde_json::to_value;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;

pub type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;
pub type AsyncFn = Box<dyn Fn(Vec<Value>) -> BoxFuture<Result<Value, String>> + Send + Sync>;

pub enum FunctionDefinition {
    Sync(Box<dyn Fn(Vec<Value>) -> Result<Value, String>>),
    Async(AsyncFn),
}

#[derive(Default)]
pub struct Context<'a> {
    data: HashMap<&'a str, Value>,
    calls: HashMap<String, FunctionDefinition>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    pub fn insert_value(&mut self, key: &'a str, value: Value) {
        self.data.insert(key, value);
    }

    #[cfg(feature = "serde")]
    pub fn insert<T: Serialize>(&mut self, key: &'a str, value: T) {
        self.data.insert(key, to_value(value).unwrap().into());
    }

    pub fn register_function(
        &mut self,
        name: impl Into<String>,
        function: impl Fn(Vec<Value>) -> Result<Value, String> + 'static,
    ) {
        self.calls
            .insert(name.into(), FunctionDefinition::Sync(Box::new(function)));
    }

    pub fn register_async_function<F, Fut>(&mut self, name: impl Into<String>, function: F)
    where
        F: Fn(Vec<Value>) -> Fut + Send + Sync + 'static,
        Fut: Future<Output = Result<Value, String>> + Send + 'static,
    {
        self.calls.insert(
            name.into(),
            FunctionDefinition::Async(Box::new(move |args| Box::pin(function(args)))),
        );
    }

    pub async fn call(&self, name: &str, args: Vec<Value>) -> Result<Value, String> {
        match self
            .calls
            .get(name)
            .ok_or_else(|| format!("Function '{}' not found", name))?
        {
            FunctionDefinition::Sync(f) => f(args),
            FunctionDefinition::Async(f) => f(args).await,
        }
    }
}

#[cfg(feature = "serde")]
#[macro_export]
macro_rules! context {
    ($($k:tt: $v:tt),* $(,)?) => {{
        let mut ctx = $crate::Context::new();
        $( $crate::context!(@ins ctx, $k, $v); )*
        ctx
    }};
    (@ins $ctx:ident, $k:expr, { $($v:tt)* }) => {
        $ctx.insert_value($k, $crate::Value::from(serde_json::json!({ $($v)* })));
    };
    (@ins $ctx:ident, $k:expr, $v:expr) => {
        $ctx.insert($k, $v);
    };
}

#[cfg(not(feature = "serde"))]
#[macro_export]
macro_rules! context {
    ($($k:tt: $v:tt),* $(,)?) => {{
        let mut ctx = $crate::Context::new();
        $( $crate::context!(@ins ctx, $k, $v); )*
        ctx
    }};
    (@ins $ctx:ident, $k:expr, $v:expr) => {
        $ctx.insert_value($k, $crate::Value::from($v));
    };
}

#[cfg(test)]
mod tests {
    use crate::{Context, Value};
    #[cfg(feature = "serde")]
    use serde::Serialize;
    #[cfg(feature = "serde")]
    use std::collections::HashMap;

    #[cfg(feature = "serde")]
    #[test]
    fn try_context() {
        let context = context! {
            "a": "123",
            "b": { "a": "test" },
            "c": 123.1
        };

        assert_eq!(context.get("a"), Some(&Value::String("123".to_string())));

        let expected_object = Value::Object(HashMap::from([(
            "a".to_string(),
            Value::String("test".to_string()),
        )]));
        assert_eq!(context.get("b"), Some(&expected_object));

        assert_eq!(context.get("c"), Some(&Value::Float(123.1)));
    }

    async fn run_values(values: Vec<Value>) -> Result<Value, String> {
        if values.len() > 1 {
            return Err("Too many arguments".to_string());
        }

        if let Some(Value::String(value)) = values.first() {
            Ok(Value::String(value.clone() + " test"))
        } else {
            Ok(Value::Null)
        }
    }

    #[tokio::test]
    async fn try_call_fn() {
        let mut context = Context::new();
        context.register_async_function("test", run_values);

        assert_eq!(context.call("test", Vec::new()).await, Ok(Value::Null));

        assert_eq!(
            context.call("test", vec![Value::Null, Value::Null]).await,
            Err("Too many arguments".to_string())
        );

        assert_eq!(
            context
                .call("test", vec![Value::String("test".to_string())])
                .await,
            Ok(Value::String("test test".to_string()))
        )
    }

    #[cfg(feature = "serde")]
    #[derive(Serialize, Clone)]
    struct User {
        name: String,
        ids: Vec<u64>,
    }

    #[cfg(feature = "serde")]
    #[tokio::test]
    async fn index_struct() {
        let user = User {
            name: "test".to_string(),
            ids: vec![1, 2],
        };

        let mut context = Context::new();
        context.insert("user", user.clone());

        let expected_user = Some(&Value::Object(HashMap::from([
            ("name".to_string(), Value::String("test".to_string())),
            (
                "ids".to_string(),
                Value::Array(vec![Value::Integer(1), Value::Integer(2)]),
            ),
        ])));

        assert_eq!(context.get("user"), expected_user);

        let context = context! {
            "user": user
        };

        assert_eq!(context.get("user"), expected_user);
    }
}
