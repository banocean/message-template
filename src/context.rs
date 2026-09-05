use crate::Value;
#[cfg(feature = "context")]
use serde::Serialize;
#[cfg(feature = "context")]
use serde_json::to_value;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;

pub type BoxFuture<T> = Pin<Box<dyn Future<Output = T> + Send>>;

pub enum FunctionDefinition {
    Sync(Box<dyn Fn(Vec<Value>) -> Result<Value, String>>),
    Async(Box<dyn Fn(Vec<Value>) -> BoxFuture<Result<Value, String>> + Send + Sync>),
}

pub struct Context<'a> {
    data: HashMap<&'a str, Value>,
    calls: HashMap<String, FunctionDefinition>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            data: Default::default(),
            calls: Default::default(),
        }
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.data.get(key)
    }

    pub fn insert_value(&mut self, key: &'a str, value: Value) {
        self.data.insert(key, value);
    }

    #[cfg(feature = "context")]
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

    // Something you'll want: a uniform way to actually call either kind.
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
