use std::collections::HashMap;
#[cfg(feature = "context")]
use serde::Serialize;
use crate::Value;
#[cfg(feature = "context")]
use serde_json::to_value;

pub struct Context<'a> {
    data: HashMap<&'a str, Value>,
    calls: HashMap<String, fn(Vec<Value>) -> Value>,
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

    pub fn register_function(&mut self, name: impl Into<String>) {
        self.calls.insert(name.into(), |_| Value::Null);
    }
}