use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    String(String),
    Integer(i64),
    Float(f64),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, value) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Object(obj) => {
                write!(f, "{{")?;
                for (i, (key, value)) in obj.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[cfg(feature = "context")]
impl From<serde_json::Value> for Value {
    fn from(value: serde_json::Value) -> Self {
        match value {
            serde_json::Value::Null => Value::Null,
            serde_json::Value::Bool(bool) => Value::Bool(bool),
            serde_json::Value::Number(number) => {
                if number.is_f64() {
                    Value::Float(number.as_f64().unwrap())
                } else if number.is_i64() {
                    Value::Integer(number.as_i64().unwrap())
                } else if number.is_u64() && number.as_u64().unwrap() <= i64::MAX as u64 {
                    Value::Integer(number.as_u64().unwrap() as i64)
                } else {
                    Value::Integer(i64::MAX)
                }
            }
            serde_json::Value::String(string) => Value::String(string),
            serde_json::Value::Array(array) => {
                Value::Array(array.into_iter().map(Value::from).collect())
            }
            serde_json::Value::Object(object) => Value::Object(
                object
                    .into_iter()
                    .map(|(key, value)| (key, Value::from(value)))
                    .collect(),
            ),
        }
    }
}
