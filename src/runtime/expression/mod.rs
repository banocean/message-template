mod binary;

use std::collections::HashMap;
use crate::parser::ast::{Expression, Identifier, IndexExpression, Literal, MemberAccessExpression, UnaryExpression, UnaryOperator};
use crate::runtime::expression::binary::evaluate_binary;
use crate::runtime::value::Value;

pub type Data<'a> = (&'a HashMap<String, Value>, &'a HashMap<String, Value>);
pub type DataMut<'a> = (&'a HashMap<String, Value>, &'a mut HashMap<String, Value>);

fn literal_to_value(literal: &Literal,) -> Value {
    match literal {
        Literal::String(s) => Value::String(s.to_string()),
        Literal::Integer(i) => Value::Integer(*i),
        Literal::Float(f) => Value::Float(*f),
        Literal::Bool(b) => Value::Bool(*b),
    }
}

fn get_variable(
    identifier: &Identifier,
    (context, environment): Data
) -> Option<Value> {
    context.get(identifier.name).or_else(|| environment.get(identifier.name)).cloned()
}

fn access_object_member(
    member_access_expression: &MemberAccessExpression,
    data: Data
) -> Result<Value, String> {
    let base_value = evaluate_expression(&member_access_expression.base, data)?;
    match base_value {
        Value::Object(obj) => {
            obj.get(member_access_expression.member.name).cloned().ok_or_else(||
                format!("There is no property named '{}'", member_access_expression.member.name)
            )
        }
        _ => Err("You can only access properties of objects".to_string())
    }
}

fn index_array(
    index_expression: &IndexExpression,
    data: Data
) -> Result<Value, String> {
    let base_value = evaluate_expression(&index_expression.base, data)?;
    let index_value = evaluate_expression(&index_expression.index, data)?;

    match (base_value, index_value) {
        (Value::Array(arr), Value::Integer(index)) => {
            if index >= 0 && index < arr.len() as i64 {
                Ok(arr[index as usize].clone())
            } else {
                Err(format!("Index {} out of bounds for array of length {}", index, arr.len()))
            }
        }
        _ => Err("Invalid operands for index access".to_string()),
    }
}

pub fn value_to_bool(value: Value) -> bool {
    match value {
        Value::Bool(b) => b,
        Value::Null => false,
        Value::Integer(i) => i != 0,
        Value::Float(f) => f != 0.0,
        _ => true
    }
}

fn evaluate_unary(
    unary_expression: &UnaryExpression,
    data: Data,
) -> Result<Value, String> {
    let value = evaluate_expression(&unary_expression.expression, data)?;
    Ok(match unary_expression.operator {
        UnaryOperator::Not => Value::Bool(!value_to_bool(value)),
        UnaryOperator::Negative => {
            match value {
                Value::Integer(i) => Value::Integer(-i),
                Value::Float(f) => Value::Float(-f),
                _ => return Err("You can only use - operator on numbers".to_string())
            }
        }
    })
}

pub fn evaluate_expression<'a>(
    expression: &Expression<'a>,
    data: Data,
) -> Result<Value, String> {
    Ok(match expression {
        Expression::Identifier(identifier) => get_variable(identifier, data)
            .ok_or(format!("Undefined variable: {}", identifier.name))?,
        Expression::Literal(literal) => literal_to_value(literal),
        Expression::Binary(bin_expr) => evaluate_binary(bin_expr, data)?,
        Expression::Unary(unary_expr) => evaluate_unary(unary_expr, data)?,
        Expression::FunctionCall(func_call) => todo!(),
        Expression::Index(index_expr) => index_array(index_expr, data)?,
        Expression::MemberAccess(member_expr) =>
            access_object_member(member_expr, data)?
    })
}
