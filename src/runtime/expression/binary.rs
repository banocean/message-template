use crate::parser::ast::{BinaryExpression, BinaryOperator};
use crate::runtime::expression::{evaluate_expression, value_to_bool, Data};
use crate::runtime::value::Value;

pub fn evaluate_binary<'a>(
    binary_expression: &BinaryExpression<'a>,
    data: Data<'a>,
) -> Result<Value, String> {
    let left_value = evaluate_expression(&binary_expression.left, data)?;
    let right_value = evaluate_expression(&binary_expression.right, data)?;

    match binary_expression.operator {
        BinaryOperator::Addition => {
            Ok(match (left_value, right_value) {
                (Value::Integer(l), Value::Integer(r)) => {
                    l.checked_add(r)
                        .map(Value::Integer)
                        .ok_or_else(|| "Integer addition overflow".to_string())?
                }

                (Value::Float(l), Value::Float(r)) => Value::Float(l + r),
                (Value::Integer(l), Value::Float(r)) => Value::Float(l as f64 + r),
                (Value::Float(l), Value::Integer(r)) => Value::Float(l + r as f64),

                (Value::String(l), Value::String(r)) => Value::String(l + &r),

                (Value::String(l), Value::Integer(r)) => Value::String(l + &r.to_string()),
                (Value::String(l), Value::Float(r)) => Value::String(l + &r.to_string()),
                (Value::String(l), Value::Bool(r)) => Value::String(l + &r.to_string()),

                (Value::Integer(l), Value::String(r)) => Value::String(l.to_string() + &r),
                (Value::Float(l), Value::String(r)) => Value::String(l.to_string() + &r),
                (Value::Bool(l), Value::String(r)) => Value::String(l.to_string() + &r),

                (l, r) => return Err(format!("Cannot add {:?} and {:?}", l, r)),
            })
        }
        BinaryOperator::Subtraction => {
            match (left_value, right_value) {
                (Value::Integer(l), Value::Integer(r)) => {
                    l.checked_sub(r)
                        .map(Value::Integer)
                        .ok_or_else(|| "Integer subtraction overflow/underflow".to_string())
                }
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(l as f64 - r)),
                (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l - r as f64)),
                (l, r) => Err(format!("Cannot subtract {l:?} and {r:?}")),
            }
        }
        BinaryOperator::Multiplication => {
            match (left_value, right_value) {
                (Value::Integer(l), Value::Integer(r)) => {
                    l.checked_mul(r)
                        .map(Value::Integer)
                        .ok_or_else(|| "Integer multiplication overflow".to_string())
                }
                (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                (Value::Integer(l), Value::Float(r)) => Ok(Value::Float(l as f64 * r)),
                (Value::Float(l), Value::Integer(r)) => Ok(Value::Float(l * r as f64)),
                (l, r) => Err(format!("Cannot multiply {:?} and {:?}", l, r)),
            }
        }
        BinaryOperator::Division => {
            match (left_value, right_value) {
                (Value::Integer(l), Value::Integer(r)) => {
                    if r == 0 {
                        return Err(format!("Can't divide {l} by zero"))
                    }

                    l.checked_div(r)
                        .map(Value::Integer)
                        .ok_or_else(|| format!("Integer division error: Can't divide {l} by {r}"))
                }
                (Value::Float(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Err(format!("Can't divide {l} by zero"))
                    } else {
                        Ok(Value::Float(l / r))
                    }
                }
                (Value::Integer(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Err(format!("Can't divide {l} by zero"))
                    } else {
                        Ok(Value::Float(l as f64 / r))
                    }
                }
                (Value::Float(l), Value::Integer(r)) => {
                    if r == 0 {
                        Err(format!("Can't divide {l} by zero"))
                    } else {
                        Ok(Value::Float(l / r as f64))
                    }
                }
                (l, r) => Err(format!("Cannot divide {l:?} by {r:?}")),
            }
        }
        BinaryOperator::Remainder => {
            match (left_value, right_value) {
                (Value::Integer(l), Value::Integer(r)) => {
                    if r == 0 {
                        return Err("Remainder by zero (integer)".to_string());
                    }
                    l.checked_rem(r)
                        .map(Value::Integer)
                        .ok_or_else(|| "Integer remainder error (e.g. i64::MIN % -1)".to_string())
                }
                (Value::Float(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Ok(Value::Float(l % r))
                    } else {
                        Ok(Value::Float(l % r))
                    }
                }
                (Value::Integer(l), Value::Float(r)) => {
                    if r == 0.0 {
                        Ok(Value::Float((l as f64) % r))
                    } else {
                        Ok(Value::Float(l as f64 % r))
                    }
                }
                (Value::Float(l), Value::Integer(r)) => {
                    if r == 0 {
                        Ok(Value::Float(l % (r as f64)))
                    } else {
                        Ok(Value::Float(l % r as f64))
                    }
                }
                (l, r) => Err(format!("Cannot apply remainder to {l:?} and {r:?}")),
            }
        }
        BinaryOperator::Exponent => {
            match (left_value, right_value) {
                (Value::Integer(base), Value::Integer(exp)) => {
                    if exp < 0 {
                        Ok(Value::Float((base as f64).powi(exp as i32)))
                    } else if exp == 0 {
                        Ok(Value::Integer(1))
                    } else if base == 0 && exp > 0 {
                        Ok(Value::Integer(0))
                    } else {
                        let u_exp = exp as u32;
                        let mut acc = 1_i64;
                        let cur_base = base;

                        if u_exp > 63 && (base > 1 || base < -1) {
                            return Err("Integer exponentiation overflow (exponent too large)".to_string());
                        }

                        for _ in 0..u_exp {
                            match acc.checked_mul(cur_base) {
                                Some(res) => acc = res,
                                None => return Err("Integer exponentiation overflow".to_string()),
                            }
                        }
                        Ok(Value::Integer(acc))
                    }
                }
                (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(exp))),
                (Value::Integer(base), Value::Float(exp)) => Ok(Value::Float((base as f64).powf(exp))),
                (Value::Float(base), Value::Integer(exp)) => Ok(Value::Float(base.powi(exp as i32))),
                (l, r) => Err(format!("Cannot use ** operator on {l:?} with exponent {r:?}")),
            }
        }

        BinaryOperator::Equal => Ok(Value::Bool(left_value == right_value)),
        BinaryOperator::NotEqual => Ok(Value::Bool(left_value != right_value)),

        BinaryOperator::Greater | BinaryOperator::Less | BinaryOperator::GreaterOrEqual | BinaryOperator::LessOrEqual => {
            let ordering = match (left_value.clone(), right_value.clone()) {
                (Value::Integer(l), Value::Integer(r)) => l.partial_cmp(&r),
                (Value::Float(l), Value::Float(r)) => l.partial_cmp(&r),
                (Value::Integer(l), Value::Float(r)) => (l as f64).partial_cmp(&r),
                (Value::Float(l), Value::Integer(r)) => l.partial_cmp(&(r as f64)),
                (Value::String(l), Value::String(r)) => l.partial_cmp(&r),
                (l, r) => return Err(format!("Cannot compare {l:?} and {r:?}")),
            };

            match ordering {
                Some(std::cmp::Ordering::Less) => Ok(Value::Bool(matches!(binary_expression.operator, BinaryOperator::Less | BinaryOperator::LessOrEqual))),
                Some(std::cmp::Ordering::Equal) => Ok(Value::Bool(matches!(binary_expression.operator, BinaryOperator::Equal | BinaryOperator::GreaterOrEqual | BinaryOperator::LessOrEqual))),
                Some(std::cmp::Ordering::Greater) => Ok(Value::Bool(matches!(binary_expression.operator, BinaryOperator::Greater | BinaryOperator::GreaterOrEqual))),
                None => Err("Comparison failed".to_string()),
            }
        }

        BinaryOperator::And => {
            Ok(Value::Bool(value_to_bool(left_value) && value_to_bool(right_value)))
        }
        BinaryOperator::Or => {
            Ok(Value::Bool(value_to_bool(left_value) || value_to_bool(right_value)))
        }
    }
}