use std::collections::HashMap;
use crate::parser::ast::{Expression, ProgramFlow, Scope, Statement};
use crate::runtime::expression::{evaluate_expression, value_to_bool, Data};
use crate::runtime::value::Value;

#[derive(Debug, PartialEq)]
pub(crate) enum ScopeExecutionResult {
    /// Generated template
    Normal(String),
    /// Returned value
    Return(Option<Value>),
    Break,
    Continue,
}

pub(crate) fn execute_scope<'a>(
    scope: &'a Scope<'a>,
    context: &'a HashMap<String, Value>,
    environment: &'a mut HashMap<String, Value>,
) -> Result<ScopeExecutionResult, String> {
    let mut accumulated_content = String::new();

    for flow in &scope.0 {
        match flow {
            ProgramFlow::Content(content) => accumulated_content.push_str(content),
            ProgramFlow::Statement(statement) => {
                match statement {
                    Statement::Let { identifier, expression } => {
                        let value = evaluate_expression(&expression, (context, environment))?;
                        environment.insert(identifier.name.to_string(), value);
                    }
                    Statement::For { identifier, iterable, body } => {
                        let iterable_value = evaluate_expression(iterable, (context, environment))?;
                        match iterable_value {
                            Value::Array(arr) => {
                                for item in arr {
                                    let mut loop_environment = environment.clone();
                                    loop_environment.insert(identifier.name.to_string(), item.clone());

                                    match execute_scope(body, &context, &mut loop_environment)? {
                                        ScopeExecutionResult::Normal(content) => accumulated_content.push_str(&content),
                                        ScopeExecutionResult::Return(value) => return Ok(ScopeExecutionResult::Return(value)),
                                        ScopeExecutionResult::Break => break,
                                        ScopeExecutionResult::Continue => continue,
                                    }
                                }
                            }
                            _ => return Err(format!("for loops can only iterate over arrays, {iterable_value:?} is not an array")),
                        }
                    }
                    Statement::If {
                        condition,
                        then_block,
                        else_block,
                        else_if_blocks
                    } => {
                        match execute_if_statement(
                            condition, then_block, else_block, else_if_blocks, context, environment
                        )? {
                            ScopeExecutionResult::Normal(content) => accumulated_content.push_str(&content),
                            ScopeExecutionResult::Return(value) => return Ok(ScopeExecutionResult::Return(value)),
                            ScopeExecutionResult::Break => return Ok(ScopeExecutionResult::Break),
                            ScopeExecutionResult::Continue => return Ok(ScopeExecutionResult::Continue),
                        }
                    }
                    Statement::Return(expression) => {
                        let value = match expression {
                            Some(expr) => Some(evaluate_expression(&expr, (context, environment))?),
                            None => None
                        };
                        return Ok(ScopeExecutionResult::Return(value));
                    }
                    Statement::Break => return Ok(ScopeExecutionResult::Break),
                    Statement::Continue => return Ok(ScopeExecutionResult::Continue),
                    Statement::Display(expression) => {
                        let value = evaluate_expression(expression, (context, environment))?;
                        accumulated_content.push_str(&format!("{}", value));
                    }
                }
            }
        }
    }

    Ok(ScopeExecutionResult::Normal(accumulated_content))
}

fn execute_if_statement(
    condition: &Expression,
    then_block: &Scope,
    else_block: &Option<Scope>,
    else_if_blocks: &Vec<(Expression, Scope)>,
    context: &HashMap<String, Value>,
    environment: &mut HashMap<String, Value>,
) -> Result<ScopeExecutionResult, String> {
    let combined_data: Data = (context, environment);

    let condition_value = evaluate_expression(&condition, combined_data)?;
        if value_to_bool(condition_value) {
            let mut then_environment = HashMap::new();
            let then_context = environment;
            execute_scope(&then_block, then_context, &mut then_environment)
        } else {
            for (condition, block) in else_if_blocks{
                let else_if_condition_value = evaluate_expression(condition, combined_data)?;
                if let Value::Bool(else_if_condition) = else_if_condition_value {
                    if else_if_condition {
                        let mut else_if_environment = HashMap::new();
                        let else_if_context = environment;
                        return execute_scope(block, else_if_context, &mut else_if_environment);
                    }
                } else {
                    return Err(format!("Else if condition must be a boolean, but got {:?}", else_if_condition_value));
                }
            }

            if let Some(else_branch) = else_block {
                let mut else_environment = HashMap::new();
                let else_context = environment;
                execute_scope(else_branch, else_context, &mut else_environment)
            } else {
                Ok(ScopeExecutionResult::Normal(String::new()))
            }
        }
}
