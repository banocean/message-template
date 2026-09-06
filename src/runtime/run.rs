use crate::lexer::iterate::Lexer;
use crate::parser::ast::Scope;
use crate::parser::iterate::Parser;
use crate::runtime::scope::{execute_scope, ScopeExecutionResult};
use crate::runtime::value::Value;
use crate::Context;
use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub enum ExecutionResult {
    Template(String),
    Value(Value),
}

pub async fn run<'a>(
    data: String,
    context: Option<&Context<'a>>,
) -> Result<ExecutionResult, String> {
    let lexer = Lexer::new(&*data);
    let parser = Parser::new(lexer);
    let ast = parser.parse().map_err(|err| err.to_string())?;
    run_ast(&ast, context).await
}

pub async fn run_ast<'a>(
    ast: &Scope<'a>,
    context: Option<&Context<'a>>,
) -> Result<ExecutionResult, String> {
    let context = match context {
        Some(context) => context,
        None => &Context::new(),
    };

    let mut environment = HashMap::new();
    Ok(match execute_scope(ast, context, &mut environment).await? {
        ScopeExecutionResult::Normal(output) => ExecutionResult::Template(output),
        ScopeExecutionResult::Return(Some(value)) => ExecutionResult::Value(value),
        ScopeExecutionResult::Return(None) => ExecutionResult::Value(Value::Null),
        ScopeExecutionResult::Continue => ExecutionResult::Value(Value::Null),
        ScopeExecutionResult::Break => ExecutionResult::Value(Value::Null),
    })
}

fn to_text_output(result: Result<ExecutionResult, String>) -> String {
    match result {
        Ok(ExecutionResult::Template(output)) => output,
        Ok(ExecutionResult::Value(value)) => value.to_string(),
        Err(err) => format!("ERROR: {err}"),
    }
}

pub async fn run_as_text<'a>(data: String, context: Option<&Context<'a>>) -> String {
    to_text_output(run(data, context).await)
}

pub async fn run_ast_as_text<'a>(ast: &Scope<'a>, context: Option<&Context<'a>>) -> String {
    to_text_output(run_ast(ast, context).await)
}
