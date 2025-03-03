use std::collections::HashMap;

use crate::Identifier;
use crate::Output;
use crate::parser as p;

/// A map containing unique names for each identifier
type VariableMap = HashMap<Identifier, Identifier>;

#[derive(Debug, Clone)]
pub struct ValidationError;

pub fn main(program: &mut p::Program, output: &Output) -> Result<(), ValidationError> {
    let mut vars: VariableMap = HashMap::new();

    let main: &mut p::Function = &mut program.0;
    let errors = main
        .body
        .iter_mut()
        .map(|block_item| match block_item {
            p::BlockItem::D(d) => resolve_decleration(d, &mut vars, output),
            p::BlockItem::S(s) => resolve_statement(s, &mut vars, output),
        })
        .filter_map(Result::err)
        .collect::<Vec<ValidationError>>();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(ValidationError)
    }
}

fn resolve_decleration(
    decleration: &mut p::Declaration,
    variable_map: &mut VariableMap,
    output: &Output,
) -> Result<(), ValidationError> {
    if variable_map.contains_key(&decleration.name) {
        output.error(
            decleration.name_span,
            String::from("duplicate variable decleration"),
        );
        return Err(ValidationError);
    }

    let unique_name = Identifier::new_label(&decleration.name.0);

    // Retrieve the original name and replace it with the new unique name.
    let original_name = std::mem::replace(&mut decleration.name, unique_name.clone());

    variable_map.insert(original_name, unique_name);

    if let Some(init) = &mut decleration.init {
        resolve_expression(init, variable_map, output)?;
    }

    Ok(())
}

fn resolve_statement(
    statement: &mut p::Statement,
    variable_map: &mut VariableMap,
    output: &Output,
) -> Result<(), ValidationError> {
    match statement {
        p::Statement::Return(expr) => resolve_expression(expr, variable_map, output),
        p::Statement::Expression(expr) => resolve_expression(expr, variable_map, output),
        p::Statement::Null => Ok(()), // nothing to do
    }
}

fn resolve_expression(
    expression: &mut p::Expression,
    variable_map: &mut VariableMap,
    output: &Output,
) -> Result<(), ValidationError> {
    match expression {
        p::Expression::Assignment(left, right) => {
            if !matches!(&**left, p::Expression::Var(_)) {
                // TODO: Output proper error message here
                // NOTE: This is blocked by multi-line span
                eprintln!("invalid lvalue encountered");
                return Err(ValidationError);
            }

            let result_left = resolve_expression(left, variable_map, output);
            let result_right = resolve_expression(right, variable_map, output);
            result_left.and(result_right)
        }

        p::Expression::Var(variable) => {
            let unique_name = variable_map.get(&variable.name).cloned().ok_or_else(|| {
                output.error(variable.span, String::from("undeclared variable"));
                ValidationError
            })?;
            variable.name = unique_name;
            Ok(())
        }

        p::Expression::Unary(_, expr) => resolve_expression(expr, variable_map, output),

        p::Expression::Binary(_, left, right) => {
            let result_left = resolve_expression(left, variable_map, output);
            let result_right = resolve_expression(right, variable_map, output);
            result_left.and(result_right)
        }

        p::Expression::Constant(_) => Ok(()), // nothing to do
    }
}
