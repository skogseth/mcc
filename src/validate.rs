use crate::Identifier;
use crate::Output;
use crate::parser as p;

use variable_map::VariableMap;
mod variable_map {
    use std::collections::HashMap;

    use super::Identifier;

    /// A map containing unique names for each identifier
    pub struct VariableMap<'a> {
        current: HashMap<Identifier, Identifier>,
        inner: Option<&'a VariableMap<'a>>,
    }

    impl VariableMap<'_> {
        pub fn new() -> Self {
            Self {
                current: HashMap::new(),
                inner: None,
            }
        }

        pub fn is_defined_here(&self, key: &Identifier) -> bool {
            self.current.contains_key(key)
        }

        pub fn add(&mut self, key: Identifier, value: Identifier) {
            self.current.insert(key, value);
        }

        pub fn get(&self, key: &Identifier) -> Option<Identifier> {
            match self.current.get(key) {
                Some(value) => Some(value.clone()),
                None => match self.inner {
                    Some(inner) => inner.get(key).clone(),
                    None => None,
                },
            }
        }
    }

    impl<'a> VariableMap<'a> {
        pub fn wrap(&'a self) -> Self {
            Self {
                current: HashMap::new(),
                inner: Some(self),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValidationError;

pub fn resolve_variables(program: &mut p::Program, output: &Output) -> Result<(), ValidationError> {
    let mut vars = VariableMap::new();
    let main: &mut p::Function = &mut program.0;
    resolve_block(&mut main.body, &mut vars, output)
}

fn resolve_block(
    block: &mut p::Block,
    variable_map: &mut VariableMap,
    output: &Output,
) -> Result<(), ValidationError> {
    let errors = block
        .0
        .iter_mut()
        .map(|block_item| match block_item {
            p::BlockItem::D(d) => resolve_decleration(d, variable_map, output),
            p::BlockItem::S(s) => resolve_statement(s, variable_map, output),
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
    if variable_map.is_defined_here(&decleration.name) {
        output.error(
            decleration.name_span,
            String::from("duplicate variable decleration"),
        );
        return Err(ValidationError);
    }

    let unique_name = Identifier::new_label(&decleration.name.0);

    // Retrieve the original name and replace it with the new unique name.
    let original_name = std::mem::replace(&mut decleration.name, unique_name.clone());

    variable_map.add(original_name, unique_name);

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

        p::Statement::If { cond, then, else_ } => {
            resolve_expression(cond, variable_map, output)?;
            resolve_statement(then, variable_map, output)?;

            if let Some(expr) = else_ {
                resolve_statement(expr, variable_map, output)?;
            }

            Ok(())
        }

        p::Statement::Compound(block) => {
            // Create new map wrapping the current one
            let mut variable_map = variable_map.wrap();
            resolve_block(block, &mut variable_map, output)
        }

        p::Statement::Break(_) | p::Statement::Continue(_) => Ok(()), // nothing to do

        p::Statement::While { cond, body, .. } | p::Statement::DoWhile { body, cond, .. } => {
            resolve_expression(cond, variable_map, output)?;
            resolve_statement(body, variable_map, output)?;
            Ok(())
        }

        p::Statement::For {
            init,
            cond,
            post,
            body,
            ..
        } => {
            // Create new map wrapping the current one
            let mut variable_map = variable_map.wrap();

            // Resolve ForInit
            match init {
                p::ForInit::D(decl) => resolve_decleration(decl, &mut variable_map, output)?,
                p::ForInit::E(Some(expr)) => resolve_expression(expr, &mut variable_map, output)?,
                p::ForInit::E(None) => {} // nothing to do
            }

            if let Some(cond) = cond {
                resolve_expression(cond, &mut variable_map, output)?;
            }

            if let Some(post) = post {
                resolve_expression(post, &mut variable_map, output)?;
            }

            resolve_statement(body, &mut variable_map, output)?;

            Ok(())
        }

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
            let unique_name = variable_map.get(&variable.name).ok_or_else(|| {
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

        p::Expression::Conditional {
            cond,
            if_true,
            if_false,
        } => {
            resolve_expression(cond, variable_map, output)?;
            resolve_expression(if_true, variable_map, output)?;
            resolve_expression(if_false, variable_map, output)?;
            Ok(())
        }
    }
}

pub fn resolve_loops(program: &mut p::Program, output: &Output) -> Result<(), ValidationError> {
    let main: &mut p::Function = &mut program.0;
    resolve_loops_block(&mut main.body, None, output)
}

fn resolve_loops_block(
    block: &mut p::Block,
    current_loop: Option<Identifier>,
    output: &Output,
) -> Result<(), ValidationError> {
    for block_item in &mut block.0 {
        match block_item {
            p::BlockItem::S(s) => resolve_loops_statement(s, current_loop.clone(), output)?,
            p::BlockItem::D(_) => {} // nothing to do
        }
    }

    Ok(())
}

fn resolve_loops_statement(
    statement: &mut p::Statement,
    current_loop: Option<Identifier>,
    output: &Output,
) -> Result<(), ValidationError> {
    match statement {
        p::Statement::Break(label) | p::Statement::Continue(label) => {
            // TODO: Return proper errors here
            *label = Some(current_loop.unwrap());
            Ok(())
        }

        #[rustfmt::skip]
        p::Statement::While { cond: _, body, label }
        | p::Statement::DoWhile { body, cond: _, label }
        | p::Statement::For { init: _, cond: _, post: _, body, label } => {
            // We here pass in the label of the loop!
            resolve_loops_statement(body, Some(label.clone()), output)
        }

        #[rustfmt::skip]
        p::Statement::If { cond: _, then, else_ } => {
            resolve_loops_statement(then, current_loop.clone(), output)?;

            if let Some(else_) = else_ {
                resolve_loops_statement(else_, current_loop, output)?;
            }

            Ok(())
        }

        p::Statement::Compound(block) => resolve_loops_block(block, current_loop, output),

        p::Statement::Return(_) | p::Statement::Expression(_) | p::Statement::Null => Ok(()), // nothing to do
    }
}
