use crate::ast::Identifier;

#[derive(Debug, Clone)]
pub struct Program(pub Function);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Return(Value),
    Unary {
        op: UnaryOperator,
        src: Value,
        dst: Variable,
    },
}

#[derive(Debug, Clone)]
pub enum Value {
    Constant(i64),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub struct Variable(pub Identifier);

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Complement,
    Negate,
}
