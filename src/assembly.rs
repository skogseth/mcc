use crate::ast::Identifier;

#[derive(Debug, Clone)]
pub struct Program(pub Function);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i64),
    Register, // TODO: Support more than one register
}
