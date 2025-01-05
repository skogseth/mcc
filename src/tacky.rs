use crate::ast::Identifier;

#[derive(Debug, Clone)]
pub struct Program(pub Function);

impl Program {
    pub fn assembly(self) -> crate::assembly::Program {
        crate::assembly::Program(self.0.assembly())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Vec<Instruction>,
}

impl Function {
    pub fn assembly(self) -> crate::assembly::Function {
        let mut instructions: Vec<crate::assembly::Instruction> = self
            .body
            .into_iter()
            .flat_map(|instruction| instruction.assembly())
            .collect();

        let offset = crate::assembly::replace_pseudo_registers(&mut instructions[..]);
        let instructions = crate::assembly::fix_move_instructions(instructions);

        crate::assembly::Function {
            name: self.name,
            instructions,
            stack_offset: offset.abs() as u32,
        }
    }
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

impl Instruction {
    pub fn assembly(self) -> impl Iterator<Item = crate::assembly::Instruction> {
        match self {
            Self::Return(value) => [
                crate::assembly::Instruction::Mov {
                    src: value.assembly(),
                    dst: crate::assembly::Operand::Register(crate::assembly::Register::Ax),
                },
                crate::assembly::Instruction::Ret,
            ]
            .into_iter(),
            Self::Unary { op, src, dst } => [
                crate::assembly::Instruction::Mov {
                    src: src.assembly(),
                    dst: dst.clone().assembly(),
                },
                crate::assembly::Instruction::Unary(op.assembly(), dst.assembly()),
            ]
            .into_iter(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Constant(i64),
    Variable(Variable),
}

impl Value {
    pub fn assembly(self) -> crate::assembly::Operand {
        match self {
            Self::Constant(i) => crate::assembly::Operand::Imm(i),
            Self::Variable(var) => var.assembly(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Variable(pub Identifier);

impl Variable {
    pub fn assembly(self) -> crate::assembly::Operand {
        crate::assembly::Operand::Pseudo(self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

impl UnaryOperator {
    pub fn assembly(self) -> crate::assembly::UnaryOperator {
        match self {
            Self::Complement => crate::assembly::UnaryOperator::Not,
            Self::Negate => crate::assembly::UnaryOperator::Neg,
        }
    }
}
