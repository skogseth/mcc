use crate::Identifier;
use crate::parser::{BinaryOperator, UnaryOperator};

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
    Binary {
        op: BinaryOperator,
        src_1: Value,
        src_2: Value,
        dst: Variable,
    },
    Copy {
        src: Value,
        dst: Variable, // ?
    },
    Jump {
        target: Identifier,
    },
    JumpIfZero {
        condition: Value,
        target: Identifier,
    },
    JumpIfNotZero {
        condition: Value,
        target: Identifier,
    },
    Label(Identifier),
}

impl Instruction {
    pub fn assembly(self) -> Vec<crate::assembly::Instruction> {
        match self {
            Self::Return(value) => vec![
                crate::assembly::Instruction::Mov {
                    src: value.assembly(),
                    dst: crate::assembly::Operand::Register(crate::assembly::Register::Ax),
                },
                crate::assembly::Instruction::Ret,
            ],

            Self::Unary { op, src, dst } => {
                macro_rules! unary_op {
                    ($unary_op:expr) => {
                        vec![
                            crate::assembly::Instruction::Mov {
                                src: src.assembly(),
                                dst: dst.clone().assembly(),
                            },
                            crate::assembly::Instruction::Unary($unary_op, dst.assembly()),
                        ]
                    };
                }

                match op {
                    UnaryOperator::Complement => unary_op!(crate::assembly::UnaryOperator::Not),
                    UnaryOperator::Negate => unary_op!(crate::assembly::UnaryOperator::Neg),

                    UnaryOperator::Not => vec![
                        crate::assembly::Instruction::Cmp(
                            crate::assembly::Operand::Imm(0),
                            src.assembly(),
                        ),
                        crate::assembly::Instruction::Mov {
                            src: crate::assembly::Operand::Imm(0),
                            dst: dst.clone().assembly(),
                        },
                        crate::assembly::Instruction::SetCC(
                            crate::assembly::CondCode::E,
                            dst.assembly(),
                        ),
                    ],
                }
            }

            Self::Binary {
                op,
                src_1,
                src_2,
                dst,
            } => {
                macro_rules! idiv {
                    ($register:expr) => {
                        vec![
                            crate::assembly::Instruction::Mov {
                                src: src_1.assembly(),
                                dst: crate::assembly::Operand::Register(
                                    crate::assembly::Register::Ax,
                                ),
                            },
                            crate::assembly::Instruction::Cdq,
                            crate::assembly::Instruction::Idiv(src_2.assembly()),
                            crate::assembly::Instruction::Mov {
                                src: crate::assembly::Operand::Register($register),
                                dst: dst.assembly(),
                            },
                        ]
                    };
                }

                macro_rules! binary_op {
                    ($bin_op:expr) => {
                        vec![
                            crate::assembly::Instruction::Mov {
                                src: src_1.assembly(),
                                dst: dst.clone().assembly(),
                            },
                            crate::assembly::Instruction::Binary(
                                $bin_op,
                                src_2.assembly(),
                                dst.assembly(),
                            ),
                        ]
                    };
                }

                macro_rules! relational_op {
                    ($cc:expr) => {
                        vec![
                            crate::assembly::Instruction::Cmp(src_2.assembly(), src_1.assembly()),
                            crate::assembly::Instruction::Mov {
                                src: crate::assembly::Operand::Imm(0),
                                dst: dst.clone().assembly(),
                            },
                            crate::assembly::Instruction::SetCC($cc, dst.assembly()),
                        ]
                    };
                }

                match op {
                    BinaryOperator::Divide => idiv!(crate::assembly::Register::Ax),
                    BinaryOperator::Remainder => idiv!(crate::assembly::Register::Dx),

                    BinaryOperator::Add => binary_op!(crate::assembly::BinaryOperator::Add),
                    BinaryOperator::Subtract => binary_op!(crate::assembly::BinaryOperator::Sub),
                    BinaryOperator::Multiply => binary_op!(crate::assembly::BinaryOperator::Mul),

                    BinaryOperator::And | BinaryOperator::Or => {
                        panic!("can't generate tacky for `&&` and `||`");
                    }

                    BinaryOperator::Equal => relational_op!(crate::assembly::CondCode::E),
                    BinaryOperator::NotEqual => relational_op!(crate::assembly::CondCode::NE),
                    BinaryOperator::LessThan => relational_op!(crate::assembly::CondCode::L),
                    BinaryOperator::GreaterThan => relational_op!(crate::assembly::CondCode::G),
                    BinaryOperator::LessOrEqual => relational_op!(crate::assembly::CondCode::LE),
                    BinaryOperator::GreaterOrEqual => relational_op!(crate::assembly::CondCode::GE),

                    BinaryOperator::Assignment => todo!(),
                }
            }

            Self::Jump { target } => vec![crate::assembly::Instruction::Jmp(target)],
            Self::JumpIfZero { condition, target } => vec![
                crate::assembly::Instruction::Cmp(
                    crate::assembly::Operand::Imm(0),
                    condition.assembly(),
                ),
                crate::assembly::Instruction::JmpCC(crate::assembly::CondCode::E, target),
            ],
            Self::JumpIfNotZero { condition, target } => vec![
                crate::assembly::Instruction::Cmp(
                    crate::assembly::Operand::Imm(0),
                    condition.assembly(),
                ),
                crate::assembly::Instruction::JmpCC(crate::assembly::CondCode::NE, target),
            ],

            Self::Copy { src, dst } => vec![crate::assembly::Instruction::Mov {
                src: src.assembly(),
                dst: dst.assembly(),
            }],
            Self::Label(label) => vec![crate::assembly::Instruction::Label(label)],
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
