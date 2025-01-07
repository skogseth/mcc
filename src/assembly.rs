use std::collections::BTreeMap;

use crate::ast::Identifier;

#[derive(Debug, Clone)]
pub struct Program(pub Function);

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.0)?;

        #[cfg(target_os = "linux")]
        f.write_str(".section .note.GNU-stack,\"\",@progbits")?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub instructions: Vec<Instruction>,
    pub stack_offset: u32,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(target_os = "macos")]
        let name = format!("_{}", self.name);
        #[cfg(target_os = "linux")]
        let name = format!("{}", self.name);

        writeln!(f, "\t.globl {name}")?;
        writeln!(f, "{name}:")?;

        // Function prologue
        writeln!(f, "\tpushq\t%rbp")?;
        writeln!(f, "\tmovq\t%rsp, %rbp")?;
        writeln!(f, "\tsubq\t${}, %rsp", self.stack_offset)?;

        for instruction in &self.instructions {
            write!(f, "{instruction}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Idiv(Operand),
    Cdq,
    Ret,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mov { src, dst } => writeln!(f, "\tmovl\t{src}, {dst}"),
            Self::Unary(op, dst) => writeln!(f, "\t{op}\t{dst}"),
            Self::Binary(op, src, dst) => writeln!(f, "\t{op}\t{src}, {dst}"),
            Self::Idiv(dst) => writeln!(f, "\tidivl\t{dst}"),
            Self::Cdq => writeln!(f, "\tcdq"),
            Self::Ret => {
                writeln!(f, "\tmovq\t%rbp, %rsp")?;
                writeln!(f, "\tpopq\t%rbp")?;
                writeln!(f, "\tret")?;
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Neg,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => f.write_str("notl"),
            Self::Neg => f.write_str("negl"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => f.write_str("addl"),
            Self::Sub => f.write_str("subl"),
            Self::Mul => f.write_str("imull"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i64),
    Register(Register),
    Pseudo(Identifier),
    Stack(i64),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "${i}"),
            Self::Register(Register::Ax) => f.write_str("%eax"),
            Self::Register(Register::Dx) => f.write_str("%edx"),
            Self::Register(Register::R10) => f.write_str("%r10d"),
            Self::Register(Register::R11) => f.write_str("%r11d"),
            Self::Stack(i) => write!(f, "{i}(%rbp)"),
            Self::Pseudo(_) => panic!("no valid assembly for pseudoregister"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Register {
    Ax,
    Dx,
    R10,
    R11,
}

pub fn replace_pseudo_registers(instructions: &mut [Instruction]) -> i64 {
    let mut current_offset: i64 = 0;
    let mut offsets: BTreeMap<Identifier, i64> = BTreeMap::new();

    let mut update_register = |operand: &mut Operand| {
        let Operand::Pseudo(identifier) = operand else {
            return;
        };

        // We want to avoid cloning the identifier here
        let identifier = std::mem::take(identifier);

        let offset = offsets.entry(identifier).or_insert_with(|| {
            current_offset -= 4;
            current_offset
        });

        *operand = Operand::Stack(*offset);
    };

    for instruction in instructions {
        match instruction {
            Instruction::Mov { src, dst } => {
                update_register(src);
                update_register(dst);
            }

            Instruction::Unary(_, op) => update_register(op),

            Instruction::Binary(_, op_1, op_2) => {
                update_register(op_1);
                update_register(op_2);
            }

            Instruction::Idiv(op) => update_register(op),

            // No pseudo-registers to change here
            Instruction::Cdq | Instruction::Ret => {}
        }
    }

    current_offset
}

pub fn fix_move_instructions(instructions: Vec<Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .flat_map(|instruction| match instruction {
            Instruction::Mov {
                src: src @ Operand::Stack(_),
                dst: dst @ Operand::Stack(_),
            } => vec![
                Instruction::Mov {
                    src,
                    dst: Operand::Register(Register::R10),
                },
                Instruction::Mov {
                    src: Operand::Register(Register::R10),
                    dst,
                },
            ],

            Instruction::Binary(
                op @ (BinaryOperator::Add | BinaryOperator::Sub),
                src @ Operand::Stack(_),
                dst @ Operand::Stack(_),
            ) => vec![
                Instruction::Mov {
                    src,
                    dst: Operand::Register(Register::R10),
                },
                Instruction::Binary(op, Operand::Register(Register::R10), dst),
            ],

            Instruction::Binary(op @ BinaryOperator::Mul, src, dst @ Operand::Stack(_)) => vec![
                Instruction::Mov {
                    src: dst.clone(),
                    dst: Operand::Register(Register::R11),
                },
                Instruction::Binary(op, src, Operand::Register(Register::R11)),
                Instruction::Mov {
                    src: Operand::Register(Register::R11),
                    dst,
                },
            ],

            Instruction::Idiv(src @ Operand::Imm(_)) => vec![
                Instruction::Mov {
                    src,
                    dst: Operand::Register(Register::R10),
                },
                Instruction::Idiv(Operand::Register(Register::R10)),
            ],

            _ => vec![instruction],
        })
        .collect()
}
