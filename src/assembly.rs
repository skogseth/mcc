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
    pub stack_offset: i64,
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(target_os = "macos")]
        let name = format!("_{}", self.name);
        #[cfg(target_os = "linux")]
        let name = format!("{}", self.name);

        writeln!(f, "\t.globl {name}")?;
        writeln!(f, "{name}:")?;

        // Write instructions to allocate stack space
        todo!();

        for instruction in &self.instructions {
            writeln!(f, "\t{instruction}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Unary(UnaryOperator, Operand),
    Ret,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mov { src, dst } => write!(f, "movl\t{src}, {dst}"),
            Self::Unary(_, _) => todo!(),
            Self::Ret => f.write_str("ret"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Neg,
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
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Register {
    Ax,
    R10,
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

            Instruction::Unary(_, dst) => update_register(dst),

            // No pseudo-registers to change here
            Instruction::Ret => {}
        }
    }

    current_offset
}

pub fn fix_move_instructions(instructions: Vec<Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .flat_map(|instruction| match instruction {
            Instruction::Mov { src, dst } => match (src, dst) {
                (src @ Operand::Stack(_), dst @ Operand::Stack(_)) => vec![
                    Instruction::Mov {
                        src,
                        dst: Operand::Register(Register::R10),
                    },
                    Instruction::Mov {
                        src: Operand::Register(Register::R10),
                        dst,
                    },
                ],
                (src, dst) => vec![Instruction::Mov { src, dst }],
            },
            _ => vec![instruction],
        })
        .collect()
}
