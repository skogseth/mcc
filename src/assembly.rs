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
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(target_os = "macos")]
        let name = format!("_{}", self.name);
        #[cfg(target_os = "linux")]
        let name = format!("{}", self.name);

        writeln!(f, "\t.globl {name}")?;
        writeln!(f, "{name}:")?;

        for instruction in &self.instructions {
            writeln!(f, "\t{instruction}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov { src: Operand, dst: Operand },
    Ret,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mov { src, dst } => write!(f, "movl\t{src}, {dst}"),
            Self::Ret => f.write_str("ret"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operand {
    Imm(i64),
    Register, // TODO: Support more than one register
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "${i}"),
            Self::Register => f.write_str("%eax"),
        }
    }
}
