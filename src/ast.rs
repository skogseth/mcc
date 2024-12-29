use std::iter::Peekable;
use std::vec::IntoIter;

use anyhow::anyhow;

use crate::lexer::{Keyword, Token};

pub fn parse(tokens: Vec<Token>) -> Result<Program, anyhow::Error> {
    let mut tokens = tokens.into_iter().peekable();

    let main = Function::parse(&mut tokens)?;
    assert_eq!(main.name.0.as_str(), "main");

    let remaining_tokens: Vec<Token> = tokens.collect();
    if !remaining_tokens.is_empty() {
        return Err(anyhow!(
            "Unexpected token(s) found after main: {remaining_tokens:?}"
        ));
    }

    Ok(Program(main))
}

pub type TokenIter<'a> = Peekable<IntoIter<Token>>;

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Program(pub Function);

impl Program {
    pub fn assembly(self) -> crate::assembly::Program {
        crate::assembly::Program(self.0.assembly())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    name: Identifier,
    body: Statement,
}

impl Function {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        match tokens.next() {
            Some(Token::Keyword(Keyword::Int)) => {}
            Some(t) => return Err(anyhow!("expected return type, found {t:?}")),
            None => return Err(anyhow!("no return_type found")),
        }

        let function_name = match tokens.next() {
            Some(Token::Identifier(s)) => Identifier(s),
            Some(t) => return Err(anyhow!("failed to find function identifier, found {t:?}")),
            None => return Err(anyhow!("failed to find function identifier")),
        };

        if !matches!(tokens.next(), Some(Token::OpenParenthesis)) {
            return Err(anyhow!(
                "no open-parenthesis found for function {function_name}"
            ));
        }

        assert!(matches!(tokens.next(), Some(Token::Keyword(Keyword::Void))));

        if !matches!(tokens.next(), Some(Token::CloseParenthesis)) {
            return Err(anyhow!(
                "no close-parenthesis found for function {function_name}"
            ));
        }

        if !matches!(tokens.next(), Some(Token::OpenBrace)) {
            return Err(anyhow!("No open brace found for function {function_name}"));
        }

        let statement = Statement::parse(tokens)?;

        if !matches!(tokens.next(), Some(Token::CloseBrace)) {
            return Err(anyhow!("No close brace found for function {function_name}"));
        }

        Ok(Function {
            name: function_name,
            body: statement,
        })
    }

    fn assembly(self) -> crate::assembly::Function {
        crate::assembly::Function {
            name: self.name,
            instructions: self.body.assembly(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
}

impl Statement {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        let remove_semicolon = |tokens: &mut TokenIter| match tokens.next() {
            Some(Token::Semicolon) => Ok(()),
            Some(t) => Err(anyhow!("expected semicolon (';'), found {t:?}")),
            None => Err(anyhow!("expected semicolon (';'), but no token was found")),
        };

        match tokens.next() {
            Some(Token::Keyword(Keyword::Return)) => {
                let expr = Expression::parse(tokens)?;
                remove_semicolon(tokens)?;
                Ok(Statement::Return(expr))
            }

            Some(t) => Err(anyhow!("unexpected token found for statement: {t:?}")),
            None => Err(anyhow!("no token found for statement")),
        }
    }

    fn assembly(self) -> Vec<crate::assembly::Instruction> {
        match self {
            Self::Return(Expression::Constant(i)) => {
                vec![
                    crate::assembly::Instruction::Mov {
                        src: crate::assembly::Operand::Imm(i),
                        dst: crate::assembly::Operand::Register,
                    },
                    crate::assembly::Instruction::Ret,
                ]
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i64),
}

impl Expression {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        match tokens.next() {
            Some(Token::Constant(i)) => Ok(Expression::Constant(i)),
            Some(t) => Err(anyhow!("unknown token found for expression: {t:?}")),
            None => Err(anyhow!("no token found for expression")),
        }
    }
}
