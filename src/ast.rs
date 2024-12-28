use anyhow::anyhow;

use crate::lexer::{Keyword, Token};

pub fn parse(tokens: Vec<Token>) -> Result<Program, anyhow::Error> {
    let return_type = tokens[0].clone();
    assert!(matches!(return_type, Token::Keyword(Keyword::Int)));

    let function_name = match tokens[1].clone() {
        Token::Identifier(s) => Identifier(s),
        _ => return Err(anyhow!("failed to find function identifier")),
    };

    if !matches!(tokens[2], Token::OpenParenthesis) {
        return Err(anyhow!(
            "No open-parenthesis found for function {function_name}"
        ));
    }

    assert!(matches!(tokens[3], Token::Keyword(Keyword::Void)));

    if !matches!(tokens[4], Token::CloseParenthesis) {
        return Err(anyhow!(
            "No close-parenthesis found for function {function_name}"
        ));
    }

    if !matches!(tokens[5], Token::OpenBrace) {
        return Err(anyhow!("No open brace found for function {function_name}"));
    }

    let body_unparsed: Vec<Token> = tokens[6..]
        .iter()
        .take_while(|t| !matches!(t, Token::CloseBrace))
        .cloned()
        .collect();

    let new_len = 6 + body_unparsed.len();
    if !matches!(tokens.get(new_len), Some(Token::CloseBrace)) {
        return Err(anyhow!("No close brace found for function {function_name}"));
    }
    if let Some(t) = tokens.get(new_len + 1) {
        return Err(anyhow!(
            "Unexpected token found after function {function_name}: {t:?}"
        ));
    }

    let mut statement_iter = body_unparsed.split(|token| matches!(token, Token::Semicolon));
    if !matches!(statement_iter.next_back(), Some([])) {
        return Err(anyhow!(
            "body of {function_name} does not end with a semicolon"
        ));
    }
    let statements: Vec<Statement> = statement_iter
        .map(Statement::parse)
        .collect::<Result<_, _>>()?;

    assert_eq!(statements.len(), 1);

    assert_eq!(function_name.0.as_str(), "main");

    let main = Function {
        name: function_name,
        body: statements[0].clone(),
    };

    Ok(Program(main))
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.0)
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
    fn parse(tokens: &[Token]) -> Result<Self, anyhow::Error> {
        // TODO: Support more statement types
        match tokens {
            [Token::Keyword(Keyword::Return), expr @ ..] => {
                let expr = Expression::parse(expr)?;
                Ok(Statement::Return(expr))
            }

            _ => Err(anyhow!(
                "sequence of tokens is not a known statement: {tokens:?}"
            )),
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
    fn parse(tokens: &[Token]) -> Result<Self, anyhow::Error> {
        // TODO: Support more expression types
        match tokens {
            [Token::Constant(i)] => Ok(Expression::Constant(*i)),

            _ => Err(anyhow!(
                "sequence of tokens is not a known expression: {tokens:?}"
            )),
        }
    }
}
