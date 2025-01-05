use std::sync::atomic::Ordering;
use std::vec::IntoIter;
use std::{iter::Peekable, sync::atomic::AtomicUsize};

use anyhow::{anyhow, Context};

use crate::lexer::{Keyword, Operator, Token};

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

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new_temp() -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let value = COUNTER.fetch_add(1, Ordering::Relaxed);
        assert_ne!(value, usize::MAX, "max number of temp values exceeded");
        Self(format!("tmp.{value}"))
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Program(pub Function);

impl Program {
    pub fn emit_tacky(self) -> crate::tacky::Program {
        crate::tacky::Program(self.0.emit_tacky())
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

    fn emit_tacky(self) -> crate::tacky::Function {
        let mut instructions = Vec::new();
        self.body.emit_tacky(&mut instructions);

        crate::tacky::Function {
            name: self.name,
            body: instructions,
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

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        match self {
            Self::Return(expr) => {
                let value = expr.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::Return(value));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i64),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

impl Expression {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        Self::parse_expr(tokens, 0)
    }

    fn parse_expr(tokens: &mut TokenIter, min_prec: u32) -> Result<Self, anyhow::Error> {
        let mut left = Self::parse_factor(tokens)?;

        while let Some(bin_op) = tokens.peek().and_then(BinaryOperator::parse) {
            // Check precedence of operator
            if bin_op.precedence() < min_prec {
                break;
            }

            // Then remember to progress the iterator
            let _ = tokens.next();

            let right = Self::parse_expr(tokens, bin_op.precedence() + 1)?;
            left = Expression::Binary(bin_op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_factor(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        match tokens.next() {
            Some(Token::Constant(i)) => Ok(Expression::Constant(i)),

            Some(Token::Operator(Operator::Minus)) => Ok(Self::Unary(
                UnaryOperator::Negate,
                Box::new(Self::parse_expr(tokens, 0)?),
            )),
            Some(Token::Operator(Operator::Tilde)) => Ok(Self::Unary(
                UnaryOperator::Complement,
                Box::new(Self::parse_expr(tokens, 0)?),
            )),
            Some(Token::Operator(op)) => Err(anyhow!("operator {op:?} not implemented")),

            Some(Token::OpenParenthesis) => {
                let expr = Self::parse_expr(tokens, 0)?;

                match tokens.next() {
                    Some(Token::CloseParenthesis) => Ok(expr),
                    Some(t) => Err(anyhow!("expected close parenthesis, found {t:?}")),
                    None => Err(anyhow!("expected close parenthesis, found nothing")),
                }
            }

            Some(t) => Err(anyhow!("unknown token found for expression: {t:?}")),
            None => Err(anyhow!("no token found for expression")),
        }
    }

    fn emit_tacky(&self, instructions: &mut Vec<crate::tacky::Instruction>) -> crate::tacky::Value {
        match self {
            Self::Constant(c) => return crate::tacky::Value::Constant(*c),
            Self::Unary(unary_op, expr) => {
                let src = expr.emit_tacky(instructions);
                let dst = crate::tacky::Variable(Identifier::new_temp());

                instructions.push(crate::tacky::Instruction::Unary {
                    op: unary_op.into_tacky(),
                    src,
                    dst: dst.clone(),
                });

                crate::tacky::Value::Variable(dst)
            }
            Self::Binary(_, _, _) => unimplemented!("can't generate tacky for binary operation"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Complement,
    Negate,
}

impl UnaryOperator {
    fn into_tacky(self) -> crate::tacky::UnaryOperator {
        match self {
            Self::Complement => crate::tacky::UnaryOperator::Complement,
            Self::Negate => crate::tacky::UnaryOperator::Negate,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
}

impl BinaryOperator {
    fn parse(token: &Token) -> Option<Self> {
        match token {
            Token::Operator(Operator::Plus) => Some(BinaryOperator::Add),
            Token::Operator(Operator::Minus) => Some(BinaryOperator::Subtract),
            Token::Operator(Operator::Asterisk) => Some(BinaryOperator::Multiply),
            Token::Operator(Operator::Slash) => Some(BinaryOperator::Divide),
            Token::Operator(Operator::Percent) => Some(BinaryOperator::Remainder),
            _ => return None,
        }
    }

    fn precedence(self) -> u32 {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract => 45,
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder => 50,
        }
    }
}
