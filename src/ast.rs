use std::sync::atomic::Ordering;
use std::vec::IntoIter;
use std::{iter::Peekable, sync::atomic::AtomicUsize};

use anyhow::{anyhow, Context};

use crate::lexer::{Keyword, Operator, Token};

pub fn parse(tokens: Vec<Token>) -> Result<Program, anyhow::Error> {
    let mut tokens = tokens.into_iter().peekable();

    let main = match Function::parse(&mut tokens) {
        Ok(func) => func,
        Err(e) => {
            let tokens_left: Vec<_> = tokens.collect();
            return Err(e).with_context(|| format!("tokens left: {tokens_left:?}"));
        }
    };
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

    pub fn new_label(prefix: &str) -> Self {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let value = COUNTER.fetch_add(1, Ordering::Relaxed);
        assert_ne!(value, usize::MAX, "max number of temp values exceeded");
        Self(format!("{prefix}.{value}"))
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
    body: Vec<BlockItem>,
}

impl Function {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        match tokens.next() {
            Some(Token::Keyword(Keyword::Int)) => {}
            Some(t) => return Err(anyhow!("expected return type, found {t:?}")),
            None => return Err(anyhow!("no return_type found")),
        }

        let name = match tokens.next() {
            Some(Token::Identifier(s)) => Identifier(s),
            Some(t) => return Err(anyhow!("failed to find function identifier, found {t:?}")),
            None => return Err(anyhow!("failed to find function identifier")),
        };

        if !matches!(tokens.next(), Some(Token::OpenParenthesis)) {
            return Err(anyhow!("no open-parenthesis found for function {name}"));
        }

        assert!(matches!(tokens.next(), Some(Token::Keyword(Keyword::Void))));

        if !matches!(tokens.next(), Some(Token::CloseParenthesis)) {
            return Err(anyhow!("no close-parenthesis found for function {name}"));
        }

        if !matches!(tokens.next(), Some(Token::OpenBrace)) {
            return Err(anyhow!("No open brace found for function {name}"));
        }

        let mut body = Vec::new();
        loop {
            match tokens.peek() {
                Some(Token::CloseBrace) => break,
                Some(_) => {
                    let block_item = BlockItem::parse(tokens)
                        .with_context(|| format!("body parsed: {body:?}"))?;
                    body.push(block_item);
                }
                None => return Err(anyhow!("No close brace found for function {name}")),
            }
        }

        if !matches!(tokens.next(), Some(Token::CloseBrace)) {
            return Err(anyhow!("No close brace found for function {name}"));
        }

        Ok(Function { name, body })
    }

    fn emit_tacky(self) -> crate::tacky::Function {
        let mut instructions = Vec::new();
        for block_item in self.body {
            block_item.emit_tacky(&mut instructions);
        }

        crate::tacky::Function {
            name: self.name,
            body: instructions,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

impl BlockItem {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        match tokens.peek() {
            Some(Token::Keyword(Keyword::Int)) => Declaration::parse(tokens).map(BlockItem::D),
            Some(_) => Statement::parse(tokens).map(BlockItem::S),
            None => Err(anyhow!("No token found for block item")),
        }
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Identifier,
    pub init: Option<Expression>,
}

impl Declaration {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        let _typ = tokens.next().expect("can't parse decleration without type");

        const MESSAGE_1: &str = "expected type to be followed by identifier in declaration";
        let name = match tokens.next() {
            Some(Token::Identifier(identifier)) => Identifier(identifier),
            Some(token) => return Err(anyhow!("{MESSAGE_1}, but found {token:?}")),
            None => return Err(anyhow!("{MESSAGE_1}, but no token was found")),
        };

        const MESSAGE_2: &str =
            "expected identifier in declaration to be followed by either assignment or semicolon";
        match tokens.next() {
            Some(Token::Operator(Operator::Assignment)) => {
                let expr = Expression::parse(tokens)?;
                remove_semicolon(tokens)?;
                Ok(Self {
                    name,
                    init: Some(expr),
                })
            }
            Some(Token::Semicolon) => Ok(Self { name, init: None }),

            Some(token) => Err(anyhow!("{MESSAGE_2}, but found {token:?}")),
            None => Err(anyhow!("{MESSAGE_2}, but no token was found")),
        }
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

fn remove_semicolon(tokens: &mut TokenIter) -> Result<(), anyhow::Error> {
    match tokens.next() {
        Some(Token::Semicolon) => Ok(()),
        Some(t) => Err(anyhow!("expected semicolon (';'), found {t:?}")),
        None => Err(anyhow!("expected semicolon (';'), but no token was found")),
    }
}

impl Statement {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        match tokens.peek() {
            Some(Token::Keyword(Keyword::Return)) => {
                let _ = tokens.next().expect("token must be return keyword");
                let expr = Expression::parse(tokens)?;
                remove_semicolon(tokens)?;
                Ok(Self::Return(expr))
            }

            Some(Token::Semicolon) => {
                let _ = tokens.next().expect("token must be semicolon");
                Ok(Self::Null)
            }

            Some(_) => {
                let expr = Expression::parse(tokens)?;
                remove_semicolon(tokens)
                    .with_context(|| format!("following expression {expr:?}"))?;
                Ok(Self::Expression(expr))
            }

            None => Err(anyhow!("no token found for statement")),
        }
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        match self {
            Self::Return(expr) => {
                let value = expr.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::Return(value));
            }
            Self::Expression(expr) => todo!(),
            Self::Null => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Constant(i64),
    Var(Identifier),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
}

impl Expression {
    fn parse(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        Self::parse_expr(tokens, 0)
    }

    fn parse_expr(tokens: &mut TokenIter, min_prec: u32) -> Result<Self, anyhow::Error> {
        let mut left = Self::parse_factor(tokens)?;

        while let Some(bin_op) = tokens.peek().and_then(BinaryOperator::parse) {
            let precedence = bin_op.precedence();

            // Check precedence of operator
            if precedence < min_prec {
                break;
            }

            // Then remember to progress the iterator
            let _ = tokens.next();

            match bin_op {
                BinaryOperator::Assignment => {
                    let right = Self::parse_expr(tokens, precedence)?;
                    left = Expression::Assignment(Box::new(left), Box::new(right));
                }
                _ => {
                    let right = Self::parse_expr(tokens, precedence + 1)?;
                    left = Expression::Binary(bin_op, Box::new(left), Box::new(right));
                }
            }
        }

        Ok(left)
    }

    fn parse_factor(tokens: &mut TokenIter) -> Result<Self, anyhow::Error> {
        let Some(next_token) = tokens.next() else {
            return Err(anyhow!("no token found for expression"));
        };

        match next_token {
            Token::Constant(i) => Ok(Expression::Constant(i)),

            Token::Identifier(identifier) => Ok(Expression::Var(Identifier(identifier))),

            // --- Parse unary operators ---
            Token::Operator(Operator::Minus) => Ok(Self::Unary(
                UnaryOperator::Negate,
                Box::new(Self::parse_factor(tokens)?),
            )),
            Token::Operator(Operator::Tilde) => Ok(Self::Unary(
                UnaryOperator::Complement,
                Box::new(Self::parse_factor(tokens)?),
            )),
            Token::Operator(Operator::Not) => Ok(Self::Unary(
                UnaryOperator::Not,
                Box::new(Self::parse_factor(tokens)?),
            )),

            Token::OpenParenthesis => {
                let expr = Self::parse_expr(tokens, 0)?;

                match tokens.next() {
                    Some(Token::CloseParenthesis) => Ok(expr),
                    Some(t) => Err(anyhow!("expected close parenthesis, found {t:?}")),
                    None => Err(anyhow!("expected close parenthesis, found nothing")),
                }
            }

            t => Err(anyhow!("unknown token found for expression: {t:?}")),
        }
    }

    fn emit_tacky(&self, instructions: &mut Vec<crate::tacky::Instruction>) -> crate::tacky::Value {
        match self {
            Self::Constant(c) => crate::tacky::Value::Constant(*c),

            Self::Var(ident) => todo!(),

            Self::Unary(unary_op, expr) => {
                let src = expr.emit_tacky(instructions);
                let dst = crate::tacky::Variable(Identifier::new_temp());

                instructions.push(crate::tacky::Instruction::Unary {
                    op: *unary_op,
                    src,
                    dst: dst.clone(),
                });

                crate::tacky::Value::Variable(dst)
            }

            Self::Binary(BinaryOperator::And, expr_1, expr_2) => {
                let false_label = Identifier::new_label("false_label");
                let end = Identifier::new_label("end");

                let val_1 = expr_1.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfZero {
                    condition: val_1,
                    target: false_label.clone(),
                });

                let val_2 = expr_2.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfZero {
                    condition: val_2,
                    target: false_label.clone(),
                });
                let dst = crate::tacky::Variable(Identifier::new_temp());

                instructions.extend_from_slice(&[
                    crate::tacky::Instruction::Copy {
                        src: crate::tacky::Value::Constant(1),
                        dst: dst.clone(),
                    },
                    crate::tacky::Instruction::Jump {
                        target: end.clone(),
                    },
                    crate::tacky::Instruction::Label(false_label),
                    crate::tacky::Instruction::Copy {
                        src: crate::tacky::Value::Constant(0),
                        dst: dst.clone(),
                    },
                    crate::tacky::Instruction::Label(end),
                ]);

                crate::tacky::Value::Variable(dst)
            }

            Self::Binary(BinaryOperator::Or, expr_1, expr_2) => {
                let true_label = Identifier::new_label("true_label");
                let end = Identifier::new_label("end");

                let val_1 = expr_1.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfNotZero {
                    condition: val_1,
                    target: true_label.clone(),
                });

                let val_2 = expr_2.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfNotZero {
                    condition: val_2,
                    target: true_label.clone(),
                });

                let dst = crate::tacky::Variable(Identifier::new_temp());

                instructions.extend_from_slice(&[
                    crate::tacky::Instruction::Copy {
                        src: crate::tacky::Value::Constant(0),
                        dst: dst.clone(),
                    },
                    crate::tacky::Instruction::Jump {
                        target: end.clone(),
                    },
                    crate::tacky::Instruction::Label(true_label),
                    crate::tacky::Instruction::Copy {
                        src: crate::tacky::Value::Constant(1),
                        dst: dst.clone(),
                    },
                    crate::tacky::Instruction::Label(end),
                ]);

                crate::tacky::Value::Variable(dst)
            }

            Self::Binary(binary_op, expr_1, expr_2) => {
                let val_1 = expr_1.emit_tacky(instructions);
                let val_2 = expr_2.emit_tacky(instructions);
                let dst = crate::tacky::Variable(Identifier::new_temp());

                instructions.push(crate::tacky::Instruction::Binary {
                    op: *binary_op,
                    src_1: val_1,
                    src_2: val_2,
                    dst: dst.clone(),
                });

                crate::tacky::Value::Variable(dst)
            }

            Self::Assignment(expr_1, expr_2) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Special
    Assignment,

    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,

    // Logical
    And,
    Or,

    // Equality
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

impl BinaryOperator {
    fn parse(token: &Token) -> Option<Self> {
        match token {
            // Special
            Token::Operator(Operator::Assignment) => Some(BinaryOperator::Assignment),

            // Arithmetic
            Token::Operator(Operator::Plus) => Some(BinaryOperator::Add),
            Token::Operator(Operator::Minus) => Some(BinaryOperator::Subtract),
            Token::Operator(Operator::Asterisk) => Some(BinaryOperator::Multiply),
            Token::Operator(Operator::Slash) => Some(BinaryOperator::Divide),
            Token::Operator(Operator::Percent) => Some(BinaryOperator::Remainder),

            // Logical
            Token::Operator(Operator::And) => Some(BinaryOperator::And),
            Token::Operator(Operator::Or) => Some(BinaryOperator::Or),

            // Equality
            Token::Operator(Operator::Equal) => Some(BinaryOperator::Equal),
            Token::Operator(Operator::NotEqual) => Some(BinaryOperator::NotEqual),
            Token::Operator(Operator::LessThan) => Some(BinaryOperator::LessThan),
            Token::Operator(Operator::LessOrEqual) => Some(BinaryOperator::LessOrEqual),
            Token::Operator(Operator::GreaterThan) => Some(BinaryOperator::GreaterThan),
            Token::Operator(Operator::GreaterOrEqual) => Some(BinaryOperator::GreaterOrEqual),

            _ => return None,
        }
    }

    fn precedence(self) -> u32 {
        match self {
            Self::Multiply | Self::Divide | Self::Remainder => 50,
            Self::Add | Self::Subtract => 45,
            Self::LessThan | Self::LessOrEqual | Self::GreaterThan | Self::GreaterOrEqual => 35,
            Self::Equal | Self::NotEqual => 30,
            Self::And => 10,
            Self::Or => 5,
            Self::Assignment => 1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_operators() {
        let tokens = vec![
            Token::Constant(3),
            Token::Operator(Operator::Minus),
            Token::Constant(2),
        ];
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens).unwrap();

        assert_eq!(
            parsed,
            Expression::Binary(
                BinaryOperator::Subtract,
                Box::new(Expression::Constant(3)),
                Box::new(Expression::Constant(2)),
            )
        );
    }

    #[test]
    fn operator_precedence() {
        let tokens = vec![
            Token::Constant(1),
            Token::Operator(Operator::Plus),
            Token::Constant(2),
            Token::Operator(Operator::Asterisk),
            Token::Constant(3),
        ];
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens).unwrap();

        assert_eq!(
            parsed,
            Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(1)),
                Box::new(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Constant(2)),
                    Box::new(Expression::Constant(3))
                )),
            )
        );
    }

    #[test]
    fn left_ordering() {
        let tokens = vec![
            Token::Constant(4),
            Token::Operator(Operator::Asterisk),
            Token::Constant(3),
            Token::Operator(Operator::Slash),
            Token::Constant(2),
        ];
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens).unwrap();

        assert_eq!(
            parsed,
            Expression::Binary(
                BinaryOperator::Divide,
                Box::new(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Constant(4)),
                    Box::new(Expression::Constant(3))
                )),
                Box::new(Expression::Constant(2)),
            )
        );
    }

    #[test]
    fn unary_and_binary_operator() {
        let tokens = vec![
            Token::Operator(Operator::Minus),
            Token::Constant(4),
            Token::Operator(Operator::Asterisk),
            Token::Constant(3),
        ];
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens).unwrap();

        assert_eq!(
            parsed,
            Expression::Binary(
                BinaryOperator::Multiply,
                Box::new(Expression::Unary(
                    UnaryOperator::Negate,
                    Box::new(Expression::Constant(4)),
                )),
                Box::new(Expression::Constant(3)),
            )
        );
    }

    #[test]
    fn operator_precedence_with_parsing() {
        let expr = "10 + (8 - 4) * 3";
        let tokens = crate::lexer::run(expr).unwrap();
        assert_eq!(
            tokens,
            [
                Token::Constant(10),
                Token::Operator(Operator::Plus),
                Token::OpenParenthesis,
                Token::Constant(8),
                Token::Operator(Operator::Minus),
                Token::Constant(4),
                Token::CloseParenthesis,
                Token::Operator(Operator::Asterisk),
                Token::Constant(3),
            ]
        );

        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens).unwrap();

        assert_eq!(
            parsed,
            Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(10)),
                Box::new(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Binary(
                        BinaryOperator::Subtract,
                        Box::new(Expression::Constant(8)),
                        Box::new(Expression::Constant(4)),
                    )),
                    Box::new(Expression::Constant(3)),
                ))
            ),
        );
    }

    #[test]
    fn operator_precedence_with_parsing_and_negation() {
        let expr = "10 + (8 - 4) * (-3)";
        let tokens = crate::lexer::run(expr).unwrap();
        assert_eq!(
            tokens,
            [
                Token::Constant(10),
                Token::Operator(Operator::Plus),
                Token::OpenParenthesis,
                Token::Constant(8),
                Token::Operator(Operator::Minus),
                Token::Constant(4),
                Token::CloseParenthesis,
                Token::Operator(Operator::Asterisk),
                Token::OpenParenthesis,
                Token::Operator(Operator::Minus),
                Token::Constant(3),
                Token::CloseParenthesis,
            ]
        );

        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens).unwrap();

        assert_eq!(
            parsed,
            Expression::Binary(
                BinaryOperator::Add,
                Box::new(Expression::Constant(10)),
                Box::new(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Binary(
                        BinaryOperator::Subtract,
                        Box::new(Expression::Constant(8)),
                        Box::new(Expression::Constant(4)),
                    )),
                    Box::new(Expression::Unary(
                        UnaryOperator::Negate,
                        Box::new(Expression::Constant(3)),
                    )),
                ))
            ),
        );
    }
}
