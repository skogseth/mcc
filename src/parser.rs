use std::iter::Peekable;
use std::vec::IntoIter;

use crate::lexer::{Keyword, Operator, Token, TokenElem};
use crate::{Identifier, Output, Span};

pub fn parse(tokens: Vec<TokenElem>, output: &Output) -> Result<Program, ParseError> {
    let mut tokens = tokens.into_iter().peekable();

    let main = Function::parse(&mut tokens, output)?;
    assert_eq!(main.name.0.as_str(), "main");

    let remaining_tokens: Vec<TokenElem> = tokens.collect();
    if !remaining_tokens.is_empty() {
        panic!("Unexpected token(s) found after main: {remaining_tokens:?}");
    }

    Ok(Program(main))
}

pub type TokenIter<'a> = Peekable<IntoIter<TokenElem>>;

#[derive(Debug)]
pub enum ParseError {
    BadTokens,
    EarlyEnd(&'static str),
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
    pub name: Identifier,
    pub body: Vec<BlockItem>,
}

impl Function {
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        let TokenElem { token, span } = tokens.next().ok_or(ParseError::EarlyEnd("return type"))?;
        let Token::Keyword(Keyword::Int) = token else {
            output.error(span, format!("expected return type, found {token}"));
            return Err(ParseError::BadTokens);
        };

        let TokenElem { token, span } = tokens
            .next()
            .ok_or(ParseError::EarlyEnd("function identifier"))?;
        let Token::Identifier(name) = token else {
            output.error(span, format!("expected function identifier, found {token}"));
            return Err(ParseError::BadTokens);
        };

        let TokenElem { token, span } = tokens
            .next()
            .ok_or(ParseError::EarlyEnd("open-parenthesis"))?;
        let Token::OpenParenthesis = token else {
            output.error(
                span,
                format!("expected open-parenthesis for function {name}, found {token}"),
            );
            return Err(ParseError::BadTokens);
        };

        assert!(matches!(
            tokens.next(),
            Some(TokenElem {
                token: Token::Keyword(Keyword::Void),
                ..
            })
        ));

        let TokenElem { token, span } = tokens
            .next()
            .ok_or(ParseError::EarlyEnd("close-parenthesis"))?;
        let Token::CloseParenthesis = token else {
            output.error(
                span,
                format!("expected close-parenthesis for function {name}, found {token}"),
            );
            return Err(ParseError::BadTokens);
        };

        let TokenElem { token, span } = tokens.next().ok_or(ParseError::EarlyEnd("open brace"))?;
        let Token::OpenBrace = token else {
            output.error(
                span,
                format!("expected open brace for function {name}, found {token}"),
            );
            return Err(ParseError::BadTokens);
        };

        let mut body = Vec::new();
        let mut encountered_bad_tokens = false;
        loop {
            let token_elem = tokens.peek().ok_or(ParseError::EarlyEnd("close brace"))?;
            match token_elem.token {
                Token::CloseBrace => break,
                _ => match BlockItem::parse(tokens, output) {
                    Ok(block_item) => body.push(block_item),
                    Err(ParseError::BadTokens) => encountered_bad_tokens = true,
                    Err(e @ ParseError::EarlyEnd(_)) => return Err(e),
                },
            }
        }

        if encountered_bad_tokens {
            return Err(ParseError::BadTokens);
        }

        let TokenElem { token, .. } = tokens.next().expect("close brace");
        assert!(matches!(token, Token::CloseBrace));

        Ok(Function { name, body })
    }

    fn emit_tacky(self) -> crate::tacky::Function {
        let mut instructions = Vec::new();
        for block_item in self.body {
            block_item.emit_tacky(&mut instructions);
        }

        // Sometimes C is a very strange language...
        // A function is allowed to not return a value
        // If the function is `main`, this should implicitly return `0`
        // If the function is _not_ `main` then it is only undefined
        // behavior if another function tries to use the value that was
        // returned, otherwise it's completely fine. The simplest way
        // to satisfy this strange set of requirements is to always append
        // an addition `return 0` to any function.
        let block_item = BlockItem::S(Statement::Return(Expression::Constant(0)));
        block_item.emit_tacky(&mut instructions);

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
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        let token_elem = tokens.peek().ok_or(ParseError::EarlyEnd("block item"))?;
        match token_elem.token {
            Token::Keyword(Keyword::Int) => Declaration::parse(tokens, output).map(BlockItem::D),
            _ => Statement::parse(tokens, output).map(BlockItem::S),
        }
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        match self {
            Self::S(statement) => statement.emit_tacky(instructions),
            Self::D(decleration) => decleration.emit_tacky(instructions),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Identifier,
    pub name_span: Span,
    pub init: Option<Expression>,
}

impl Declaration {
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        let _typ = tokens.next().expect("can't parse decleration without type");

        let name_elem = tokens
            .next()
            .ok_or(ParseError::EarlyEnd("identifier in declaration"))?;
        let Token::Identifier(name) = name_elem.token else {
            output.error(
                name_elem.span,
                format!("expected type to be followed by identifier in declaration"),
            );
            return Err(ParseError::BadTokens);
        };

        let init = {
            let token_elem = tokens
                .next()
                .ok_or(ParseError::EarlyEnd("either assignment or semicolon"))?;
            match token_elem.token {
                Token::Operator(Operator::Assignment) => Some(take_expr(tokens, output)?),
                Token::Semicolon => None,
                _ => {
                    output.error(
                        token_elem.span,
                        String::from("expected identifier in declaration to be followed by either assignment or semicolon"),
                    );
                    return Err(ParseError::BadTokens);
                }
            }
        };

        Ok(Self {
            name,
            name_span: name_elem.span,
            init,
        })
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        // We only need to emit tacky in the case where we have an init expression
        if let Some(init) = &self.init {
            init.emit_tacky(instructions);
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    Null,
}

fn take_expr(tokens: &mut TokenIter, output: &Output) -> Result<Expression, ParseError> {
    match Expression::parse(tokens, output) {
        Ok(expr) => {
            let next = tokens.next().ok_or(ParseError::EarlyEnd("statement"))?;
            match next.token {
                Token::Semicolon => return Ok(expr),
                _ => output.error(next.span, String::from("expected semicolon")),
            }
        }
        Err(ParseError::BadTokens) => {}
        Err(e @ ParseError::EarlyEnd(_)) => return Err(e),
    };

    // We know keep taking tokens until we find a semicolon
    let mut span = None;
    loop {
        let elem = tokens.next().ok_or(ParseError::EarlyEnd("statement"))?;

        if matches!(elem.token, Token::Semicolon) {
            if let Some(current_span) = span {
                output.warning(current_span, String::from("not parsed"));
            }
            break;
        }

        let current_span: &mut Span = span.get_or_insert(elem.span);
        if elem.span.line != current_span.line {
            output.warning(*current_span, String::from("not parsed"));
            *current_span = elem.span;
        } else {
            current_span.end_position = elem.span.end_position;
        }
    }

    Err(ParseError::BadTokens)
}

impl Statement {
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        let peeked = tokens.peek().ok_or(ParseError::EarlyEnd("statement"))?;
        match peeked.token {
            Token::Keyword(Keyword::Return) => {
                let _ = tokens.next().expect("token must be return keyword");
                let expr = take_expr(tokens, output)?;
                Ok(Self::Return(expr))
            }

            Token::Semicolon => {
                let _ = tokens.next().expect("token must be semicolon");
                Ok(Self::Null)
            }

            _ => {
                let expr = take_expr(tokens, output)?;
                Ok(Self::Expression(expr))
            }
        }
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        match self {
            Self::Return(expr) => {
                let value = expr.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::Return(value));
            }
            Self::Expression(expr) => {
                // Don't do anything with the returned temporary
                let _ = expr.emit_tacky(instructions);
            }
            Self::Null => {} // nothing to do
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    pub name: Identifier,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Constant(i64),
    Var(Variable),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
}

impl Expression {
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        Self::parse_expr(tokens, 0, output)
    }

    fn parse_expr(
        tokens: &mut TokenIter,
        min_prec: u32,
        output: &Output,
    ) -> Result<Self, ParseError> {
        let mut left = Self::parse_factor(tokens, output)?;

        while let Some(bin_op) = tokens.peek().and_then(|e| BinaryOperator::parse(&e.token)) {
            let precedence = bin_op.precedence();

            // Check precedence of operator
            if precedence < min_prec {
                break;
            }

            // Then remember to progress the iterator
            let _ = tokens.next();

            match bin_op {
                BinaryOperator::Assignment => {
                    let right = Self::parse_expr(tokens, precedence, output)?;
                    left = Expression::Assignment(Box::new(left), Box::new(right));
                }
                _ => {
                    let right = Self::parse_expr(tokens, precedence + 1, output)?;
                    left = Expression::Binary(bin_op, Box::new(left), Box::new(right));
                }
            }
        }

        Ok(left)
    }

    fn parse_factor(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        let token_elem = tokens
            .next()
            .ok_or(ParseError::EarlyEnd("no token found for expression"))?;

        match token_elem.token {
            Token::Constant(i) => Ok(Expression::Constant(i)),

            Token::Identifier(identifier) => Ok(Expression::Var(Variable {
                name: identifier,
                span: token_elem.span,
            })),

            // --- Parse unary operators ---
            Token::Operator(Operator::Minus) => Ok(Self::Unary(
                UnaryOperator::Negate,
                Box::new(Self::parse_factor(tokens, output)?),
            )),
            Token::Operator(Operator::Tilde) => Ok(Self::Unary(
                UnaryOperator::Complement,
                Box::new(Self::parse_factor(tokens, output)?),
            )),
            Token::Operator(Operator::Not) => Ok(Self::Unary(
                UnaryOperator::Not,
                Box::new(Self::parse_factor(tokens, output)?),
            )),

            Token::OpenParenthesis => {
                let expr = Self::parse_expr(tokens, 0, output)?;

                let next_token = tokens
                    .next()
                    .ok_or(ParseError::EarlyEnd("close parenthesis"))?;
                match next_token.token {
                    Token::CloseParenthesis => Ok(expr),
                    t => {
                        output.error(
                            next_token.span,
                            format!("expected close parenthesis, found {t}"),
                        );
                        Err(ParseError::BadTokens)
                    }
                }
            }

            _ => {
                output.error(
                    token_elem.span,
                    format!("unexpected token found for expression"),
                );
                Err(ParseError::BadTokens)
            }
        }
    }

    fn emit_tacky(&self, instructions: &mut Vec<crate::tacky::Instruction>) -> crate::tacky::Value {
        match self {
            Self::Constant(c) => crate::tacky::Value::Constant(*c),

            Self::Var(Variable { name, .. }) => {
                return crate::tacky::Value::Variable(crate::tacky::Variable(name.clone()));
            }

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

            Self::Assignment(expr_1, expr_2) => {
                let Expression::Var(var) = &**expr_1 else {
                    panic!("unexpected expression found in assignment");
                };
                let var = crate::tacky::Variable(var.name.clone());

                let result = expr_2.emit_tacky(instructions);

                instructions.push(crate::tacky::Instruction::Copy {
                    src: result,
                    dst: var.clone(),
                });

                return crate::tacky::Value::Variable(var);
            }
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
    use std::sync::LazyLock;

    use crate::Span;

    use super::*;

    static DUMMY_OUTPUT: LazyLock<Output<'static>> = LazyLock::new(Output::dummy);

    #[test]
    fn binary_operators() {
        let just_tokens = vec![
            Token::Constant(3),
            Token::Operator(Operator::Minus),
            Token::Constant(2),
        ];
        let tokens: Vec<_> = just_tokens
            .into_iter()
            .map(|token| TokenElem {
                token,
                span: Span::dummy(),
            })
            .collect();
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

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
        let just_tokens = vec![
            Token::Constant(1),
            Token::Operator(Operator::Plus),
            Token::Constant(2),
            Token::Operator(Operator::Asterisk),
            Token::Constant(3),
        ];
        let tokens: Vec<_> = just_tokens
            .into_iter()
            .map(|token| TokenElem {
                token,
                span: Span::dummy(),
            })
            .collect();
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

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
        let just_tokens = vec![
            Token::Constant(4),
            Token::Operator(Operator::Asterisk),
            Token::Constant(3),
            Token::Operator(Operator::Slash),
            Token::Constant(2),
        ];
        let tokens: Vec<_> = just_tokens
            .into_iter()
            .map(|token| TokenElem {
                token,
                span: Span::dummy(),
            })
            .collect();
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

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
        let just_tokens = vec![
            Token::Operator(Operator::Minus),
            Token::Constant(4),
            Token::Operator(Operator::Asterisk),
            Token::Constant(3),
        ];
        let tokens: Vec<_> = just_tokens
            .into_iter()
            .map(|token| TokenElem {
                token,
                span: Span::dummy(),
            })
            .collect();
        let mut tokens = tokens.into_iter().peekable();

        let parsed = Expression::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

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
        let tokens: Vec<_> = crate::lexer::make_lexer(&[expr])
            .collect::<Result<_, _>>()
            .unwrap();

        let just_tokens = tokens
            .iter()
            .map(|elem| elem.token.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            just_tokens,
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

        let parsed = Expression::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

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
        let tokens: Vec<_> = crate::lexer::make_lexer(&[expr])
            .collect::<Result<_, _>>()
            .unwrap();

        let just_tokens = tokens
            .iter()
            .map(|elem| elem.token.clone())
            .collect::<Vec<_>>();
        assert_eq!(
            just_tokens,
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
        let parsed = Expression::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

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
