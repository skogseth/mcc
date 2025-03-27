use std::iter::Peekable;
use std::vec::IntoIter;

use crate::lexer::{Keyword, Operator, Punct, Token, TokenElem};
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

fn take_punct(tokens: &mut TokenIter, expected: Punct, output: &Output) -> Result<(), ParseError> {
    let elem = tokens
        .next()
        .ok_or(ParseError::EarlyEnd("close-parenthesis"))?;

    match elem.token {
        Token::Punct(p) if p == expected => Ok(()),
        _ => {
            output.error(elem.span, format!("expected {expected}"));
            Err(ParseError::BadTokens)
        }
    }
}

fn take_operator(
    tokens: &mut TokenIter,
    expected: Operator,
    output: &Output,
) -> Result<(), ParseError> {
    let elem = tokens
        .next()
        .ok_or(ParseError::EarlyEnd("close-parenthesis"))?;

    match elem.token {
        Token::Operator(o) if o == expected => Ok(()),
        _ => {
            output.error(elem.span, format!("expected {expected}"));
            Err(ParseError::BadTokens)
        }
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
    pub name: Identifier,
    pub body: Block,
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

        take_punct(tokens, Punct::OpenParenthesis, output)?;

        assert!(matches!(
            tokens.next(),
            Some(TokenElem {
                token: Token::Keyword(Keyword::Void),
                ..
            })
        ));

        take_punct(tokens, Punct::CloseParenthesis, output)?;

        let body = Block::parse(tokens, output)?;

        Ok(Function { name, body })
    }

    fn emit_tacky(self) -> crate::tacky::Function {
        let mut instructions = Vec::new();
        self.body.emit_tacky(&mut instructions);

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
pub struct Block(pub Vec<BlockItem>);

impl Block {
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        take_punct(tokens, Punct::OpenBrace, output)?;

        let mut block_items = Vec::new();
        let mut encountered_bad_tokens = false;
        loop {
            let token_elem = tokens.peek().ok_or(ParseError::EarlyEnd("close brace"))?;
            match token_elem.token {
                Token::Punct(Punct::CloseBrace) => break,
                _ => match BlockItem::parse(tokens, output) {
                    Ok(block_item) => block_items.push(block_item),
                    Err(ParseError::BadTokens) => encountered_bad_tokens = true,
                    Err(e @ ParseError::EarlyEnd(_)) => return Err(e),
                },
            }
        }

        if encountered_bad_tokens {
            return Err(ParseError::BadTokens);
        }

        take_punct(tokens, Punct::CloseBrace, output)?;

        Ok(Self(block_items))
    }

    fn emit_tacky(self, instructions: &mut Vec<crate::tacky::Instruction>) {
        for block_item in self.0 {
            block_item.emit_tacky(instructions);
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
                "expected type to be followed by identifier in declaration".to_string(),
            );
            return Err(ParseError::BadTokens);
        };

        let init = {
            let token_elem = tokens
                .next()
                .ok_or(ParseError::EarlyEnd("either assignment or semicolon"))?;
            match token_elem.token {
                Token::Operator(Operator::Assignment) => {
                    let expr = take_expr(tokens, output)?;
                    take_punct(tokens, Punct::Semicolon, output)?;
                    Some(expr)
                }
                Token::Punct(Punct::Semicolon) => None,
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
            let result = init.emit_tacky(instructions);
            instructions.push(crate::tacky::Instruction::Copy {
                src: result,
                dst: crate::tacky::Variable(self.name.clone()),
            });
        }
    }
}

#[derive(Debug, Clone)]
pub enum ForInit {
    D(Declaration),
    E(Option<Expression>),
}

impl ForInit {
    fn parse(tokens: &mut TokenIter, output: &Output) -> Result<Self, ParseError> {
        let token_elem = tokens.peek().ok_or(ParseError::EarlyEnd("block item"))?;

        match token_elem.token {
            Token::Keyword(Keyword::Int) => Declaration::parse(tokens, output).map(ForInit::D),
            Token::Punct(Punct::Semicolon) => {
                let _ = tokens.next().expect("token must be semicolon");
                Ok(Self::E(None))
            }
            _ => {
                let expr = take_expr(tokens, output)?;
                take_punct(tokens, Punct::Semicolon, output)?;
                Ok(Self::E(Some(expr)))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If {
        cond: Expression,
        then: Box<Statement>,
        else_: Option<Box<Statement>>,
    },
    Compound(Block),
    Break(Option<Identifier>),
    Continue(Option<Identifier>),
    While {
        cond: Expression,
        body: Box<Statement>,
        label: Identifier,
    },
    DoWhile {
        body: Box<Statement>,
        cond: Expression,
        label: Identifier,
    },
    For {
        init: ForInit,
        cond: Option<Expression>,
        post: Option<Expression>,
        body: Box<Statement>,
        label: Identifier,
    },
    Null,
}

fn take_expr(tokens: &mut TokenIter, output: &Output) -> Result<Expression, ParseError> {
    match Expression::parse(tokens, output) {
        Ok(expr) => return Ok(expr),
        Err(ParseError::BadTokens) => {}
        Err(e @ ParseError::EarlyEnd(_)) => return Err(e),
    }

    // We know keep taking tokens until we find a semicolon
    let mut span = None;
    loop {
        let elem = tokens.next().ok_or(ParseError::EarlyEnd("statement"))?;

        if matches!(elem.token, Token::Punct(Punct::Semicolon)) {
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
                take_punct(tokens, Punct::Semicolon, output)?;
                Ok(Self::Return(expr))
            }

            Token::Keyword(Keyword::Break) => {
                let _ = tokens.next().expect("token must be break keyword");
                take_punct(tokens, Punct::Semicolon, output)?;
                Ok(Self::Break(None))
            }

            Token::Keyword(Keyword::Continue) => {
                let _ = tokens.next().expect("token must be continue keyword");
                take_punct(tokens, Punct::Semicolon, output)?;
                Ok(Self::Continue(None))
            }

            Token::Keyword(Keyword::While) => {
                let _ = tokens.next().expect("token must be while keyword");

                take_punct(tokens, Punct::OpenParenthesis, output)?;
                let cond = take_expr(tokens, output)?;
                take_punct(tokens, Punct::CloseParenthesis, output)?;

                let body = Box::new(Statement::parse(tokens, output)?);

                let label = Identifier::new_loop();

                Ok(Self::While { cond, body, label })
            }

            Token::Keyword(Keyword::Do) => {
                let _ = tokens.next().expect("token must be do keyword");

                let body = Box::new(Statement::parse(tokens, output)?);

                let elem = tokens.next().expect("token must be while keyword");
                if !matches!(elem.token, Token::Keyword(Keyword::While)) {
                    output.error(elem.span, String::from("expected `while`"));
                    return Err(ParseError::BadTokens);
                }

                take_punct(tokens, Punct::OpenParenthesis, output)?;
                let cond = take_expr(tokens, output)?;
                take_punct(tokens, Punct::CloseParenthesis, output)?;

                take_punct(tokens, Punct::Semicolon, output)?;

                let label = Identifier::new_loop();

                Ok(Self::DoWhile { body, cond, label })
            }

            Token::Keyword(Keyword::For) => {
                let _ = tokens.next().expect("token must be for keyword");

                take_punct(tokens, Punct::OpenParenthesis, output)?;

                let init = ForInit::parse(tokens, output)?;

                let peeked = tokens.peek().ok_or(ParseError::EarlyEnd("condition"))?;
                let cond = match peeked.token {
                    Token::Punct(Punct::Semicolon) => None,
                    _ => Some(take_expr(tokens, output)?),
                };

                take_punct(tokens, Punct::Semicolon, output)?;

                let peeked = tokens.peek().ok_or(ParseError::EarlyEnd("post expr"))?;
                let post = match peeked.token {
                    Token::Punct(Punct::CloseParenthesis) => None,
                    _ => Some(Expression::parse(tokens, output)?),
                };

                take_punct(tokens, Punct::CloseParenthesis, output)?;

                let body = Box::new(Statement::parse(tokens, output)?);

                let label = Identifier::new_loop();

                Ok(Self::For {
                    init,
                    cond,
                    post,
                    body,
                    label,
                })
            }

            Token::Punct(Punct::Semicolon) => {
                let _ = tokens.next().expect("token must be semicolon");
                Ok(Self::Null)
            }

            Token::Keyword(Keyword::If) => {
                let _ = tokens.next().expect("token must be return keyword");

                take_punct(tokens, Punct::OpenParenthesis, output)?;
                let cond = take_expr(tokens, output)?;
                take_punct(tokens, Punct::CloseParenthesis, output)?;

                let then = Box::new(Statement::parse(tokens, output)?);

                let else_ = tokens
                    .next_if(|t| matches!(t.token, Token::Keyword(Keyword::Else)))
                    .map(|_t| Statement::parse(tokens, output))
                    .transpose()?
                    .map(Box::new);

                Ok(Self::If { cond, then, else_ })
            }

            Token::Punct(Punct::OpenBrace) => {
                let block = Block::parse(tokens, output)?;
                Ok(Self::Compound(block))
            }

            _ => {
                let expr = take_expr(tokens, output)?;
                take_punct(tokens, Punct::Semicolon, output)?;
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
            Self::If { cond, then, else_ } => {
                let end_label = Identifier::new_label("end");
                let else_label = Identifier::new_label("else");

                let c = cond.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfZero {
                    condition: c,
                    target: else_label.clone(),
                });

                then.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::Jump {
                    target: end_label.clone(),
                });

                instructions.push(crate::tacky::Instruction::Label(else_label));

                if let Some(expr) = else_ {
                    expr.emit_tacky(instructions);
                }

                instructions.push(crate::tacky::Instruction::Label(end_label));
            }
            Self::Compound(block) => {
                for block_item in block.0 {
                    block_item.emit_tacky(instructions);
                }
            }
            Self::Break(label) => {
                let target = label.unwrap().with_prefix("break_");
                instructions.push(crate::tacky::Instruction::Jump { target });
            }
            Self::Continue(label) => {
                let target = label.unwrap().with_prefix("continue_");
                instructions.push(crate::tacky::Instruction::Jump { target });
            }
            Self::While { cond, body, label } => {
                let continue_ = label.clone().with_prefix("continue_");
                let break_ = label.with_prefix("break_");

                instructions.push(crate::tacky::Instruction::Label(continue_.clone()));

                let condition = cond.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfZero {
                    condition,
                    target: break_.clone(),
                });

                body.emit_tacky(instructions);

                instructions.push(crate::tacky::Instruction::Jump { target: continue_ });
                instructions.push(crate::tacky::Instruction::Label(break_));
            }

            Self::DoWhile { body, cond, label } => {
                let start = Identifier::new_label("start");
                instructions.push(crate::tacky::Instruction::Label(start.clone()));

                body.emit_tacky(instructions);

                let continue_ = label.clone().with_prefix("continue_");
                instructions.push(crate::tacky::Instruction::Label(continue_));

                let condition = cond.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfNotZero {
                    condition,
                    target: start,
                });

                let break_ = label.with_prefix("break_");
                instructions.push(crate::tacky::Instruction::Label(break_));
            }

            Self::For {
                init,
                cond,
                post,
                body,
                label,
            } => {
                let start = Identifier::new_label("start");
                let continue_ = label.clone().with_prefix("continue_");
                let break_ = label.with_prefix("break_");

                match init {
                    ForInit::D(decleration) => decleration.emit_tacky(instructions),
                    ForInit::E(maybe_expr) => {
                        if let Some(expr) = maybe_expr {
                            expr.emit_tacky(instructions);
                        }
                    }
                }

                instructions.push(crate::tacky::Instruction::Label(start.clone()));

                if let Some(cond) = cond {
                    let condition = cond.emit_tacky(instructions);
                    instructions.push(crate::tacky::Instruction::JumpIfZero {
                        condition,
                        target: break_.clone(),
                    });
                }

                body.emit_tacky(instructions);

                instructions.push(crate::tacky::Instruction::Label(continue_.clone()));

                if let Some(post) = post {
                    post.emit_tacky(instructions);
                }

                instructions.push(crate::tacky::Instruction::Jump { target: start });
                instructions.push(crate::tacky::Instruction::Label(break_));
            }
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
    Conditional {
        cond: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
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
                BinaryOperator::QuestionMark => {
                    let middle = Self::parse_expr(tokens, 0, output)?;
                    take_operator(tokens, Operator::Colon, output)?;
                    let right = Self::parse_expr(tokens, precedence, output)?;
                    left = Self::Conditional {
                        cond: Box::new(left),
                        if_true: Box::new(middle),
                        if_false: Box::new(right),
                    };
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

            Token::Punct(Punct::OpenParenthesis) => {
                let expr = Self::parse_expr(tokens, 0, output)?;
                take_punct(tokens, Punct::CloseParenthesis, output)?;
                Ok(expr)
            }

            _ => {
                output.error(
                    token_elem.span,
                    "unexpected token found for expression".to_string(),
                );
                Err(ParseError::BadTokens)
            }
        }
    }

    fn emit_tacky(&self, instructions: &mut Vec<crate::tacky::Instruction>) -> crate::tacky::Value {
        match self {
            Self::Constant(c) => crate::tacky::Value::Constant(*c),

            Self::Var(Variable { name, .. }) => {
                crate::tacky::Value::Variable(crate::tacky::Variable(name.clone()))
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

                crate::tacky::Value::Variable(var)
            }

            Self::Conditional {
                cond,
                if_true,
                if_false,
            } => {
                let return_var = crate::tacky::Variable(Identifier::new_temp());
                let end_label = Identifier::new_label("end");
                let expr2_label = Identifier::new_label("expr2");

                let c = cond.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::JumpIfZero {
                    condition: c,
                    target: expr2_label.clone(),
                });

                let v1 = if_true.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::Copy {
                    src: v1,
                    dst: return_var.clone(),
                });
                instructions.push(crate::tacky::Instruction::Jump {
                    target: end_label.clone(),
                });

                instructions.push(crate::tacky::Instruction::Label(expr2_label));

                let v2 = if_false.emit_tacky(instructions);
                instructions.push(crate::tacky::Instruction::Copy {
                    src: v2,
                    dst: return_var.clone(),
                });

                instructions.push(crate::tacky::Instruction::Label(end_label));

                crate::tacky::Value::Variable(return_var)
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

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Complement => f.write_str("~"),
            Self::Negate => f.write_str("-"),
            Self::Not => f.write_str("!"),
        }
    }
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

    // Conditional
    QuestionMark,
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

            // Conditional
            Token::Operator(Operator::QuestionMark) => Some(BinaryOperator::QuestionMark),

            _ => None,
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
            Self::QuestionMark => 3,
            Self::Assignment => 1,
        }
    }
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assignment => f.write_str("="),
            Self::Add => f.write_str("+"),
            Self::Subtract => f.write_str("-"),
            Self::Multiply => f.write_str("*"),
            Self::Divide => f.write_str("/"),
            Self::Remainder => f.write_str("%"),
            Self::And => f.write_str("&&"),
            Self::Or => f.write_str("||"),
            Self::Equal => f.write_str("=="),
            Self::NotEqual => f.write_str("!="),
            Self::LessThan => f.write_str("<"),
            Self::LessOrEqual => f.write_str("<="),
            Self::GreaterThan => f.write_str(">"),
            Self::GreaterOrEqual => f.write_str(">="),
            Self::QuestionMark => f.write_str("?"),
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
                Token::Punct(Punct::OpenParenthesis),
                Token::Constant(8),
                Token::Operator(Operator::Minus),
                Token::Constant(4),
                Token::Punct(Punct::CloseParenthesis),
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
                Token::Punct(Punct::OpenParenthesis),
                Token::Constant(8),
                Token::Operator(Operator::Minus),
                Token::Constant(4),
                Token::Punct(Punct::CloseParenthesis),
                Token::Operator(Operator::Asterisk),
                Token::Punct(Punct::OpenParenthesis),
                Token::Operator(Operator::Minus),
                Token::Constant(3),
                Token::Punct(Punct::CloseParenthesis),
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

    #[test]
    fn conditional() {
        let expr = "int a = 0 ? 1 - 3 : 4 * (-1);";
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
                Token::Keyword(Keyword::Int),
                Token::Identifier(Identifier(String::from("a"))),
                Token::Operator(Operator::Assignment),
                Token::Constant(0),
                Token::Operator(Operator::QuestionMark),
                Token::Constant(1),
                Token::Operator(Operator::Minus),
                Token::Constant(3),
                Token::Operator(Operator::Colon),
                Token::Constant(4),
                Token::Operator(Operator::Asterisk),
                Token::Punct(Punct::OpenParenthesis),
                Token::Operator(Operator::Minus),
                Token::Constant(1),
                Token::Punct(Punct::CloseParenthesis),
                Token::Punct(Punct::Semicolon),
            ]
        );

        let mut tokens = tokens.into_iter().peekable();
        let parsed = Declaration::parse(&mut tokens, &DUMMY_OUTPUT).unwrap();

        assert_eq!(parsed.name, Identifier(String::from("a")));
        assert_eq!(
            parsed.init,
            Some(Expression::Conditional {
                cond: Box::new(Expression::Constant(0)),
                if_true: Box::new(Expression::Binary(
                    BinaryOperator::Subtract,
                    Box::new(Expression::Constant(1)),
                    Box::new(Expression::Constant(3)),
                )),
                if_false: Box::new(Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Expression::Constant(4)),
                    Box::new(Expression::Unary(
                        UnaryOperator::Negate,
                        Box::new(Expression::Constant(1))
                    )),
                )),
            })
        );
    }
}
