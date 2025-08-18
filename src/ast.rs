use crate::{Identifier, Span};

#[derive(Debug, Clone)]
pub struct Program(pub Function);

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<BlockItem>);

#[derive(Debug, Clone)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: Identifier,
    pub name_span: Span,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum ForInit {
    D(Declaration),
    E(Option<Expression>),
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
