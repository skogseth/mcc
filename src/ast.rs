use crate::lexer::{Keyword, Token};

pub fn parse(tokens: Vec<Token>) -> Function {
    let return_type = tokens[0].clone();
    assert!(matches!(return_type, Token::Keyword(Keyword::Int)));

    let function_name = match tokens[1].clone() {
        Token::Identifier(s) => Identifier(s),
        _ => panic!("bad token"),
    };

    assert!(matches!(tokens[2], Token::OpenParenthesis));
    assert!(matches!(tokens[3], Token::Keyword(Keyword::Void)));
    assert!(matches!(tokens[4], Token::CloseParenthesis));
    assert!(matches!(tokens[5], Token::OpenBrace));

    let body_unparsed: Vec<Token> = tokens[6..]
        .iter()
        .take_while(|t| !matches!(t, Token::CloseBrace))
        .cloned()
        .collect();

    let statements: Vec<Statement> = body_unparsed
        .split(|t| matches!(t, Token::Semicolon))
        .map(|tokens| match tokens {
            [Token::Keyword(Keyword::Return), Token::Constant(i)] => {
                Statement::Return(Expression::Constant(*i))
            }
            _ => panic!("bad statement!"),
        })
        .collect();

    assert_eq!(statements.len(), 1);

    Function {
        name: function_name,
        body: statements[0].clone(),
    }
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i64),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug, Clone)]
pub struct Function {
    name: Identifier,
    body: Statement,
}
