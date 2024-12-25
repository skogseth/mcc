use crate::lexer::{Keyword, Token};

pub fn parse(tokens: Vec<Token>) -> Function {
    let return_type = tokens[0].clone();

    let function_name = match tokens[1].clone() {
        Token::Identifier(s) => Identifier(s),
        _ => panic!("bad token"),
    };

    assert!(matches!(tokens[2], Token::OpenParenthesis));
    assert!(matches!(tokens[3], Token::Keyword(Keyword::Void)));
    assert!(matches!(tokens[4], Token::CloseParenthesis));
    assert!(matches!(tokens[5], Token::OpenBrace));

    let statement: Vec<Token> = tokens[6..]
        .iter()
        .take_while(|t| !matches!(t, Token::Semicolon))
        .cloned()
        .collect();

    let statement = match statement[..] {
        [Token::Keyword(Keyword::Return), Token::Constant(i)] => {
            Statement::Return(Expression::Constant(i))
        }
        _ => panic!("bad statement!"),
    };

    Function {
        name: function_name,
        body: statement,
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
