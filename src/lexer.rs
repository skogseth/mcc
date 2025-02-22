use std::str::FromStr;

use crate::Span;

pub fn run(lines: &[&str]) -> Result<Vec<TokenElem>, LexerError> {
    let mut iter = lines
        .iter()
        .copied()
        .enumerate()
        .flat_map(|(line_num, line)| {
            line.chars()
                .chain(std::iter::once('\n'))
                .enumerate()
                .map(move |(char_pos, char)| CharElem {
                    char,
                    line: line_num,
                    position: char_pos,
                })
        })
        .peekable();

    let mut tokens: Vec<TokenElem> = Vec::new();
    while let Some(c) = iter.next() {
        if c.char.is_whitespace() {
            continue;
        }

        // If we encounter any "normal characters" then we have found an identifier
        let token_elem = if c.char.is_alphabetic() || c.char == '_' {
            let mut extracted = String::from(c.char);
            let mut span = Span {
                line: c.line,
                start_position: c.position,
                end_position: c.position,
            };

            // Important that we allow numbers here, but not in the initial position ^
            while let Some(c) = iter.next_if(|c| c.char.is_alphanumeric() || c.char == '_') {
                extracted.push(c.char);
                span.end_position = c.position;
            }

            let token = match extracted.parse::<Keyword>() {
                Ok(keyword) => Token::Keyword(keyword),
                Err(()) => Token::Identifier(extracted),
            };

            TokenElem { token, span }
        } else if c.char.is_numeric() {
            let mut extracted = String::from(c.char);
            let mut span = Span {
                line: c.line,
                start_position: c.position,
                end_position: c.position,
            };

            while let Some(c) = iter.next_if(|c| c.char.is_alphanumeric()) {
                if c.char.is_numeric() {
                    extracted.push(c.char);
                    span.end_position = c.position;
                } else {
                    return Err(LexerError {
                        message: "encountered alphabetic character as part of a number",
                        char_elem: c,
                    });
                }
            }

            let token = Token::Constant(extracted.parse().unwrap());
            TokenElem { token, span }
        } else {
            let token = match c.char {
                '(' => Token::OpenParenthesis,
                ')' => Token::CloseParenthesis,
                '{' => Token::OpenBrace,
                '}' => Token::CloseBrace,
                ';' => Token::Semicolon,

                // This can be either plus ('+') or increment ('++')
                '+' => match iter.next_if(|c| c.char == '+') {
                    Some(_) => Token::Operator(Operator::Increment),
                    None => Token::Operator(Operator::Plus),
                },

                // This can be either minus ('-') or decrement ('--')
                '-' => match iter.next_if(|c| c.char == '-') {
                    Some(_) => Token::Operator(Operator::Decrement),
                    None => Token::Operator(Operator::Minus),
                },

                '*' => Token::Operator(Operator::Asterisk),
                '/' => Token::Operator(Operator::Slash),
                '%' => Token::Operator(Operator::Percent),
                '~' => Token::Operator(Operator::Tilde),

                // Could be either "not" ('!') or "not equal" ('!=')
                '!' => match iter.next_if(|c| c.char == '=') {
                    Some(_) => Token::Operator(Operator::NotEqual),
                    None => Token::Operator(Operator::Not),
                },

                '&' => match iter.next_if(|c| c.char == '&') {
                    Some(_) => Token::Operator(Operator::And),
                    None => {
                        return Err(LexerError {
                            message: "bitwise and ('&') is not supported",
                            char_elem: c,
                        })
                    }
                },
                '|' => match iter.next_if(|c| c.char == '|') {
                    Some(_) => Token::Operator(Operator::Or),
                    None => {
                        return Err(LexerError {
                            message: "bitwise or ('|') is not supported",
                            char_elem: c,
                        })
                    }
                },

                '=' => match iter.next_if(|c| c.char == '=') {
                    Some(_) => Token::Operator(Operator::Equal),
                    None => Token::Operator(Operator::Assignment),
                },

                '<' => match iter.next_if(|c| c.char == '=') {
                    Some(_) => Token::Operator(Operator::LessOrEqual),
                    None => Token::Operator(Operator::LessThan),
                },
                '>' => match iter.next_if(|c| c.char == '=') {
                    Some(_) => Token::Operator(Operator::GreaterOrEqual),
                    None => Token::Operator(Operator::GreaterThan),
                },

                _ => {
                    return Err(LexerError {
                        message: "not a valid token",
                        char_elem: c,
                    })
                }
            };

            TokenElem {
                token,
                span: Span {
                    line: c.line,
                    start_position: c.position,
                    end_position: c.position,
                },
            }
        };

        tokens.push(token_elem);
    }

    Ok(tokens)
}

#[derive(Debug, Clone, Copy)]
pub struct CharElem {
    pub char: char,
    pub line: usize,
    pub position: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenElem {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: &'static str,
    pub char_elem: CharElem,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Identifier(String),
    Constant(i64), // TODO: Maybe a custom constant type?
    Keyword(Keyword),
    Operator(Operator),
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(name) => write!(f, "identifier `{name}`"),
            Self::Constant(i) => write!(f, "constant with value `{i}`"),
            Self::Keyword(keyword) => write!(f, "keyword `{keyword}`"),
            Self::Operator(op) => write!(f, "{op:?}"),
            Self::OpenParenthesis => f.write_str("`(`"),
            Self::CloseParenthesis => f.write_str("`)`"),
            Self::OpenBrace => f.write_str("`{`"),
            Self::CloseBrace => f.write_str("`}`"),
            Self::Semicolon => f.write_str("`;`"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => f.write_str("int"),
            Self::Void => f.write_str("void"),
            Self::Return => f.write_str("return"),
        }
    }
}

impl FromStr for Keyword {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Self::Int),
            "void" => Ok(Self::Void),
            "return" => Ok(Self::Return),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Assignment,

    Plus,  // +
    Minus, // -

    Increment, // ++
    Decrement, // --

    Asterisk, // *
    Slash,    // /
    Percent,  // %
    Tilde,    // ~

    Not, // !
    And, // &&
    Or,  // ||

    Equal,          // ==
    NotEqual,       // !=
    LessThan,       // <
    GreaterThan,    // >
    LessOrEqual,    // <=
    GreaterOrEqual, // <=
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn statement() {
        let raw = "return var_1 * (-490)";
        let tokens: Vec<_> = run(&[raw]).unwrap().into_iter().map(|t| t.token).collect();

        let expected = [
            Token::Keyword(Keyword::Return),
            Token::Identifier(String::from("var_1")),
            Token::Operator(Operator::Asterisk),
            Token::OpenParenthesis,
            Token::Operator(Operator::Minus),
            Token::Constant(490),
            Token::CloseParenthesis,
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn unicode() {
        let identifier = String::from("_æôπ_üµ2_京แðİ");
        let raw = format!("return {identifier}");
        let tokens: Vec<_> = run(&[&raw]).unwrap().into_iter().map(|t| t.token).collect();

        let expected = [
            Token::Keyword(Keyword::Return),
            Token::Identifier(identifier),
        ];

        assert_eq!(tokens, expected);
    }
}
