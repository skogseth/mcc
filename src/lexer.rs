use std::str::FromStr;

use anyhow::anyhow;

pub fn run(content: &str) -> Result<Vec<Token>, anyhow::Error> {
    let mut chars = content.chars().peekable();

    let mut tokens = Vec::new();
    while let Some(c) = chars.next() {
        if c.is_whitespace() {
            continue;
        }

        // If we encounter any "normal characters" then we have found an identifier
        if c.is_alphabetic() || c == '_' {
            let mut extracted = String::from(c);

            // Important that we allow numbers here, but not in the initial position ^
            while let Some(c) = chars.next_if(|c| c.is_alphanumeric() || *c == '_') {
                extracted.push(c);
            }

            let token = match extracted.parse::<Keyword>() {
                Ok(keyword) => Token::Keyword(keyword),
                Err(()) => Token::Identifier(extracted),
            };

            tokens.push(token);
            continue;
        }

        match c {
            '0'..='9' => {
                let mut extracted = String::from(c);
                loop {
                    match chars.peek() {
                        Some('0'..='9') => {
                            let c = chars.next().unwrap();
                            extracted.push(c);
                        }

                        Some(c) if c.is_alphabetic() => {
                            return Err(anyhow!("Bad token found: {c}"))
                        }

                        Some(_) | None => break,
                    }
                }

                let parsed: i64 = extracted.parse().unwrap();
                tokens.push(Token::Constant(parsed));
            }

            '(' => tokens.push(Token::OpenParenthesis),
            ')' => tokens.push(Token::CloseParenthesis),
            '{' => tokens.push(Token::OpenBrace),
            '}' => tokens.push(Token::CloseBrace),
            ';' => tokens.push(Token::Semicolon),

            // This can be either plus ('+') or increment ('++')
            '+' => match chars.next_if(|c| *c == '+') {
                Some(_) => tokens.push(Token::Operator(Operator::Increment)),
                None => tokens.push(Token::Operator(Operator::Plus)),
            },

            // This can be either minus ('-') or decrement ('--')
            '-' => match chars.next_if(|c| *c == '-') {
                Some(_) => tokens.push(Token::Operator(Operator::Decrement)),
                None => tokens.push(Token::Operator(Operator::Minus)),
            },

            '*' => tokens.push(Token::Operator(Operator::Asterisk)),
            '/' => tokens.push(Token::Operator(Operator::Slash)),
            '%' => tokens.push(Token::Operator(Operator::Percent)),
            '~' => tokens.push(Token::Operator(Operator::Tilde)),

            // Could be either "not" ('!') or "not equal" ('!=')
            '!' => match chars.next_if(|c| *c == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::NotEqual)),
                None => tokens.push(Token::Operator(Operator::Not)),
            },

            '&' => match chars.next_if(|c| *c == '&') {
                Some(_) => tokens.push(Token::Operator(Operator::And)),
                None => return Err(anyhow!("bitwise and ('&') is not supported")),
            },
            '|' => match chars.next_if(|c| *c == '|') {
                Some(_) => tokens.push(Token::Operator(Operator::Or)),
                None => return Err(anyhow!("bitwise or ('|') is not supported")),
            },

            '=' => match chars.next_if(|c| *c == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::Equal)),
                None => tokens.push(Token::Operator(Operator::Assignment)),
            },

            '<' => match chars.next_if(|c| *c == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::LessOrEqual)),
                None => tokens.push(Token::Operator(Operator::LessThan)),
            },
            '>' => match chars.next_if(|c| *c == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::GreaterOrEqual)),
                None => tokens.push(Token::Operator(Operator::GreaterThan)),
            },

            c => return Err(anyhow!("No matching token found for {c}")),
        }
    }

    Ok(tokens)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Int,
    Void,
    Return,
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
        let tokens = run(raw).unwrap();

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
        let raw = "return _æôπ_üµ2_京แðİ";
        let tokens = run(raw).unwrap();

        let expected = [
            Token::Keyword(Keyword::Return),
            Token::Identifier(String::from("_æôπ_üµ2_京แðİ")),
        ];

        assert_eq!(tokens, expected);
    }
}
