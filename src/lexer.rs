use std::str::FromStr;

pub fn run(content: &str) -> Result<Vec<Token>, anyhow::Error> {
    let mut chars = content.chars().peekable();

    let mut tokens = Vec::new();
    while let Some(c) = chars.next() {
        match c {
            _ if c.is_whitespace() => continue,

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

            // If we encounter any "normal characters" then we have found an identifier
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut extracted = String::from(c);

                // Important that we allow numbers here, but not in the initial position ^
                while let Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9') = chars.peek() {
                    let c = chars.next().unwrap();
                    extracted.push(c);
                }

                let token = match extracted.parse::<Keyword>() {
                    Ok(keyword) => Token::Keyword(keyword),
                    Err(()) => Token::Identifier(extracted),
                };

                tokens.push(token);
            }

            '0'..='9' => {
                let mut extracted = String::from(c);
                loop {
                    match chars.peek() {
                        Some('0'..='9') => {
                            let c = chars.next().unwrap();
                            extracted.push(c);
                        }

                        Some(c) if c.is_alphabetic() => {
                            return Err(anyhow::anyhow!("Bad token found: {c}"))
                        }

                        Some(_) | None => break,
                    }
                }

                let parsed: i64 = extracted.parse().unwrap();
                tokens.push(Token::Constant(parsed));
            }

            c => return Err(anyhow::anyhow!("No matching token found for {c}")),
        }
    }

    Ok(tokens)
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,  // +
    Minus, // -

    Increment, // ++
    Decrement, // --

    Asterisk, // *
    Slash,    // /
    Percent,  // %
    Tilde,    // ~
}
