use std::str::FromStr;

use thiserror::Error;

pub fn run(content: String) -> Result<Vec<Token>, LexerError> {
    let mut iter = content
        .lines()
        .enumerate()
        .flat_map(|(i, line)| {
            line.chars().enumerate().map(move |(j, char)| CharWithSpan {
                char,
                line: i,
                position: j,
            })
        })
        .peekable();

    let mut tokens = Vec::new();
    while let Some(c) = iter.next() {
        if c.char.is_whitespace() {
            continue;
        }

        // If we encounter any "normal characters" then we have found an identifier
        if c.char.is_alphabetic() || c.char == '_' {
            let mut extracted = String::from(c.char);

            // Important that we allow numbers here, but not in the initial position ^
            while let Some(c) = iter.next_if(|c| c.char.is_alphanumeric() || c.char == '_') {
                extracted.push(c.char);
            }

            let token = match extracted.parse::<Keyword>() {
                Ok(keyword) => Token::Keyword(keyword),
                Err(()) => Token::Identifier(extracted),
            };

            tokens.push(token);
            continue;
        }

        match c.char {
            '0'..='9' => {
                let mut extracted = String::from(c.char);
                loop {
                    match iter.peek() {
                        Some(CharWithSpan {
                            char: '0'..='9', ..
                        }) => {
                            let c = iter.next().unwrap();
                            extracted.push(c.char);
                        }

                        Some(c) if c.char.is_alphabetic() => {
                            return Err(LexerError {
                                message: "encountered alphabetic character as part of a number",
                                char_elem: *c,
                                file_content: content,
                            })
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
            '+' => match iter.next_if(|c| c.char == '+') {
                Some(_) => tokens.push(Token::Operator(Operator::Increment)),
                None => tokens.push(Token::Operator(Operator::Plus)),
            },

            // This can be either minus ('-') or decrement ('--')
            '-' => match iter.next_if(|c| c.char == '-') {
                Some(_) => tokens.push(Token::Operator(Operator::Decrement)),
                None => tokens.push(Token::Operator(Operator::Minus)),
            },

            '*' => tokens.push(Token::Operator(Operator::Asterisk)),
            '/' => tokens.push(Token::Operator(Operator::Slash)),
            '%' => tokens.push(Token::Operator(Operator::Percent)),
            '~' => tokens.push(Token::Operator(Operator::Tilde)),

            // Could be either "not" ('!') or "not equal" ('!=')
            '!' => match iter.next_if(|c| c.char == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::NotEqual)),
                None => tokens.push(Token::Operator(Operator::Not)),
            },

            '&' => match iter.next_if(|c| c.char == '&') {
                Some(_) => tokens.push(Token::Operator(Operator::And)),
                None => {
                    return Err(LexerError {
                        message: "bitwise and ('&') is not supported",
                        char_elem: c,
                        file_content: content,
                    })
                }
            },
            '|' => match iter.next_if(|c| c.char == '|') {
                Some(_) => tokens.push(Token::Operator(Operator::Or)),
                None => {
                    return Err(LexerError {
                        message: "bitwise or ('|') is not supported",
                        char_elem: c,
                        file_content: content,
                    })
                }
            },

            '=' => match iter.next_if(|c| c.char == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::Equal)),
                None => tokens.push(Token::Operator(Operator::Assignment)),
            },

            '<' => match iter.next_if(|c| c.char == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::LessOrEqual)),
                None => tokens.push(Token::Operator(Operator::LessThan)),
            },
            '>' => match iter.next_if(|c| c.char == '=') {
                Some(_) => tokens.push(Token::Operator(Operator::GreaterOrEqual)),
                None => tokens.push(Token::Operator(Operator::GreaterThan)),
            },

            _ => {
                return Err(LexerError {
                    message: "not a valid token",
                    char_elem: c,
                    file_content: content,
                })
            }
        }
    }

    Ok(tokens)
}

#[derive(Debug, Clone, Copy)]
pub struct CharWithSpan {
    pub char: char,
    pub line: usize,
    pub position: usize,
}

#[derive(Debug, Clone, Error)]
pub struct LexerError {
    pub message: &'static str,
    pub char_elem: CharWithSpan,
    pub file_content: String,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lines: Vec<(usize, &str)> = self
            .file_content
            .lines()
            .enumerate()
            .map(|(i, line)| (i + 1, line))
            .collect();

        let lower_line_number = usize::saturating_sub(self.char_elem.line, 2);
        let correct_line_number = usize::saturating_add(self.char_elem.line, 1);
        let upper_line_number = std::cmp::min(
            usize::saturating_add(self.char_elem.line, 2),
            lines.len() - 1,
        );

        for (line_number, line) in &lines[lower_line_number..correct_line_number] {
            writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
        }

        writeln!(
            f,
            "- {fill}{marker} {message}",
            fill = " ".repeat(self.char_elem.position),
            marker = console::style("^").red(),
            message = console::style(self.message).red(),
        )?;

        for (line_number, line) in &lines[correct_line_number..=upper_line_number] {
            writeln!(f, "{} {}", console::style(line_number).blue(), line)?;
        }

        Ok(())
    }
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
        let raw = String::from("return var_1 * (-490)");
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
        let raw = String::from("return _æôπ_üµ2_京แðİ");
        let tokens = run(raw).unwrap();

        let expected = [
            Token::Keyword(Keyword::Return),
            Token::Identifier(String::from("_æôπ_üµ2_京แðİ")),
        ];

        assert_eq!(tokens, expected);
    }
}
