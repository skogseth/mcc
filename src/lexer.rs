use std::{iter::Peekable, str::FromStr};

use crate::{Identifier, Span};

#[derive(Debug)]
pub struct Lexer<I: Iterator<Item = CharElem>>(Peekable<I>);

pub fn make_lexer(lines: &[&str]) -> Lexer<impl Iterator<Item = CharElem>> {
    let iter = lines
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

    Lexer(iter)
}

impl<I: Iterator<Item = CharElem>> Iterator for Lexer<I> {
    type Item = Result<TokenElem, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = loop {
            let c = self.0.next()?;
            if !c.char.is_whitespace() {
                break c;
            }
        };

        // If we encounter any "normal characters" then we have found an identifier
        if c.char.is_alphabetic() || c.char == '_' {
            let mut extracted = String::from(c.char);
            let mut span = Span {
                line: c.line,
                start_position: c.position,
                end_position: c.position,
            };

            // Important that we allow numbers here, but not in the initial position ^
            while let Some(c) = self
                .0
                .next_if(|c| c.char.is_alphanumeric() || c.char == '_')
            {
                extracted.push(c.char);
                span.end_position = c.position;
            }

            let token = match extracted.parse::<Keyword>() {
                Ok(keyword) => Token::Keyword(keyword),
                Err(()) => Token::Identifier(Identifier(extracted)),
            };

            Some(Ok(TokenElem { token, span }))
        } else if c.char.is_numeric() {
            let mut extracted = String::from(c.char);
            let mut span = Span {
                line: c.line,
                start_position: c.position,
                end_position: c.position,
            };

            while let Some(c) = self.0.next_if(|c| c.char.is_alphanumeric()) {
                if c.char.is_numeric() {
                    extracted.push(c.char);
                    span.end_position = c.position;
                } else {
                    return Some(Err(LexerError {
                        message: "encountered alphabetic character as part of a number",
                        char_elem: c,
                    }));
                }
            }

            let token = Token::Constant(extracted.parse().unwrap());
            Some(Ok(TokenElem { token, span }))
        } else {
            let token_elem: TokenElem = match c.char {
                '(' => Token::OpenParenthesis.with_span(Span::single(c)),
                ')' => Token::CloseParenthesis.with_span(Span::single(c)),
                '{' => Token::OpenBrace.with_span(Span::single(c)),
                '}' => Token::CloseBrace.with_span(Span::single(c)),
                ';' => Token::Semicolon.with_span(Span::single(c)),

                // This can be either plus ('+') or increment ('++')
                '+' => match self.0.next_if(|c| c.char == '+') {
                    Some(c2) => Token::Operator(Operator::Increment).with_span(Span::wide(c, c2)),
                    None => Token::Operator(Operator::Plus).with_span(Span::single(c)),
                },

                // This can be either minus ('-') or decrement ('--')
                '-' => match self.0.next_if(|c| c.char == '-') {
                    Some(c2) => Token::Operator(Operator::Decrement).with_span(Span::wide(c, c2)),
                    None => Token::Operator(Operator::Minus).with_span(Span::single(c)),
                },

                '*' => Token::Operator(Operator::Asterisk).with_span(Span::single(c)),
                '/' => Token::Operator(Operator::Slash).with_span(Span::single(c)),
                '%' => Token::Operator(Operator::Percent).with_span(Span::single(c)),
                '~' => Token::Operator(Operator::Tilde).with_span(Span::single(c)),

                // Could be either "not" ('!') or "not equal" ('!=')
                '!' => match self.0.next_if(|c| c.char == '=') {
                    Some(c2) => Token::Operator(Operator::NotEqual).with_span(Span::wide(c, c2)),
                    None => Token::Operator(Operator::Not).with_span(Span::single(c)),
                },

                '&' => match self.0.next_if(|c| c.char == '&') {
                    Some(c2) => Token::Operator(Operator::And).with_span(Span::wide(c, c2)),
                    None => {
                        return Some(Err(LexerError {
                            message: "bitwise and ('&') is not supported",
                            char_elem: c,
                        }));
                    }
                },
                '|' => match self.0.next_if(|c| c.char == '|') {
                    Some(c2) => Token::Operator(Operator::Or).with_span(Span::wide(c, c2)),
                    None => {
                        return Some(Err(LexerError {
                            message: "bitwise or ('|') is not supported",
                            char_elem: c,
                        }));
                    }
                },

                '=' => match self.0.next_if(|c| c.char == '=') {
                    Some(c2) => Token::Operator(Operator::Equal).with_span(Span::wide(c, c2)),
                    None => Token::Operator(Operator::Assignment).with_span(Span::single(c)),
                },

                '<' => match self.0.next_if(|c| c.char == '=') {
                    Some(c2) => Token::Operator(Operator::LessOrEqual).with_span(Span::wide(c, c2)),
                    None => Token::Operator(Operator::LessThan).with_span(Span::single(c)),
                },
                '>' => match self.0.next_if(|c| c.char == '=') {
                    Some(c2) => {
                        Token::Operator(Operator::GreaterOrEqual).with_span(Span::wide(c, c2))
                    }
                    None => Token::Operator(Operator::GreaterThan).with_span(Span::single(c)),
                },

                _ => {
                    return Some(Err(LexerError {
                        message: "not a valid token",
                        char_elem: c,
                    }));
                }
            };

            Some(Ok(token_elem))
        }
    }
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
    Identifier(Identifier),
    Constant(i64), // TODO: Maybe a custom constant type?
    Keyword(Keyword),
    Operator(Operator),
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

impl Token {
    fn with_span(self, span: Span) -> TokenElem {
        TokenElem { token: self, span }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(name) => write!(f, "identifier `{name}`"),
            Self::Constant(i) => write!(f, "constant with value `{i}`"),
            Self::Keyword(keyword) => write!(f, "keyword `{keyword}`"),
            Self::Operator(op) => write!(f, "`{op}` (operator)"),
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
    Return,
    Typedef,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Return => f.write_str("return"),
            Self::Typedef => f.write_str("typedef"),
        }
    }
}

impl FromStr for Keyword {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "return" => Ok(Self::Return),
            "typedef" => Ok(Self::Typedef),
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

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assignment => f.write_str("="),
            Self::Plus => f.write_str("+"),
            Self::Minus => f.write_str("-"),

            Self::Increment => f.write_str("++"),
            Self::Decrement => f.write_str("--"),

            Self::Asterisk => f.write_str("*"),
            Self::Slash => f.write_str("/"),
            Self::Percent => f.write_str("%"),
            Self::Tilde => f.write_str("~"),

            Self::Not => f.write_str("!"),
            Self::And => f.write_str("&&"),
            Self::Or => f.write_str("||"),

            Self::Equal => f.write_str("=="),
            Self::NotEqual => f.write_str("!="),
            Self::LessThan => f.write_str("<"),
            Self::GreaterThan => f.write_str(">"),
            Self::LessOrEqual => f.write_str("<="),
            Self::GreaterOrEqual => f.write_str("<="),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn statement() {
        let raw = "return var_1 * (-490)";
        let tokens: Vec<_> = make_lexer(&[raw]).map(|t| t.unwrap().token).collect();

        let expected = [
            Token::Keyword(Keyword::Return),
            Token::Identifier(Identifier::new("var_1")),
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
        let identifier = Identifier::new("_æôπ_üµ2_京แðİ");
        let raw = format!("return {identifier}");
        let tokens: Vec<_> = make_lexer(&[&raw]).map(|t| t.unwrap().token).collect();

        let expected = [
            Token::Keyword(Keyword::Return),
            Token::Identifier(identifier),
        ];

        assert_eq!(tokens, expected);
    }
}
