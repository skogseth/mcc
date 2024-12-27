use regex::Regex;

pub fn run(content: &str) -> Result<Vec<Token>, anyhow::Error> {
    let mut parser = Parser::new(content);

    let mut tokens = Vec::new();
    while let Some(token) = parser.find_next()? {
        tokens.push(token);
    }

    Ok(tokens)
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    Int,
    Void,
    Return,
}

#[derive(Debug, Clone)]
pub enum Token {
    Identifier(String),
    Constant(i64), // TODO: Maybe a custom constant type?
    Keyword(Keyword),
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

struct Parser<'s> {
    s: &'s str,
    identifier: Regex,
    constant: Regex,
}

impl<'s> Parser<'s> {
    fn new(s: &'s str) -> Self {
        Self {
            s,
            identifier: Regex::new(r#"[a-zA-Z_]\w*\b"#).unwrap(),
            constant: Regex::new(r#"[0-9]+\b"#).unwrap(),
        }
    }

    fn find_next(&mut self) -> Result<Option<Token>, anyhow::Error> {
        self.s = self.s.trim_start();

        if self.s.is_empty() {
            return Ok(None);
        }

        match &self.s[0..1].chars().next().unwrap() {
            '(' => {
                self.s = &self.s[1..];
                Ok(Some(Token::OpenParenthesis))
            }
            ')' => {
                self.s = &self.s[1..];
                Ok(Some(Token::CloseParenthesis))
            }
            '{' => {
                self.s = &self.s[1..];
                Ok(Some(Token::OpenBrace))
            }
            '}' => {
                self.s = &self.s[1..];
                Ok(Some(Token::CloseBrace))
            }
            ';' => {
                self.s = &self.s[1..];
                Ok(Some(Token::Semicolon))
            }

            // For identifiers we use regex
            'a'..='z' | 'A'..='Z' | '_' => {
                let m = self.identifier.find(&self.s).unwrap();
                let range = m.range();

                assert_eq!(range.start, 0);

                let end = range.end;
                let extracted = &self.s[range];
                self.s = &self.s[end..];

                let token = match extracted {
                    "int" => Token::Keyword(Keyword::Int),
                    "void" => Token::Keyword(Keyword::Void),
                    "return" => Token::Keyword(Keyword::Return),
                    _ => Token::Identifier(extracted.to_owned()),
                };
                return Ok(Some(token));
            }

            // For constants we use regex
            '0'..='9' => {
                let m = self.constant.find(&self.s).unwrap();
                let range = m.range();

                assert_eq!(range.start, 0);

                let end = range.end;
                let extracted = &self.s[range];
                self.s = &self.s[end..];

                let parsed: i64 = extracted.parse().unwrap();
                return Ok(Some(Token::Constant(parsed)));
            }

            c => Err(anyhow::anyhow!("No matching token found for {c}")),
        }
    }
}
