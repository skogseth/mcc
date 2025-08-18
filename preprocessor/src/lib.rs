use std::collections::BTreeMap;
use std::str::FromStr;

pub fn main(input: &str) -> String {
    let input = initial_processing(input);

    let mut defines: BTreeMap<&str, &str> = BTreeMap::new();
    let mut cond_counter = CondCounter::default();

    input
        .lines()
        .filter_map(|line| -> Option<String> {
            match line.strip_prefix('#') {
                // Line contains a preprocessor directive
                Some(rest) => {
                    handle_directive(rest, &mut defines, &mut cond_counter);
                    None
                }

                // Line does not contain a preprocessor directive
                None => {
                    if !cond_counter.currently_active() {
                        return None;
                    }

                    // TODO: This is horrible performance wise, right?
                    let mut line: String = line.to_owned();
                    for (define, replace_with) in &defines {
                        line = line.replace(define, replace_with);
                    }

                    Some(line)
                }
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}

/// The goals of this function is to (eventually):
/// 1. Replace all line splits, either `\n` or `\r\n`, with `\n`
/// 2. Merge "continued lines" (lines that end with a `\`)
/// 3. Replace all comments with single spaces
fn initial_processing(s: &str) -> String {
    // Step 1 (TODO: Horribly inefficient, probably)
    let s: String = s.lines().collect::<Vec<&str>>().join("\n");

    // Step 2
    let s: String = {
        let mut chars = s.chars().peekable();
        let mut s = String::new();

        while let Some(c) = chars.next() {
            match c {
                '\\' if chars.next_if(|c| *c == '\n').is_some() => {
                    // Don't add either of the characters to the string
                }
                _ => s.push(c),
            }
        }

        s
    };

    // Step 3
    let s: String = {
        let mut chars = s.chars().peekable();
        let mut s = String::new();

        while let Some(c) = chars.next() {
            match c {
                // If we encounter a string literal, we just read the entire thing
                // (This is done to avoid processing comments in string literals)
                // NOTE: This must happen after "continued lines" have been parsed!!
                '\"' => {
                    // Take characters until we find the end ...
                    while let Some(next) = chars.next_if(|c| *c != '\"') {
                        s.push(next);
                    }

                    // ... and then remember to get the end of the literal as well
                    if let Some(next) = chars.next() {
                        debug_assert_eq!(next, '\"');
                        s.push(next);
                    }
                }

                // This could be comment!
                '/' => {
                    if chars.next_if(|c| *c == '/').is_some() {
                        // It's a comment! Read until the end of the line (and discard)
                        while chars.next_if(|c| *c != '\n').is_some() {}
                    }

                    // TODO: Support block comments
                }

                _ => s.push(c),
            }
        }

        s
    };

    s
}

enum Directive {
    // Macros
    Define,
    Undef,

    // Conditionals
    // If,
    // Elif,
    Else,
    Endif,

    // Macro conditionals
    Ifdef,
    Ifndef,
    Elifdef,
    Elifndef,

    // Output
    Error,
    Warning,

    // The special one
    Pragma,
}

impl FromStr for Directive {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "define" => Ok(Self::Define),
            "undef" => Ok(Self::Undef),

            // "if" => Ok(Self::If),
            // "elif" => Ok(Self::Elif),
            "else" => Ok(Self::Else),
            "endif" => Ok(Self::Endif),

            "ifdef" => Ok(Self::Ifdef),
            "ifndef" => Ok(Self::Ifndef),
            "elifdef" => Ok(Self::Elifdef),
            "elifndef" => Ok(Self::Elifndef),

            "error" => Ok(Self::Error),
            "warning" => Ok(Self::Warning),

            "pragma" => Ok(Self::Pragma),

            _ => Err(format!("unknown preprocessor directive: {s}")),
        }
    }
}

fn handle_directive<'a>(
    rest: &'a str,
    defines: &mut BTreeMap<&'a str, &'a str>,
    cond_counter: &mut CondCounter,
) {
    let (directive, rest) = rest.split_once(char::is_whitespace).unwrap_or((rest, ""));

    // TODO: Handle parsing error
    let directive: Directive = directive.parse().unwrap();

    match directive {
        Directive::Define => {
            if cond_counter.currently_active() {
                let (name, remains) = rest.split_once(char::is_whitespace).unwrap();
                defines.insert(name, remains);
            }
        }
        Directive::Undef => {
            if cond_counter.currently_active() {
                let name = rest;
                assert!(!name.chars().any(char::is_whitespace));
                defines.remove(name).unwrap();
            }
        }

        Directive::Else => {
            assert!(rest.is_empty());
            cond_counter.else_if(|| true);
        }
        Directive::Endif => {
            assert!(rest.is_empty());
            cond_counter.pop().unwrap();
        }

        Directive::Ifdef => {
            let name = rest;
            assert!(!name.chars().any(char::is_whitespace));
            cond_counter.push(defines.contains_key(name));
        }
        Directive::Ifndef => {
            let name = rest;
            assert!(!name.chars().any(char::is_whitespace));
            cond_counter.push(!defines.contains_key(name));
        }
        Directive::Elifdef => {
            let name = rest;
            assert!(!name.chars().any(char::is_whitespace));
            cond_counter.else_if(|| defines.contains_key(name));
        }
        Directive::Elifndef => {
            let name = rest;
            assert!(!name.chars().any(char::is_whitespace));
            cond_counter.else_if(|| !defines.contains_key(name));
        }

        Directive::Error => {
            if cond_counter.currently_active() {
                panic!("{rest}");
            }
        }
        Directive::Warning => {
            if cond_counter.currently_active() {
                eprintln!("{rest}");
            }
        }

        Directive::Pragma => { /* We currently don't support any pragmas */ }
    }
}

use utils::*;
mod utils {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum CondState {
        Active,
        Inactive { was_active: bool },
    }

    #[derive(Debug, Default)]
    pub struct CondCounter(Vec<CondState>);

    impl CondCounter {
        pub fn push(&mut self, active: bool) {
            self.0.push(match active {
                true => CondState::Active,
                false => CondState::Inactive { was_active: false },
            });
        }

        pub fn currently_active(&self) -> bool {
            self.0
                .last()
                .copied()
                .map(|state| state == CondState::Active)
                .unwrap_or(true)
        }

        // TODO: Not sure I like this name
        pub fn else_if(&mut self, pred: impl Fn() -> bool) {
            let current = self.0.last_mut().unwrap();
            match *current {
                CondState::Active => *current = CondState::Inactive { was_active: true },
                CondState::Inactive { was_active: true } => { /* nothing to do */ }
                CondState::Inactive { was_active: false } => {
                    if pred() {
                        *current = CondState::Active;
                    }
                }
            }
        }

        pub fn pop(&mut self) -> Option<CondState> {
            self.0.pop()
        }
    }
}
