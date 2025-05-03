use std::collections::BTreeMap;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Error;

pub fn main(input: &str) -> String {
    let mut defines: BTreeMap<&str, String> = BTreeMap::new();
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
                    // TODO: This is horrible performance wise, right?
                    let mut line: String = line.to_owned();
                    for (define, replace_with) in &defines {
                        line = line.replace(define, replace_with);
                    }

                    cond_counter.current().unwrap_or(true).then_some(line)
                }
            }
        })
        .collect()
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

            _ => Err(format!("unknown preprocessor directive: {s}")),
        }
    }
}

fn handle_directive<'a>(
    rest: &'a str,
    defines: &mut BTreeMap<&'a str, String>,
    cond_counter: &mut CondCounter,
) {
    let mut split = rest.split_whitespace();

    // TODO: Handle these unwraps, somehow?
    let directive: Directive = split.next().unwrap().parse().unwrap();

    match directive {
        Directive::Define => {
            let name = split.next().unwrap();
            let remains: String = split.collect();
            defines.insert(name, remains);
        }
        Directive::Undef => {
            let name = split.next().unwrap();
            assert!(split.next().is_none());
            defines.remove(name).unwrap();
        }

        Directive::Else => {
            assert!(split.next().is_none());
            let current = cond_counter.current_mut().unwrap();
            *current = !*current;
        }
        Directive::Endif => {
            assert!(split.next().is_none());
            cond_counter.pop().unwrap();
        }

        Directive::Ifdef => {
            let name = split.next().unwrap();
            assert!(split.next().is_none());
            cond_counter.push(defines.contains_key(name));
        }
        Directive::Ifndef => {
            let name = split.next().unwrap();
            assert!(split.next().is_none());
            cond_counter.push(!defines.contains_key(name));
        }
        Directive::Elifdef => {
            let name = split.next().unwrap();
            assert!(split.next().is_none());
            let current = cond_counter.current_mut().unwrap();
            *current = !*current && defines.contains_key(name);
        }
        Directive::Elifndef => {
            let name = split.next().unwrap();
            assert!(split.next().is_none());
            let current = cond_counter.current_mut().unwrap();
            *current = !*current && !defines.contains_key(name);
        }
    }
}

use utils::*;
mod utils {
    #[derive(Debug, Default)]
    pub struct CondCounter(Vec<bool>);

    impl CondCounter {
        pub fn push(&mut self, value: bool) {
            self.0.push(value);
        }

        pub fn current(&self) -> Option<bool> {
            self.0.last().copied()
        }

        pub fn current_mut(&mut self) -> Option<&mut bool> {
            self.0.last_mut()
        }

        pub fn pop(&mut self) -> Option<bool> {
            self.0.pop()
        }
    }
}
