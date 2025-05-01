use std::collections::BTreeMap;
use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct Error;

pub fn main(input: &str) -> String {
    let mut defines: BTreeMap<&str, String> = BTreeMap::new();

    input
        .lines()
        .filter_map(|line| -> Option<String> {
            match line.strip_prefix('#') {
                // Line contains a preprocessor directive
                Some(rest) => {
                    let mut split = rest.split_whitespace();

                    // TODO: Handle these unwraps, somehow?
                    let directive: Directive = split.next().unwrap().parse().unwrap();

                    match directive {
                        Directive::Define => {
                            let name = split.next().unwrap();
                            let remains: String = split.collect();
                            defines.insert(name, remains);
                            None
                        }
                    }
                }

                // Line does not contain a preprocessor directive
                None => {
                    // TODO: This is horrible performance wise, right?
                    let mut line: String = line.to_owned();
                    for (define, replace_with) in &defines {
                        line = line.replace(define, replace_with);
                    }

                    Some(line)
                }
            }
        })
        .collect()
}

enum Directive {
    Define,
}

impl FromStr for Directive {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "define" => Ok(Self::Define),
            _ => Err(format!("unknown preprocessor directive: {s}")),
        }
    }
}
