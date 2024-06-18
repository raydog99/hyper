use std::collections::VecDeque;

#[derive(Debug, PartialEq)]
pub enum Hole {
    Hole {
        pattern: String,
        variable: String,
        offset: usize,
        kind: String,
    },
    Constant(String),
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '\''
}

fn parse_identifier(input: &str) -> Option<(&str, String)> {
    let mut chars = input.chars();
    let mut identifier = String::new();

    if let Some(first_char) = chars.next() {
        if is_identifier_char(first_char) {
            identifier.push(first_char);
            for c in chars {
                if is_identifier_char(c) {
                    identifier.push(c);
                } else {
                    break;
                }
            }
            return Some((&input[identifier.len()..], identifier));
        }
    }

    None
}

fn parse_hole_pattern(input: &str, left: &str, right: &str) -> Option<(&str, Hole)> {
    let mut chars = input.chars();
    let mut pattern = String::new();

    for c in chars.by_ref().take(left.len()) {
        pattern.push(c);
    }

    if pattern == left {
        let mut rest = &input[left.len()..];
        if let Some((remaining, variable)) = parse_identifier(rest) {
            rest = remaining;
            pattern.push_str(&variable);

            for c in rest.chars().take(right.len()) {
                pattern.push(c);
            }

            if &rest[..right.len()] == right {
                return Some((
                    &rest[right.len()..],
                    Hole::Hole {
                        pattern,
                        variable,
                        offset: input.len() - rest.len(),
                        kind: "value".to_string(),
                    },
                ));
            }
        }
    }

    None
}

fn parse_regex_hole(input: &str) -> Option<(&str, Hole)> {
    let mut chars = input.chars();
    let mut pattern = String::new();

    if chars.next() == Some('$') {
        pattern.push('$');
        let mut rest = &input[1..];

        if let Some((remaining, variable)) = parse_identifier(rest) {
            rest = remaining;
            pattern.push_str(&variable);

            if let Some(sep) = rest.chars().next() {
                pattern.push(sep);
                rest = &rest[1..];

                let mut regex_pattern = String::new();
                let mut bracket_depth = 0;

                for c in rest.chars() {
                    if c == '$' && bracket_depth == 0 {
                        regex_pattern.push('$');
                        return Some((
                            &rest[regex_pattern.len() + 1..],
                            Hole::Hole {
                                pattern: pattern + &regex_pattern,
                                variable,
                                offset: input.len() - rest.len(),
                                kind: "value".to_string(),
                            },
                        ));
                    } else {
                        regex_pattern.push(c);
                        if c == '[' {
                            bracket_depth += 1;
                        } else if c == ']' {
                            bracket_depth -= 1;
                        }
                    }
                }
            }
        }
    }

    None
}

fn parse_constant(input: &str) -> Option<(&str, Hole)> {
    let mut chars = input.chars();
    let mut constant = String::new();

    for c in chars.by_ref() {
        if c == '$' {
            break;
        }
        constant.push(c);
    }

    if !constant.is_empty() {
        return Some((&input[constant.len()..], Hole::Constant(constant)));
    }

    None
}

pub fn parse_template(input: &str) -> Vec<Hole> {
    let mut holes = Vec::new();
    let mut chars = VecDeque::from(input.chars());
    let mut offset = 0;

    while !chars.is_empty() {
        let remaining: String = chars.iter().collect();
        if let Some((rest, hole)) = parse_hole_pattern(&remaining, "", "")
            .or_else(|| parse_hole_pattern(&remaining, "[", "]"))
            .or_else(|| parse_hole_pattern(&remaining, "{", "}"))
            .or_else(|| parse_hole_pattern(&remaining, "(", ")"))
            .or_else(|| parse_hole_pattern(&remaining, "<", ">"))
            .or_else(|| parse_regex_hole(&remaining))
            .or_else(|| parse_constant(&remaining))
        {
            let parsed_len = remaining.len() - rest.len();
            for _ in 0..parsed_len {
                chars.pop_front();
            }
            match hole {
                Hole::Hole { pattern, variable, kind, .. } => {
                    holes.push(Hole::Hole {
                        pattern,
                        variable,
                        offset,
                        kind,
                    });
                    offset += pattern.len();
                }
                Hole::Constant(value) => {
                    holes.push(Hole::Constant(value));
                    offset += value.len();
                }
            }
        } else {
            break;
        }
    }

    holes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_template() {
        let input = "$name$ is $age$ years old.";
        let expected = vec![
            Hole::Hole {
                pattern: "$name$".to_string(),
                variable: "name".to_string(),
                offset: 0,
                kind: "value".to_string(),
            },
            Hole::Constant(" is ".to_string()),
            Hole::Hole {
                pattern: "$age$".to_string(),
                variable: "age".to_string(),
                offset: 7,
                kind: "value".to_string(),
            },
            Hole::Constant(" years old.".to_string()),
        ];
        assert_eq!(parse_template(input), expected);
    }
}