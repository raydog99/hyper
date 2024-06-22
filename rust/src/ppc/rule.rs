use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Ast {
    String(String),
    Template(Vec<String>),
    Equal(Box<Ast>, Box<Ast>),
    NotEqual(Box<Ast>, Box<Ast>),
    Rewrite(Box<Ast>, (Box<Ast>, Box<Ast>)),
    Match(Box<Ast>, Vec<(Box<Ast>, Vec<Ast>)>),
    True,
    False,
    Option(String),
}

pub type Rule = Vec<Ast>;

#[derive(Debug, Default)]
pub struct Options {
    pub nested: bool,
    pub strict: bool,
}

#[derive(Debug)]
struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}", self.0)
    }
}

impl Error for ParseError {}

fn parse_ast(tokens: &[&str]) -> Result<(Ast, &[&str]), Box<dyn Error>> {
    match tokens.first() {
        Some(&"true") => Ok((Ast::True, &tokens[1..])),
        Some(&"false") => Ok((Ast::False, &tokens[1..])),
        Some(&"option") if tokens.len() > 1 => Ok((Ast::Option(tokens[1].to_string()), &tokens[2..])),
        Some(&"=") if tokens.len() > 3 => {
            let (lhs, _) = parse_ast(&tokens[1..2])?;
            let (rhs, rest) = parse_ast(&tokens[3..])?;
            Ok((Ast::Equal(Box::new(lhs), Box::new(rhs)), rest))
        },
        Some(&"!=") if tokens.len() > 2 => {
            let (lhs, _) = parse_ast(&tokens[1..2])?;
            let (rhs, rest) = parse_ast(&tokens[2..])?;
            Ok((Ast::NotEqual(Box::new(lhs), Box::new(rhs)), rest))
        },
        Some(&"->") if tokens.len() > 3 => {
            let (lhs, _) = parse_ast(&tokens[1..2])?;
            let (rhs, rest) = parse_ast(&tokens[3..])?;
            Ok((Ast::Rewrite(Box::new(lhs), (Box::new(rhs.clone()), Box::new(rhs))), rest))
        },
        Some(&token) => Ok((Ast::String(token.to_string()), &tokens[1..])),
        None => Err(Box::new(ParseError("Unexpected end of input".to_string())))
    }
}

fn parse_rule(tokens: &[&str]) -> Result<Rule, Box<dyn Error>> {
    let mut rule = Vec::new();
    let mut rest = tokens;

    while !rest.is_empty() {
        if rest[0] == "," {
            rest = &rest[1..];
            continue;
        }
        let (ast, new_rest) = parse_ast(rest)?;
        rule.push(ast);
        rest = new_rest;
    }

    Ok(rule)
}

pub fn create(rule_text: &str) -> Result<Rule, Box<dyn Error>> {
    let tokens: Vec<&str> = rule_text.split_whitespace().collect();
    if tokens.first() != Some(&"where") {
        return Err(Box::new(ParseError("Rule must start with 'where'".to_string())));
    }
    parse_rule(&tokens[1..])
}

pub fn options(rule: &Rule) -> Options {
    rule.iter().fold(Options::default(), |mut opts, ast| {
        if let Ast::Option(name) = ast {
            match name.as_str() {
                "nested" => opts.nested = true,
                "strict" => opts.strict = true,
                _ => {}
            }
        }
        opts
    })
}

pub fn is_strict(rule: &Rule) -> bool {
    options(rule).strict
}