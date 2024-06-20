use std::env;
use std::path::Path;
use regex::Regex;

pub type Variable = String;
pub type Pattern = String;
pub type Offset = usize;
pub type Kind = AttributeKind;

#[derive(Debug, EquatCopy, Clone, Eq, PartialEq)]
pub enum AttributeKind {
    Value,
    Length,
    Lines,
    OffsetStart,
    OffsetEnd,
    LineStart,
    LineEnd,
    ColumnStart,
    ColumnEnd,
    FilePath,
    FileName,
    FileDirectory,
    Lowercase,
    Uppercase,
    Capitalize,
    Uncapitalize,
    UpperCamelCase,
    LowerCamelCase,
    UpperSnakeCase,
    LowerSnakeCase,
    External(String),
}

#[derive(Debug)]
pub enum TemplateElement {
    Constant(String),
    Hole {
        pattern: Pattern,
        variable: Variable,
        offset: Offset,
        kind: Kind,
    },
}

pub fn attribute_to_kind(s: &str) -> AttributeKind {
    match s {
        "value" => AttributeKind::Value,
        "length" => AttributeKind::Length,
        "lines" => AttributeKind::Lines,
        "offset" => AttributeKind::OffsetStart,
        "offset.start" => AttributeKind::OffsetStart,
        "offset.end" => AttributeKind::OffsetEnd,
        "line" => AttributeKind::LineStart,
        "line.start" => AttributeKind::LineStart,
        "line.end" => AttributeKind::LineEnd,
        "column" => AttributeKind::ColumnStart,
        "column.start" => AttributeKind::ColumnStart,
        "column.end" => AttributeKind::ColumnEnd,
        "file" => AttributeKind::FilePath,
        "file.path" => AttributeKind::FilePath,
        "file.name" => AttributeKind::FileName,
        "file.directory" => AttributeKind::FileDirectory,
        "lowercase" => AttributeKind::Lowercase,
        "UPPERCASE" => AttributeKind::Uppercase,
        "Capitalize" => AttributeKind::Capitalize,
        "uncapitalize" => AttributeKind::Uncapitalize,
        "UpperCamelCase" => AttributeKind::UpperCamelCase,
        "lowerCamelCase" => AttributeKind::LowerCamelCase,
        "UPPER_SNAKE_CASE" => AttributeKind::UpperSnakeCase,
        "lower_snake_case" => AttributeKind::LowerSnakeCase,
        "lsif.hover" => AttributeKind::External("lsif.hover".to_string()),
        _ => panic!("invalid attribute {}", s),
    }
}

fn camel_to_snake(s: &str) -> String {
    let mut words = Vec::new();
    let mut word = String::new();
    for c in s.chars() {
        if c.is_uppercase() {
            if !word.is_empty() {
                words.push(word.to_lowercase());
                word = String::new();
            }
            word.push(c.to_lowercase().next().unwrap());
        } else {
            word.push(c);
        }
    }
    if !word.is_empty() {
        words.push(word.to_lowercase());
    }
    words.join("_")
}

pub fn substitute_kind(
    filepath: Option<&Path>,
    hole: &TemplateElement,
    env: &[(Variable, String)],
) -> Option<String> {
    match hole {
        TemplateElement::Constant(_) => None,
        TemplateElement::Hole {
            variable,
            kind,
            ..
        } => {
            let lookup = |var: &Variable| env.iter().find(|(k, _)| k == var).map(|(_, v)| v);

            let match_start = |var: &Variable| lookup(var).map(|_| 0); // Placeholder implementation
            let match_end = |var: &Variable| lookup(var).map(|v| v.len()); // Placeholder implementation

            let line_start = |filepath: Option<&Path>, var: &Variable| {
                match_start(var).map(|_| "1".to_string()) // Placeholder implementation
            };
            let line_end = |filepath: Option<&Path>, var: &Variable| {
                match_end(var).map(|_| "1".to_string()) // Placeholder implementation
            };
            let column_start = |filepath: Option<&Path>, var: &Variable| {
                match_start(var).map(|_| "1".to_string()) // Placeholder implementation
            };
            let column_end = |filepath: Option<&Path>, var: &Variable| {
                match_end(var).map(|_| "1".to_string()) // Placeholder implementation
            };

            let offset_to_line_column = |_source: &str, _offset: usize| (1, 1); // Placeholder implementation

            let strip_var = |var: &Variable| {
                let re = Regex::new(r"^\[(.*?)\]$").unwrap();
                re.captures(var)
                    .and_then(|caps| caps.get(1).map(|m| m.as_str().to_string()))
            };

            match kind {
                AttributeKind::Value => lookup(variable),
                AttributeKind::Length => lookup(variable).map(|value| value.len().to_string()),
                AttributeKind::Lines => lookup(variable).map(|value| (value.matches('\n').count() + 1).to_string()),
                AttributeKind::OffsetStart => match_start(variable),
                AttributeKind::OffsetEnd => match_end(variable),
                AttributeKind::LineStart => line_start(filepath, variable),
                AttributeKind::LineEnd => line_end(filepath, variable),
                AttributeKind::ColumnStart => column_start(filepath, variable),
                AttributeKind::ColumnEnd => column_end(filepath, variable),
                AttributeKind::FilePath => filepath.map(|p| p.to_string_lossy().to_string()),
                AttributeKind::FileName => filepath.map(|p| p.file_name().unwrap().to_string_lossy().to_string()),
                AttributeKind::FileDirectory => filepath.map(|p| p.parent().unwrap().to_string_lossy().to_string()),
                AttributeKind::Lowercase => lookup(variable).map(|value| value.to_lowercase()),
                AttributeKind::Uppercase => lookup(variable).map(|value| value.to_uppercase())
            }
        }
    }
})