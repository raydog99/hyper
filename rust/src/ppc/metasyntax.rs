use std::collections::HashSet;

#[derive(Debug, PartialEq, Eq, Hash)]
enum HoleType {
    Everything,
    Expression,
    Alphanum,
    NonSpace,
    Line,
    Blank,
}

enum Delimiter {
    Delimited(Option<String>, Option<String>),
    ReservedIdentifiers(HashSet<String>),
}

struct Hole {
    hole_type: HoleType,
    delimiter: Delimiter,
}

struct Regex {
    start: String,
    sep: char,
    end: String,
}

type Syntax = Vec<Either<Hole, Regex>>;

struct Alias {
    pattern: String,
    match_template: String,
    rule: Option<String>,
}

struct Metasyntax {
    syntax: Syntax,
    identifier: String,
    aliases: Vec<Alias>,
}

fn default_syntax() -> Syntax {
    let mut syntax = Vec::new();
    syntax.push(Either::Right(Regex {
        start: ":[".to_string(),
        sep: '~',
        end: "]".to_string(),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Everything,
        delimiter: Delimiter::Delimited(Some(":[".to_string()), Some("]".to_string())),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Expression,
        delimiter: Delimiter::Delimited(Some(":[".to_string()), Some(":e]".to_string())),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Alphanum,
        delimiter: Delimiter::Delimited(Some(":[[".to_string()), Some("]]".to_string())),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::NonSpace,
        delimiter: Delimiter::Delimited(Some(":[".to_string()), Some(".]".to_string())),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Line,
        delimiter: Delimiter::Delimited(Some(":[".to_string()), Some("\\n]".to_string())),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Blank,
        delimiter: Delimiter::Delimited(Some(":[ ".to_string()), Some("]".to_string())),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Expression,
        delimiter: Delimiter::ReservedIdentifiers(
            vec![
                "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ξ", "π", "ρ", "ς",
                "σ", "τ", "υ", "φ", "χ", "ψ", "ω",
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect(),
        ),
    }));
    syntax.push(Either::Left(Hole {
        hole_type: HoleType::Everything,
        delimiter: Delimiter::ReservedIdentifiers(
            vec!["Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω"]
                .into_iter()
                .map(|s| s.to_string())
                .collect(),
        ),
    }));
    syntax
}

fn default_identifier() -> String {
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_".to_string()
}

fn default_aliases() -> Vec<Alias> {
    vec![Alias {
        pattern: "...".to_string(),
        match_template: ":[_]".to_string(),
        rule: None,
    }]
}

fn default_metasyntax() -> Metasyntax {
    Metasyntax {
        syntax: default_syntax(),
        identifier: default_identifier(),
        aliases: default_aliases(),
    }
}

fn create(syntax: Syntax, identifier: String, aliases: Vec<Alias>) -> Metasyntax {
    Metasyntax {
        syntax,
        identifier,
        aliases,
    }
}

fn default() -> Metasyntax {
    create(default_syntax(), default_identifier(), default_aliases())
}