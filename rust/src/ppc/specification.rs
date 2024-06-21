pub mod template_match {
    use regex::escape;

    pub struct Rule;

    pub struct T {
        pub match_template: String,
        pub rule: Option<Rule>,
        pub rewrite_template: Option<String>,
    }

    impl T {
        pub fn create(rewrite_template: Option<String>, rule: Option<Rule>, match_template: String) -> Self {
            T {
                match_template,
                rule,
                rewrite_template,
            }
        }
    }

    enum Extracted {
        Regex(String),
        ContiguousWhitespace(String),
        NonSpace(String),
    }

    impl Extracted {
        fn to_regex(&self) -> String {
            match self {
                Extracted::Regex(s) => s.clone(),
                Extracted::ContiguousWhitespace(_) => "\\s+".to_string(),
                Extracted::NonSpace(s) => escape(s),
            }
        }
    }

    pub fn to_regex(t: &T) -> String {
        let extracted = extract(&t.match_template);
        let regex_parts: Vec<String> = extracted.iter().map(|e| e.to_regex()).collect();
        format!("({})", regex_parts.join(""))
    }

    fn extract(template: &str) -> Vec<Extracted> {
        vec![]
    }
}