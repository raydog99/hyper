use regex::Regex;

enum RE {
    Str(String),
    Re(Regex),
}

enum Lens {
    Del(RE, String),
    Store(RE),
    Value(String),
    Counter(String),
    Seq(String),
    Key(RE),
    Label(String),
}

fn concat(l1: &Lens, l2: &Lens, input: &[String]) -> Vec<String> {
    let mut result = Vec::new();
    for s in input {
        let res1 = apply_lens(l1, s);
        for r1 in res1 {
            let res2 = apply_lens(l2, &r1);
            result.extend(res2);
        }
    }
    result
}

fn union(l1: &Lens, l2: &Lens, input: &str) -> String {
    let res1 = apply_lens(l1, input);
    if !res1.is_empty() {
        return res1.join("");
    }
    apply_lens(l2, input).join("")
}

fn repetition(op: &str, l: &Lens, input: &[String]) -> Vec<String> {
    let mut result = Vec::new();
    for s in input {
        let res = apply_lens(l, s);
        result.extend(res);
        match op {
            "*" => continue,
            "+" if res.is_empty() => return Vec::new(),
            "?" => break,
            _ => (),
        }
    }
    result
}

fn subtree(l: &Lens, input: &[String]) -> Vec<Vec<String>> {
    let mut result = Vec::new();
    for s in input {
        let res = apply_lens(l, s);
        result.push(res);
    }
    result
}

fn square(left: &Lens, body: &Lens, right: &Lens, input: &str) -> String {
    let res1 = apply_lens(left, input);
    if res1.is_empty() {
        return String::new();
    }
    let res2 = apply_lens(right, &res1[0]);
    if res2.is_empty() {
        return String::new();
    }
    let mid = &res2[0][res1[0].len()..];
    let mid_res = apply_lens(body, mid);
    let mut result = res1[0].clone();
    result.push_str(&mid_res.join(""));
    result.push_str(&res2[0][mid.len()..]);
    result
}