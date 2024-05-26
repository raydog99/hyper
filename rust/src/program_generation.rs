use std::collections::HashMap;

type Ident = String;

enum Envcomp {
    Var(Ident),
    Lam(Ident, Box<Envcomp>),
    App(Box<Envcomp>, Box<Envcomp>),
    LamD(Ident, Box<Envcomp>),
    AppD(Box<Envcomp>, Box<Envcomp>),
}

enum Exp {
    Var'(Ident),
    Lam'(Ident, Box<Exp>),
    App'(Box<Exp>, Box<Exp>),
    LamD'(Ident, Box<Exp>),
    AppD'(Box<Exp>, Box<Exp>),
}

enum Value {
    Fun(Box<dyn Fn(Value) -> Value>),
    Code(Box<Exp>),
}

fn spec(e: &Exp, r: &mut HashMap<Ident, Value>) -> Value {
    match e {
        Exp::Var'(x) => r.get(x).cloned().unwrap_or(Value::Code(Box::new(Exp::Var'(x.clone())))),
        Exp::Lam'(x, e) => Value::Fun(Box::new(move |y| {
            r.insert(x.clone(), y);
            spec(e, r)
        })),
        Exp::App'(f, a) => match (spec(f, r), spec(a, r)) {
            (Value::Fun(ff), a) => ff(a),
            _ => panic!("Type error"),
        },
        Exp::LamD'(x, e) => {
            let xx = gensym();
            r.insert(x.clone(), Value::Code(Box::new(Exp::Var'(xx.clone()))));
            let body = spec(e, r);
            Value::Code(Box::new(Exp::Lam'(xx, Box::new(match body {
                Value::Code(e) => *e,
                _ => panic!("Type error"),
            }))))
        }
        Exp::AppD'(f, a) => match (spec(f, r), spec(a, r)) {
            (Value::Code(ff), Value::Code(aa)) => Value::Code(Box::new(Exp::App'(ff, aa))),
            _ => panic!("Type error"),
        },
    }
}