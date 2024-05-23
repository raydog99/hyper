enum Term<T> {
    Var(usize),
    App(Box<Term<T>>, Box<Term<T>>),
    Lam(String, Box<Term<T>>),
}

impl<T: Clone> Term<T> {
    fn eval(&self, env: &Vec<(String, T)>) -> T {
        match self {
            Term::Var(n) => env[*n].1.clone(),
            Term::App(f, arg) => f.eval(env)(arg.eval(env)),
            Term::Lam(x, body) => Box::new(move |v| body.eval(&[(x.clone(), v)].into_iter().collect())),
        }
    }
}

pub fn fix<T>(f: fn(Term<T>) -> Term<T>) -> Term<T> {
    Term::Lam(
        "x".to_string(),
        Box::new(Term::App(
            Box::new(f(Term::Var(0))),
            Term::Var(0),
        )),
    )
}

pub fn id<T>() -> Box<dyn Fn(T) -> T> {
    Box::new(|x| x)
}

pub fn k<T>() -> Box<dyn Fn(T) -> Box<dyn Fn(T) -> T>> {
    Box::new(|x| Box::new(move |y| x))
}

pub fn b<T>() -> Box<dyn Fn(T) -> Box<dyn Fn(T) -> Box<dyn Fn(T) -> T>>> {
    Box::new(|x| Box::new(move |y| Box::new(move |z| y)))
}