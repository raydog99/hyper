use std::collections::HashMap;

type Id = String;

enum Term<'a> {
    Var(&'a Id),
    Term(&'a Id, Vec<Term<'a>>),
}

type Substitution<'a> = HashMap<&'a Id, Term<'a>>;

fn occurs<'a>(x: &Id, t: &Term<'a>) -> bool {
    match t {
        Term::Var(y) => x == y,
        Term::Term(_, args) => args.iter().any(|arg| occurs(x, arg)),
    }
}

fn subst<'a>(s: &Term<'a>, x: &Id, t: &Term<'a>) -> Term<'a> {
    match t {
        Term::Var(y) if x == y => s.clone(),
        Term::Var(_) => t.clone(),
        Term::Term(f, args) => Term::Term(f, args.iter().map(|arg| subst(s, x, arg)).collect()),
    }
}

fn apply<'a>(s: &Substitution<'a>, t: &Term<'a>) -> Term<'a> {
    match t {
        Term::Var(x) => s.get(x).cloned().unwrap_or_else(|| t.clone()),
        Term::Term(f, args) => Term::Term(f, args.iter().map(|arg| apply(s, arg)).collect()),
    }
}

fn unify_one<'a>(s: &Term<'a>, t: &Term<'a>) -> Option<Substitution<'a>> {
    match (s, t) {
        (Term::Var(x), Term::Var(y)) if x == y => Some(HashMap::new()),
        (Term::Var(x), Term::Var(y)) => Some(vec![(y, Term::Var(x))].into_iter().collect()),
        (Term::Term(f, sc), Term::Term(g, tc)) if f == g && sc.len() == tc.len() => {
            let mut sub = HashMap::new();
            for (s, t) in sc.iter().zip(tc.iter()) {
                let s_sub = unify_one(&apply(&sub, s), &apply(&sub, t))?;
                sub.extend(s_sub);
            }
            Some(sub)
        }
        (Term::Var(x), t) if occurs(x, t) => None,
        (s, Term::Var(x)) if occurs(x, s) => None,
        (Term::Var(x), t) => Some(vec![(x, t.clone())].into_iter().collect()),
        (s, Term::Var(x)) => Some(vec![(x, s.clone())].into_iter().collect()),
        _ => None,
    }
}

fn unify<'a>(pairs: &[(Term<'a>, Term<'a>)]) -> Option<Substitution<'a>> {
    let mut sub = HashMap::new();
    for (s, t) in pairs {
        let s_sub = unify_one(&apply(&sub, s), &apply(&sub, t))?;
        sub.extend(s_sub);
    }
    Some(sub)
}