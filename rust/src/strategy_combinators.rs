use std::marker::PhantomData;

struct TP<M, A>(Box<dyn Fn(M) -> A>);

struct TU<M, A>(Box<dyn Fn(M) -> A>);

// Strategy application
impl<M, A> TP<M, A> {
    fn apply_tp(&self, m: M) -> A {
        (self.0)(m)
    }
}

impl<M, A> TU<M, A> {
    fn apply_tu(&self, m: M) -> A {
        (self.1)(m)
    }
}

// Strategy construction
pub fn poly_tp<M, A>(m: M) -> TP<M, A>
where
    M: Clone,
{
    TP(Box::new(move |x| x.clone()))
}

pub fn poly_tu<M, A>(m: M) -> TU<M, A>
where
    M: Clone,
{
    TU(Box::new(move |x| x.clone()))
}

pub fn adhoc_tp<M, A, B>(m: M, t: A) -> TP<M, B>
where
    M: Fn(A) -> B + 'static,
    A: Clone,
{
    TP(Box::new(move |x| m(x.clone())))
}

pub fn adhoc_tu<M, A, B>(m: M, t: A) -> TU<M, B>
where
    M: Fn(A) -> B + 'static,
    A: Clone,
{
    TU(Box::new(move |x| m(x.clone())))
}

// Sequential composition
pub fn seq_tp<M, A>(m: TP<M, TP<M, A>>) -> TP<M, A> {
    TP(Box::new(move |x| m.apply_tp(x).apply_tp(x)))
}

pub fn let_tp<M, A>(m: TU<M, TP<M, A>>) -> TP<M, A> {
    TP(Box::new(move |x| m.apply_tu(x).apply_tp(x)))
}

pub fn seq_tu<M, A>(m: TP<M, TU<M, A>>) -> TU<M, A> {
    TU(Box::new(move |x| m.apply_tp(x).apply_tu(x)))
}

pub fn let_tu<M, A>(m: TU<M, TU<M, A>>) -> TU<M, A> {
    TU(Box::new(move |x| {
        let f = m.apply_tu(x);
        f.apply_tu(x)
    }))
}

// Choice
pub fn choice_tp<M, A>(m: M) -> TP<M, A>
where
    M: Fn() -> TP<M, A> + 'static,
{
    TP(Box::new(move |x| m().apply_tp(x)))
}

pub fn choice_tu<M, A>(m: M) -> TU<M, A>
where
    M: Fn() -> TU<M, A> + 'static,
{
    TU(Box::new(move |x| m().apply_tu(x)))
}

// Traversal combinators
pub fn all_tp<M, A>(m: M) -> TP<M, A>
where
    M: Clone,
{
    TP(Box::new(move |x| x.clone()))
}

pub fn one_tp<M, A>(m: M) -> TP<M, A>
where
    M: Clone,
{
    TP(Box::new(move |x| x.clone()))
}

pub fn all_tu<M, A>(m: M, a: A) -> TU<M, A>
where
    A: Clone,
{
    TU(Box::new(move |_| a.clone()))
}

pub fn one_tu<M, A>(m: M) -> TU<M, A>
where
    M: Clone,
{
    TU(Box::new(move |_| m.clone()))
}