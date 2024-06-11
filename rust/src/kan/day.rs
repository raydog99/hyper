use std::ops::{Apply, Deref};

struct Day<F, G, A>(F, G, Box<dyn Fn(F, G) -> A>);

impl<F, G, A> Day<F, G, A> {
    fn fmap<B>(self, f: impl Fn(A) -> B) -> Day<F, G, B>
    where
        F: Functor,
        G: Functor,
    {
        Day(
            self.0.fmap(|_| ()),
            self.1.fmap(|_| ()),
            Box::new(move |f, g| f(self.2(f, g))),
        )
    }
}

impl<F, G, A> Applicative for Day<F, G, A>
where
    F: Functor,
    G: Functor,
{
    fn pure(a: A) -> Day<F, G, A> {
        Day(F::pure(()), G::pure(()), Box::new(move |_, _| a))
    }

    fn ap(self, other: Day<F, G, A>) -> Day<F, G, A> {
        Day(
            (self.0).apply(other.0),
            (self.1).apply(other.1),
            Box::new(move |f, g| (self.2)(f, (other.2)(f, g))),
        )
    }

impl<F, G, H, A> Day<F, Day<G, H, A>, A> {
    fn assoc(self) -> Day<Day<F, G, A>, H, A>
    where
        F: Functor,
        G: Functor,
        H: Functor,
    {
        Day(
            Day(self.0 .0, self.0 .1 .0, Box::new(|f, g| (self.0 .2)(f, g))),
            self.0 .1 .1,
            Box::new(move |fg, h| (self.0 .1 .2)(fg.1, h)((fg.2)(fg.0, fg.1))),
        )
    }

    fn disassoc(self) -> Day<F, Day<G, H, A>, A>
    where
        F: Functor,
        G: Functor,
        H: Functor,
    {
        Day(
            self.0 .0,
            Day(self.0 .1 .0, self.0 .1 .1, Box::new(|g, h| (self.0 .1 .2)(g, h))),
            Box::new(move |f, gh| (self.0 .2)(f, gh.0)(gh.2)(gh.0, gh.1)),
        )
    }
}

impl<F, G, A> Day<F, G, A> {
    fn swapped(self) -> Day<G, F, A> {
        Day(self.1, self.0, Box::new(move |g, f| (self.2)(f, g)))
    }
}

impl<F, A> Day<(), F, A> {
    fn intro1(f: F) -> Day<(), F, A> {
        Day((), f, Box::new(|_, f| f))
    }
}

impl<G, A> Day<G, (), A> {
    fn intro2(g: G) -> Day<G, (), A> {
        Day(g, (), Box::new(|g, _| g))
    }
}

impl<F, A> Day<(), F, A> {
    fn elim1(self) -> F {
        self.1
    }
}

impl<G, A> Day<G, (), A> {
    fn elim2(self) -> G {
        self.0
    }
}

impl<F, G, A, B> Day<F, G, A> {
    fn trans1(self, f: impl Fn(A) -> B) -> Day<F, G, B>
    where
        F: Functor,
        G: Functor,
    {
        Day(self.0.fmap(|_| ()), self.1.fmap(|_| ()), Box::new(move |f, g| f((self.2)(f, g))))
    }

    fn trans2(self, g: impl Fn(G) -> G2) -> Day<F, G2, A>
    where
        F: Functor,
        G: Functor<Arr = G2>,
    {
        Day(self.0, g(self.1), Box::new(move |f, g2| (self.2)(f, g2)))
    }
}