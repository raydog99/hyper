use std::marker::PhantomData;

struct LeftAdjoint<F, G, A>
where
    F: Functor,
    G: Functor,
{
    f: PhantomData<F>,
    g: PhantomData<G>,
    value: Box<dyn Fn(Box<dyn Fn(A) -> G::Output>) -> F::Output>,
}

trait Functor {
    type Output;
    fn fmap<B, F>(&self, f: F) -> Self::Output
    where
        F: Fn(B) -> B + 'static;
}

impl<F, G, A> Functor for LeftAdjoint<F, G, A>
where
    F: Functor,
    G: Functor,
{
    type Output = LeftAdjoint<F, G, B>;

    fn fmap<B, F>(&self, f: F) -> Self::Output
    where
        F: Fn(A) -> B + 'static,
    {
        LeftAdjoint {
            f: PhantomData,
            g: PhantomData,
            value: Box::new(move |g| {
                (self.value)(Box::new(move |a| g(Box::new(move || f(a)))))
            }),
        }
    }
}

impl<F, G, A> LeftAdjoint<F, G, A>
where
    F: Applicative,
    G: Applicative,
{
    fn apply<B>(self, other: LeftAdjoint<F, G, B>) -> LeftAdjoint<F, G, <G as Applicative>::Output>
    where
        A: Fn(B) -> <G as Applicative>::Output,
    {
        LeftAdjoint {
            f: PhantomData,
            g: PhantomData,
            value: Box::new(move |g| {
                let f = self.value(Box::new(move |f| other.value(Box::new(move |x| g(Box::new(move || f(x)))))));
                let g = self.g.apply(other.g);
                f.apply(g)
            }),
        }
    }

    fn pure(a: A) -> LeftAdjoint<F, G, A> {
        LeftAdjoint {
            f: PhantomData,
            g: PhantomData,
            value: Box::new(move |g| g(Box::new(move || a))),
        }
    }
}

trait Applicative: Functor {
    fn pure<A>(a: A) -> Self::Output;
    fn apply<A, B>(self, other: Self::Output) -> Self::Output
    where
        A: Fn(B) -> Self::Output;
}

struct Adjunction<F, U>
where
    F: Functor,
    U: Functor,
{
    left_adjunct: Box<dyn Fn(Box<dyn Fn(F::Output) -> U::Output>) -> U::Output>,
    right_adjunct: Box<dyn Fn(U::Output) -> F::Output>,
}

impl<F, U, A> LeftAdjoint<F, Identity<A>, A>
where
    F: Functor,
    U: Functor,
{
    fn adjoint_to_left_adjoint(adj: &Adjunction<F, U>, u: U::Output) -> LeftAdjoint<F, Identity<A>, A> {
        LeftAdjoint {
            f: PhantomData,
            g: PhantomData,
            value: Box::new(move |g| Identity((adj.right_adjunct)(u).fmap(|f| g(Box::new(move || f))))),
        }
    }

    fn left_adjoint_to_adjoint(self) -> Adjunction<F, U> {
        Adjunction {
            left_adjunct: Box::new(move |g| (self.value)(Box::new(move |f| g(Box::new(move |x| f(x)))))),
            right_adjunct: Box::new(|_| unimplemented!()),
        }
    }
}