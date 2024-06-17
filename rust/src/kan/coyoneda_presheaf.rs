use std::ops::Deref;

struct Coyoneda<F, A>
where
    F: Fn(A) -> B,
    A: ?Sized,
    B: ?Sized,
{
    run_coyoneda: Box<dyn Fn(F) -> Box<dyn Deref<Target = F>>>,
}

impl<F, A, B> Coyoneda<F, A>
where
    F: Fn(A) -> B + 'static,
    A: ?Sized,
    B: ?Sized,
{
    fn contramap<C>(self, f: impl Fn(C) -> A + 'static) -> Coyoneda<impl Fn(C) -> B + 'static, C>
    where
        C: ?Sized,
    {
        let run_coyoneda = self.run_coyoneda;
        Coyoneda {
            run_coyoneda: Box::new(move |k| {
                run_coyoneda(Box::new(move |a| k(f(a))))
            }),
        }
    }
}

impl<F, A> Coyoneda<F, A>
where
    F: Fn(A) -> A + 'static,
    A: ?Sized,
{
    fn lift(x: F) -> Coyoneda<F, A> {
        Coyoneda {
            run_coyoneda: Box::new(move |f| Box::new(f(x))),
        }
    }

    fn lower(self) -> F {
        let run_coyoneda = self.run_coyoneda;
        run_coyoneda(Box::new(|a| a))
    }
}

impl<F, G, A> Coyoneda<F, A>
where
    F: Fn(A) -> B + 'static,
    G: Fn(F) -> Box<dyn Deref<Target = G>> + 'static,
    A: ?Sized,
    B: ?Sized,
{
    fn hoist(self, f: G) -> Coyoneda<impl Fn(A) -> B + 'static, A> {
        let run_coyoneda = self.run_coyoneda;
        Coyoneda {
            run_coyoneda: Box::new(move |k| f(run_coyoneda(Box::new(k)))),
        }
    }
}

type Presheaf<A> = Box<dyn Fn(A) -> i32>;

fn contramap<A, B>(f: impl Fn(B) -> A + 'static, g: Presheaf<A>) -> Presheaf<B>
where
    A: ?Sized,
    B: ?Sized,
{
    Box::new(move |b| g(f(b)))
}

fn tabulate<A>(a: A) -> Presheaf<A>
where
    A: ?Sized,
{
    Box::new(move |_| 0)
}