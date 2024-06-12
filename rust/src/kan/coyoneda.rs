struct Coyoneda<F, A>
where
    F: Contravariant,
{
    f: Box<dyn Fn(A) -> F::Res>,
    m: F::Res,
}

impl<F, A> Coyoneda<F, A>
where
    F: Contravariant,
{
    fn new(f: Box<dyn Fn(A) -> F::Res>, m: F::Res) -> Self {
        Coyoneda { f, m }
    }

    fn contramap<B>(self, g: Box<dyn Fn(B) -> A>) -> Coyoneda<F, B> {
        Coyoneda {
            f: Box::new(move |x| self.f(g(x))),
            m: self.m,
        }
    }
}

trait Contravariant {
    type Res;
    fn contramap<A, B>(self, f: Box<dyn Fn(B) -> A>) -> Self::Res;
}