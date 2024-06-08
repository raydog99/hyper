struct Codensity<'a, M, A>(Box<dyn 'a + Fn(Box<dyn 'a + Fn(A) -> M<B>>) -> M<B>>);

impl<'a, M, A> Codensity<'a, M, A>
where
    M: Monad<A>,
{
    fn return_(x: A) -> Codensity<'a, M, A> {
        Codensity(Box::new(move |k| k(x)))
    }

    fn bind<B, F>(self, f: F) -> Codensity<'a, M, B>
    where
        F: 'a + Fn(A) -> Codensity<'a, M, B>,
    {
        Codensity(Box::new(move |k| {
            (self.0)(Box::new(move |a| {
                let codensity = f(a);
                (codensity.0)(k)
            }))
        }))
    }

    fn map<B, F>(self, f: F) -> Codensity<'a, M, B>
    where
        F: 'a + Fn(A) -> B,
    {
        Codensity(Box::new(move |k| {
            (self.0)(Box::new(move |a| k(f(a))))
        }))
    }

    fn lower_codensity(self) -> M<A> {
        (self.0)(Box::new(|a| M::return_(a)))
    }
}

fn reset<'a, M, A>(m: Codensity<'a, M, A>) -> M<A>
where
    M: Monad<A>,
{
    m.lower_codensity()
}

fn shift<'a, M, A, B>(f: impl 'a + Fn(Box<dyn 'a + Fn(A) -> Codensity<'a, M, B>>) -> Codensity<'a, M, A>) -> Codensity<'a, M, A>
where
    M: Monad<A>,
    M: Monad<B>,
{
    let aux = move |k| {
        let codensity = f(Box::new(move |a| Codensity::return_(a)));
        (codensity.0)(k)
    };
    Codensity(Box::new(move |k| aux(k)))
}