use std::marker::PhantomData;

struct LanF<F, G, A>(PhantomData<(F, G, fn(F, A) -> A)>);

impl<F, G, A> LanF<F, G, A> {
    fn fmap<B, FuncA, FuncB>(self, f: FuncA) -> LanF<F, G, B>
    where
        FuncA: Fn(A) -> B,
        FuncB: Fn(fn(F, A) -> A, G) -> B,
    {
        LanF(PhantomData, f)
    }

    fn apply<B, FuncA, FuncB, FuncC>(
        self,
        f: LanF<F, G, impl Fn(A) -> B>,
        x: LanF<F, G, A>,
    ) -> LanF<F, G, B>
    where
        FuncA: Fn(fn(F, impl Fn(A) -> B) -> impl Fn(A) -> B, G) -> impl Fn(A) -> B,
        FuncB: Fn(fn(F, A) -> A, G) -> A,
        FuncC: Fn(fn(F, B) -> B, G) -> B,
    {
        LanF(PhantomData, |k, g| {
            f.0(|f, r| r(k(f)), g)(x.0(k, g))
        })
    }

    fn pure(a: A) -> LanF<F, G, A> {
        LanF(PhantomData, |_, _| a)
    }

    fn to_lan<T, FuncA, FuncB>(self, phi: FuncA) -> T
    where
        FuncA: Fn(G) -> T,
        FuncB: Fn(fn(F, A) -> A, G) -> T,
    {
        self.0(|f, a| phi(f, a), std::marker::PhantomData)
    }

    fn from_lan<T, FuncA>(self, phi: FuncA) -> T
    where
        FuncA: Fn(LanF<F, G, A>) -> T,
    {
        phi(LanF(PhantomData, |k, g| k(std::marker::PhantomData, g)))
    }
}