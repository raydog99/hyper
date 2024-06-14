mod yoneda {
    pub struct Yoneda<F, A>(Box<dyn Fn(Box<dyn Fn(A) -> R>) -> F>);

    impl<F, A> Yoneda<F, A> {
        pub fn run_yoneda<R>(&self, k: Box<dyn Fn(A) -> R>) -> F {
            (self.0)(k)
        }
    }

    pub fn lift_yoneda<F, A>(fa: F) -> Yoneda<F, A>
    where
        F: Clone,
    {
        Yoneda(Box::new(move |k| {
            fa.clone().contramap(k)
        }))
    }

    pub fn lower_yoneda<F, A>(m: Yoneda<F, A>) -> F
    where
        F: Clone,
    {
        m.run_yoneda(Box::new(|x| x.clone()))
    }

    pub fn contramap<F, A, B>(g: Box<dyn Fn(B) -> A>, m: Yoneda<F, A>) -> Yoneda<F, B> {
        Yoneda(Box::new(move |k| m.run_yoneda(Box::new(move |x| k(g(x))))))
    }
}