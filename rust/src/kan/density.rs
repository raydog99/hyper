pub struct Density<A>(Box<dyn Fn(Box<dyn Fn(A) -> B>) -> B + 'static>);

impl<A> Density<A> {
    pub fn return_(a: A) -> Density<A> {
        Density(Box::new(move |_| a))
    }

    pub fn bind<B, F>(self, k: F) -> Density<B>
    where
        F: Fn(A) -> Density<B> + 'static,
    {
        let f = self.0;
        Density(Box::new(move |g| {
            let Density(h) = k(f(Box::new(|x| x)));
            h(g)
        }))
    }

    pub fn map<B, F>(self, f: F) -> Density<B>
    where
        F: Fn(A) -> B + 'static,
    {
        let f = self.0;
        Density(Box::new(move |g| {
            let h = move |x| g(f(Box::new(move |y| f(y))));
            h(Box::new(|x| x))
        }))
    }

    pub fn extract(self) -> A {
        self.0(Box::new(|x| x))
    }

    pub fn duplicate(self) -> Density<Density<A>> {
        let f = self.0;
        Density(Box::new(move |g| {
            let h = move |x| g(Density(Box::new(move |k| f(k)(x))));
            h(Box::new(|x| x))
        }))
    }

    pub fn extend<B, F>(self, f: F) -> Density<B>
    where
        F: Fn(Density<A>) -> B + 'static,
    {
        let f = self.0;
        Density(Box::new(move |g| {
            let h = move |x| g(f(Density(Box::new(move |k| k(x)))));
            h(Box::new(|x| x))
        }))
    }
}