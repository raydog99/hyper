struct Rift<G, H, A>(Box<dyn Fn(Box<dyn Fn(B) -> (A, C)>, G) -> H>);

impl<G, H, A, B, C> Rift<G, H, A>
where
    G: Clone,
    H: Clone,
{
    fn contramap<F>(self, f: F) -> Rift<G, H, B>
    where
        F: Fn(B) -> A + 'static,
    {
        Rift(Box::new(move |bac: Box<dyn Fn(B1) -> (B, C)>, g: G| {
            (self.0)(Box::new(move |b: B1| {
                let (a, c) = bac(b);
                (f(a), c)
            }), g.clone())
        }))
    }
}

impl<G, A, B, C> Rift<G, G, A>
where
    G: Clone,
{
    fn divide<F>(f: F, g: Rift<G, G, A>, h: Rift<G, G, B>) -> Rift<G, G, C>
    where
        F: Fn(A) -> B + 'static,
    {
        Rift(Box::new(move |bac: Box<dyn Fn(B1) -> (C, D)>, g: G| {
            g.0(Box::new(move |a: B1| {
                let (c, d) = bac(a);
                (f(c), d)
            }), g.clone())
        }))
    }
}