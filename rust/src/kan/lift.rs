use std::ops::Fn;

pub struct Lift<G, H, A>
where
    G: Clone,
{
    value: Box<dyn Fn(Box<dyn Fn(A) -> B>, G) -> H>,
}

impl<G, H, A, B, C> Lift<G, H, A>
where
    G: Clone,
{
    pub fn dimap<F, FG>(
        &self,
        f: F,
        g: impl Fn(A) -> C,
    ) -> Lift<G, H, C>
    where
        F: Fn(B) -> A + 'static,
        FG: Fn(G) -> G + 'static,
    {
        Lift {
            value: Box::new(move |bac, a| {
                self.value(Box::new(move |a| bac(g(a))), FG(a.clone()))
            }),
        }
    }

    pub fn divide<B>(&self, f: impl Fn(A) -> B + 'static, s: Lift<G, H, B>) -> Lift<G, H, C>
    where
        B: 'static,
        C: 'static,
    {
        Lift {
            value: Box::new(move |bac, a| {
                s.value(Box::new(move |b| bac(b)), self.value(Box::new(move |a| bac(f(a))), a.clone()))
            }),
        }
    }
}