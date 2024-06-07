use std::marker::PhantomData;

struct Identity<A>(A);

struct Adjunction<T, G> {
    unit: Box<dyn Fn(G) -> T>,
    right_adjoint: Box<dyn Fn(T) -> G>,
    left_adjunct: Box<dyn Fn(Box<dyn Fn(G) -> T>) -> Box<dyn Fn(T) -> T>>,
    right_adjunctor: Box<dyn Fn(Box<dyn Fn(T) -> G>) -> Box<dyn Fn(G) -> G>>,
}

struct Composition<T, G> {
    compose: Box<dyn Fn(T) -> Compose<T, G>>,
    decompose: Box<dyn Fn(Compose<T, G>) -> T>,
}

struct Compose<T, G>(T, PhantomData<G>);

impl<T, G> Compose<T, G> {
    fn map<A, F>(self, f: F) -> Compose<A, G>
    where
        F: Fn(T) -> A + 'static,
    {
        Compose(f(self.0), PhantomData)
    }
}

struct Ran<G, H, A> {
    un_ran: Box<dyn Fn(Box<dyn Fn(H) -> (G, A)>) -> A>,
    _phantom: PhantomData<(G, H, A)>,
}

impl<G, H, A> Ran<G, H, A> {
    fn map<B, F>(self, f: F) -> Ran<G, H, B>
    where
        F: Fn(A) -> B + 'static,
    {
        Ran {
            un_ran: Box::new(move |k| f((self.un_ran)(Box::new(move |b| k(b))))),
            _phantom: PhantomData,
        }
    }

    fn apply<B, F>(self, f: Ran<G, H, F>) -> Ran<G, H, B>
    where
        F: Fn(A) -> B + 'static,
    {
        Ran {
            un_ran: Box::new(move |k| {
                let (c1, f) = (f.un_ran)(Box::new(move |b| k(b)));
                let (c2, a) = (self.un_ran)(Box::new(move |b| k(b)));
                f(a)
            }),
            _phantom: PhantomData,
        }
    }
}

fn pure<G, H, A>(a: A) -> Ran<G, H, A> {
    Ran {
        un_ran: Box::new(move |_| a),
        _phantom: PhantomData,
    }
}

fn to_ran<G, H, T, A>(f: impl Fn(H) -> T, ran: Ran<G, H, A>) -> T {
    f((ran.un_ran)(Box::new(|b| (Identity(b), b))))
}

fn from_ran<G, H, T, A>(s: impl Fn(Ran<G, H, A>) -> T, h: H) -> T {
    s(Ran {
        un_ran: Box::new(move |k| {
            let (c, d) = k(h);
            d
        }),
        _phantom: PhantomData,
    })
}

fn adjoint_to_ran<G, T, A>(adj: Adjunction<T, G>, g: G) -> Ran<T, Identity<A>, A> {
    Ran {
        un_ran: Box::new(move |k| {
            let (c, d) = k(Identity((adj.unit)(g)));
            d
        }),
        _phantom: PhantomData,
    }
}

fn ran_to_adjoint<G, T, A>(adj: Adjunction<T, G>, ran: Ran<T, Identity<A>, A>) -> T {
    (adj.right_adjoint)(to_ran(|i| i.0, ran))
}

fn ran_to_composed_adjoint<G, T, H, A>(adj: Adjunction<T, G>, ran: Ran<T, H, A>) -> H {
    to_ran(
        |h| h.map(|t| (adj.right_adjoint)(t)),
        ran,
    )
}

fn composed_adjoint_to_ran<G, T, H, A>(adj: Adjunction<T, G>, h: H) -> Ran<T, H, A>
where
    H: Clone,
{
    Ran {
        un_ran: Box::new(move |k| {
            let (c, d) = k(h.clone().map(|g| (adj.unit)(g)));
            d
        }),
        _phantom: PhantomData,
    }
}

fn compose_ran<T, G, H, A>(
    comp: Composition<T, G>,
    ran: Ran<T, Ran<G, H, A>, A>,
) -> Ran<Compose<T, G>, H, A> {
    Ran {
        un_ran: Box::new(move |k| {
            ran.un_ran(Box::new(move |r| {
                let (c, d) = (r.un_ran)(Box::new(move |b| {
                    let (c, d) = k(b);
                    (c.map(Identity), d)
                }));
                ((comp.compose)(c.map(|i| i.0)), d)
            }))
        }),
        _phantom: PhantomData,
    }
}

fn decompose_ran<T, G, H, A>(
    comp: Composition<T, G>,
    ran: Ran<Compose<T, G>, H, A>,
) -> Ran<T, Ran<G, H, A>, A> {
    Ran {
        un_ran: Box::new(move |k| {
            ran.un_ran(Box::new(move |b| {
                let (c, d) = k(Ran {
                    un_ran: Box::new(move |k| {
                        let (c, d) = k(b);
                        (c.map(|c| (comp.decompose)(c)), d)
                    }),
                    _phantom: PhantomData,
                });
                (c, d)
            }))
        }),
        _phantom: PhantomData,
    }
}

fn gran<H, A>(h: H) -> Ran<Identity<A>, H, A> {
    Ran {
        un_ran: Box::new(move |k| {
            let (c, d) = k(h);
            d
        }),
        _phantom: PhantomData,
    }
}