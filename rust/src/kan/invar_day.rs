pub mod invar_day {
    use std::any::Any;

    pub struct Day<F, G, A> {
        pub fb: F,
        pub gc: G,
        pub bca: Box<dyn Fn(Box<dyn Any>, Box<dyn Any>) -> A>,
        pub abc: Box<dyn Fn(A) -> (Box<dyn Any>, Box<dyn Any>)>,
    }

    impl<F, G, A, B, C> Day<F, G, A>
    where
        F: Clone,
        G: Clone,
        A: Clone,
        B: 'static,
        C: 'static,
    {
        pub fn day(fa: F, gb: G) -> Day<F, G, (B, C)> {
            Day {
                fb: fa,
                gc: gb,
                bca: Box::new(|b, c| (b.downcast::<B>().unwrap(), c.downcast::<C>().unwrap())),
                abc: Box::new(|a| {
                    let (b, c) = a.clone();
                    (Box::new(b), Box::new(c))
                }),
            }
        }

        pub fn invmap<B, D>(
            self,
            f: impl Fn(A) -> B + 'static,
            g: impl Fn(C) -> D + 'static,
        ) -> Day<F, G, B> {
            Day {
                fb: self.fb,
                gc: self.gc,
                bca: Box::new(move |b, c| {
                    f((self.bca)(b, c.downcast::<C>().unwrap()))
                }),
                abc: Box::new(move |a| {
                    let (b, c) = (self.abc)(a.clone());
                    (b, Box::new(g(c.downcast::<C>().unwrap())))
                }),
            }
        }

        pub fn assoc<H, D, E>(self) -> Day<Day<F, G, D>, H, A>
        where
            H: Clone,
            D: 'static,
            E: 'static,
        {
            let f = move |a| {
                let (b, c) = (self.abc)(a.clone());
                let (d, e) = (self.gc.abc)(c.downcast::<E>().unwrap());
                (
                    Day {
                        fb: self.fb.clone(),
                        gc: d,
                        bca: self.bca.clone(),
                        abc: Box::new(move |a| (b.clone(), a)),
                    },
                    e,
                )
            };
            let g = move |bc, e| {
                (self.bca)(
                    bc.fb,
                    (bc.gc.bca)(bc.gc.gc, e.downcast::<E>().unwrap()),
                )
            };
            Day {
                fb: Day {
                    fb: self.fb,
                    gc: self.gc.gc,
                    bca: Box::new(move |b, c| b),
                    abc: Box::new(move |c| (c, Box::new(()))),
                },
                gc: self.gc.gc,
                bca: Box::new(g),
                abc: Box::new(f),
            }
        }

        pub fn disassoc<H, B, D>(self) -> Day<F, Day<G, H, A>, B>
        where
            H: Clone,
            B: 'static,
            D: 'static,
        {
            let f = move |b, day: Day<G, H, A>| {
                (day.bca)(
                    (self.fb.bca)(b.downcast::<B>().unwrap(), day.gc),
                    day.abc,
                )
            };
            let g = move |b| {
                let (a, c) = (self.abc)((self.bca)(self.fb.abc.downcast::<B>().unwrap(), self.gc));
                (b.downcast::<B>().unwrap(), Day {
                    fb: self.fb.gc,
                    gc: c,
                    bca: self.fb.bca.clone(),
                    abc: a,
                })
            };
            Day {
                fb: self.fb.fb,
                gc: Day {
                    fb: self.fb.gc,
                    gc: self.gc,
                    bca: self.fb.bca.clone(),
                    abc: self.abc.clone(),
                },
                bca: Box::new(f),
                abc: Box::new(g),
            }
        }

        pub fn swapped(self) -> Day<G, F, A> {
            Day {
                fb: self.gc,
                gc: self.fb,
                bca: Box::new(move |c, b| (self.bca)(b, c.downcast::<C>().unwrap())),
                abc: Box::new(move |a| {
                    let (b, c) = (self.abc)(a.clone());
                    (c, b)
                }),
            }
        }

        pub fn intro1<B>(self, fa: F) -> Day<(), F, B>
        where
            B: 'static,
        {
            Day {
                fb: (),
                gc: fa,
                bca: Box::new(move |_, a| a.downcast::<B>().unwrap()),
                abc: Box::new(move |a| (Box::new(()), a)),
            }
        }

        pub fn intro2<B>(self, fa: F) -> Day<F, (), B>
        where
            B: 'static,
        {
            Day {
                fb: fa,
                gc: (),
                bca: Box::new(move |a, _| a.downcast::<B>().unwrap()),
                abc: Box::new(move |a| (a, Box::new(()))),
            }
        }

        pub fn elim1(self) -> F
        where
            A: 'static,
        {
            self.invmap(
                |a| (self.bca)(Box::new(()), a.downcast::<A>().unwrap()),
                |a| a,
            )
            .gc
        }

        pub fn elim2(self) -> F
        where
            A: 'static,
        {
            self.invmap(
                |a| (self.bca)(a, Box::new(())),
                |_| (),
            )
            .fb
        }

        pub fn trans1<H, B>(self, fg: impl Fn(F) -> H + 'static) -> Day<H, G, A>
        where
            H: 'static,
        {
            Day {
                fb: fg(self.fb),
                gc: self.gc,
                bca: self.bca,
                abc: self.abc,
            }
        }

        pub fn trans2<H, B>(self, gh: impl Fn(G) -> H + 'static) -> Day<F, H, A>
        where
            H: 'static,
        {
            Day {
                fb: self.fb,
                gc: gh(self.gc),
                bca: self.bca,
                abc: self.abc,
            }
        }
    }

    pub struct Contravariant<F, G, A> {
        pub fb: F,
        pub gc: G,
        pub abc: Box<dyn Fn(A) -> (Box<dyn Any>, Box<dyn Any>)>,
    }

    impl<F, G, A> Day<F, G, A> {
        pub fn to_contravariant(self) -> Contravariant<F, G, A> {
            Contravariant {
                fb: self.fb,
                gc: self.gc,
                abc: self.abc,
            }
        }
    }

    pub struct Covariant<F, G, A> {
        pub fb: F,
        pub gc: G,
        pub bca: Box<dyn Fn(Box<dyn Any>, Box<dyn Any>) -> A>,
        }

    impl<F, G, A> Day<F, G, A> {
        pub fn to_covariant(self) -> Covariant<F, G, A> {
            Covariant {
                fb: self.fb,
                gc: self.gc,
                bca: self.bca,
            }
        }
    }
}