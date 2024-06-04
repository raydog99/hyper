use std::ops::{Deref, DerefMut};

struct Arena<Pos, Dis> {
    pos: Pos,
    dis: Box<dyn Fn(Pos) -> Dis>,
}

struct Lens<Dom, Cod> {
    observe: Box<dyn Fn(Dom) -> Cod>,
    interpret: Box<dyn Fn(Dom, Box<dyn Fn(Cod) -> Cod>) -> Box<dyn Fn(Dom) -> Dom>>,
}

impl<A> Lens<A, A> {
    fn id_lens(a: Arena<A, impl Fn(A) -> A>) -> Self {
        Lens {
            observe: Box::new(|x| x),
            interpret: Box::new(|p, f| Box::new(move |x| f(x)(x))),
        }
    }
}

impl<A1, A2, A3> Lens<A2, A3> {
    fn compose<B2, B3>(self, lens12: Lens<A1, A2>) -> Lens<A1, A3>
    where
        A2: Clone,
        A3: Clone,
        B2: 'static,
        B3: 'static,
    {
        let observe = move |p| self.observe(lens12.observe(p.clone()));
        let interpret = move |p, f| {
            lens12.interpret(
                p,
                Box::new(move |x| {
                    self.interpret(x.clone(), Box::new(move |y| f(y.clone())))
                }),
            )
        };
        Lens {
            observe: Box::new(observe),
            interpret: Box::new(interpret),
        }
    }
}

type Display<Pos, Dis> = (Pos, Box<dyn Fn(Pos) -> Dis>);
type AsFunctor<Pos, Dis, Res> = (Pos, Box<dyn Fn(Box<dyn Fn(Pos) -> Dis>) -> Res>);

struct Lens<A, B> {
    o: Box<dyn Fn(A) -> B>,
    i: Box<dyn Fn(B, Dis<B>) -> Dis<A>>,
}

impl<A, B> Lens<A, B> {
    fn duoidal(a1: Arena, a2: Arena, b1: Arena, b2: Arena) -> Lens<(Arena, Arena), (Arena, Arena)> {
        let x = (a1.combine(&a2), b1.combine(&b2));
        let y = (a1.pair(&b1), a2.pair(&b2).combine());

        let o = Box::new(
            |(p1, p2), (q1, q2)| {
                let pp = (
                    (p1, q1),
                    |d: &Dis<(Arena, Arena)>| {
                        (p2.apply(d.0.deref()), q2.apply(d.1.deref()))
                    },
                );
                pp
            },
        );

        let i = Box::new(
            |(p1, p2), (q1, q2), (de1, de2): Dis<(Arena, Arena)>| {
                (
                    (de1.0.deref().clone(), de2.0.deref().clone()),
                    (de1.1.deref().clone(), de2.1.deref().clone()),
                )
            },
        );

        Lens { o, i }
    }
}