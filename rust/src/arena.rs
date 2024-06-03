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