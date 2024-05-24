type Pu<A> = Box<dyn FnOnce(A)>;

pub fn pair<A, B>(pa: Pu<A>, pb: Pu<B>) -> Pu<(A, B)> {
    Box::new(move |(a, b)| {
        pa(a);
        pb(b);
    })
}

pub fn triple<A, B, C>(pa: Pu<A>, pb: Pu<B>, pc: Pu<C>) -> Pu<(A, B, C)> {
    Box::new(move |(a, b, c)| {
        pa(a);
        pb(b);
        pc(c);
    })
}

pub fn quad<A, B, C, D>(pa: Pu<A>, pb: Pu<B>, pc: Pu<C>, pd: Pu<D>) -> Pu<(A, B, C, D)> {
    Box::new(move |(a, b, c, d)| {
        pa(a);
        pb(b);
        pc(c);
        pd(d);
    })
}

pub fn wrap<A, B>(i: impl Fn(A) -> B + 'static, j: impl Fn(B) -> A + 'static) -> Pu<B>
where
    A: 'static,
    B: 'static,
{
    Box::new(move |b| {
        let a = j(b);
        let b = i(a);
    })
}

pub fn zero_to(n: u32) -> Pu<u32> {
    if n == 0 {
        Box::new(|_| {})
    } else {
        wrap(
            |i| i / 256,
            |i| i % 256,
        )(zero_to(n / 256))
    }
}

pub fn unit<A>() -> Pu<A> {
    Box::new(|_| {})
}

pub fn char() -> Pu<char> {
    wrap(
        |c| c as u32 as char,
        |r| r as u32,
    )(zero_to(255))
}

pub fn bool() -> Pu<bool> {
    wrap(
        |b| if b { 1 } else { 0 },
        |i| i != 0,
    )(zero_to(1))
}

pub fn nat() -> Pu<u32> {
    let half = 128;
    Box::new(move |n| {
        if n < half {
            unit()(n);
        } else {
            wrap(
                |i| half + i % half,
                |i| (i - half) / half,
            )(nat())(n);
        }
    })
}

pub fn fixed_list<A>(pa: Pu<A>, n: usize) -> Pu<Vec<A>> {
    if n == 0 {
        Box::new(|_| Vec::new())
    } else {
        wrap(
            |p: (A, Vec<A>)| {
                let mut v = p.1;
                v.insert(0, p.0);
                v
            },
            |mut v: Vec<A>| (v.remove(0), v),
        )(pair(pa, fixed_list(pa, n - 1)))
    }
}

pub fn list<A>(pa: Pu<A>) -> Pu<Vec<A>> {
    Box::new(move |v| {
        let n = v.len();
        nat()(n as u32);
        fixed_list(pa, n)(v);
    })
}

pub fn string() -> Pu<String> {
    list(char())
}

pub fn alt<A>(tag: impl Fn(&A) -> usize + 'static, ps: Vec<Pu<A>>) -> Pu<A> {
    if ps.is_empty() {
        panic!("alt: empty list");
    }
    Box::new(move |a| {
        let n = tag(&a);
        ps[n](a);
    })
}

pub fn p_maybe<A>(pa: Pu<A>) -> Pu<Option<A>> {
    alt(
        |p: &Option<A>| p.is_none() as usize,
        vec![
            Box::new(|_| None),
            wrap(
                |a| Some(a),
                |o| o.unwrap(),
            )(pa),
        ],
    )
}

pub fn p_either<A, B>(pa: Pu<A>, pb: Pu<B>) -> Pu<Either<A, B>> {
    alt(
        |e: &Either<A, B>| e.is_left() as usize,
        vec![
            wrap(
                |a| Either::Left(a),
                |e| match e {
                    Either::Left(a) => a,
                    Either::Right(_) => panic!("impossible"),
                },
            )(pa),
            wrap(
                |b| Either::Right(b),
                |e| match e {
                    Either::Left(_) => panic!("impossible"),
                    Either::Right(b) => b,
                },
            )(pb),
        ],
    )
}