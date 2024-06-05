package capitan

type LanF[F, G, A any] func(func(F, A) A, G) A

func (l LanF[F, G, A]) FMap[B any](f func(A) B) LanF[F, G, B] {
    return func(k func(F, B) B, g G) B {
        return f(l(func(f F, A) A { return k(f, a) }, g))
    }
}

func (l LanF[F, G, A]) Apply[B any](f LanF[F, G, func(A) B], x LanF[F, G, A]) LanF[F, G, B] {
    return func(k func(F, B) B, g G) B {
        return f(func(f F, r func(A) B) func(A) B { return func(a A) B { return r(k(f, a)) } }, g)(l(func(f F, a A) A { return k(f, a) }, g))
    }
}

func Pure[F, G, A any](a A) LanF[F, G, A] {
    return func(_ func(F, A) A, _ G) A { return a }
}

func (l LanF[F, G, A]) ToLan[T any](phi func(G, T) T) T {
    return l(func(f F, a A) A { return phi(f, a) }, id[G]())
}

func (l LanF[F, G, A]) FromLan[T any](phi func(LanF[F, G, A], T) T) T {
    return phi(l, id[T]())
}