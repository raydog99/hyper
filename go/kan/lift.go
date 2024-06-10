package capitan

type Lift[G, H, A any] struct {
    value func(func(A) B, G) H
}

func (l Lift[G, H, A]) Dimap[B, C any](f func(B) A, g func(A) C) Lift[G, H, C] {
    return Lift[G, H, C]{
        value: func(bac func(C) B, a G) H {
            return l.value(func(a A) B {
                return bac(g(a))
            }, f(a))
        },
    }
}

func (l Lift[G, H, A]) Divide[B any](f func(A) B, s Lift[G, H, B]) Lift[G, H, C] {
    return Lift[G, H, C]{
        value: func(bac func(C) B, a G) H {
            return s.value(func(b B) B {
                return bac(b)
            }, l.value(func(a A) B {
                return bac(f(a))
            }, a))
        },
    }
}