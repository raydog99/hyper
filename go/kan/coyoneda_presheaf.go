package capitan

type Coyoneda[F any, A any] struct {
    runCoyoneda func(f func(A) B) F
}

func contramap[F any, A any, B any](f func(B) A, c Coyoneda[F, A]) Coyoneda[F, B] {
    return Coyoneda[F, B]{
        runCoyoneda: func(k func(B) C) F {
            return c.runCoyoneda(func(a A) C {
                return k(f(a))
            })
        },
    }
}

func lift[F any, A any](x F) Coyoneda[F, A] {
    return Coyoneda[F, A]{
        runCoyoneda: func(f func(A) B) F {
            return x
        },
    }
}

func lower[F any, A any](c Coyoneda[F, A]) F {
    return c.runCoyoneda(func(a A) A { return a })
}

func hoist[F any, G any, A any](f func(F) G, c Coyoneda[F, A]) Coyoneda[G, A] {
    return Coyoneda[G, A]{
        runCoyoneda: func(k func(A) B) G {
            return f(c.runCoyoneda(func(a A) B {
                return k(a)
            }))
        },
    }
}

type Presheaf[A any] func(A) int

func contramap[A any, B any](f func(B) A, g Presheaf[A]) Presheaf[B] {
    return func(b B) int {
        return g(f(b))
    }
}

func tabulate[A any](a A) Presheaf[A] {
    return func(_ A) int {
        return 0
    }
}