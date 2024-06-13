package capitan

type Density[A any] struct {
    f func(b any) A
    x any
}

func Return[A any](a A) Density[A] {
    return Density[A]{func(b any) A { return a }, nil}
}

func (d Density[A]) Bind[B any](k func(A) Density[B]) Density[B] {
    g, y := k(d.f(d.x)).f, k(d.f(d.x)).x
    return Density[B]{g, y}
}

func (d Density[A]) Map[B any](f func(A) B) Density[B] {
    return Density[B]{func(y any) B { return f(d.f(y)) }, d.x}
}

func (d Density[A]) Extract() A {
    return d.f(d.x)
}

func (d Density[A]) Duplicate() Density[Density[A]] {
    return Density[Density[A]]{func(g func(Density[A]) any) Density[A] {
        return Density[A]{func(y any) A { return g(Density[A]{d.f, y}) }, y}
    }, d.x}
}

func (d Density[A]) Extend(f func(Density[A]) B) Density[B] {
    return Density[B]{func(y any) B { return f(Density[A]{d.f, y}) }, d.x}
}