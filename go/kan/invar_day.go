package capitan

type Invar_Day[F, G, A any] struct {
	Fb  F
	Gc  G
	Bca func(b, c any) A
	Abc func(a A) (b any, c any)
}

func day[F, G, A, B, C any](fa F, gb G) Invar_Day[F, G, A] {
	return Invar_Day[F, G, A]{
		Fb: fa,
		Gc: gb,
		Bca: func(b B, c C) A {
			return A(B(b), C(c))
		},
		Abc: func(a A) (B, C) {
			b, c := interface{}(a).(A)
			return B(b), C(c)
		},
	}
}

func invmap[F, G, A, B, C, D any](f func(A) B, g func(C) D, day Invar_Day[F, G, A]) Invar_Day[F, G, B] {
	return Invar_Day[F, G, B]{
		Fb: day.Fb,
		Gc: day.Gc,
		Bca: func(b B, c D) B {
			return f(day.Bca(interface{}(b).(A), interface{}(c).(C)))
		},
		Abc: func(a B) (B, D) {
			b, c := day.Abc(interface{}(a).(A))
			return B(b), D(g(interface{}(c).(C)))
		},
	}
}

func assoc[F, G, H, A, B, C any](day Invar_Day[F, Invar_Day[G, H, C], A]) Invar_Day[Invar_Day[F, G, C], H, A] {
	return Invar_Day[Invar_Day[F, G, C], H, A]{
		Fb: day.Fb,
		Gc: day.Gc.Gc,
		Bca: func(bc Invar_Day[F, G, C], e H) A {
			return day.Bca(bc.Fb, day.Gc.Bca(bc.Gc, e))
		},
		Abc: func(a A) (Invar_Day[F, G, C], H) {
			b, c := day.Abc(a)
			d, e := day.Gc.Abc(c)
			return Invar_Day[F, G, C]{day.Fb, d, day.Bca, func(a A) (B, C) { return b, interface{}(a).(C) }}, e
		},
	}
}

func disassoc[F, G, H, A, B, C any](day Invar_Day[Invar_Day[F, G, B], H, A]) Invar_Day[F, Invar_Day[G, H, A], B] {
	return Invar_Day[F, Invar_Day[G, H, A], B]{
		Fb: day.Fb.Fb,
		Gc: Invar_Day[G, H, A]{day.Fb.Gc, day.Gc, day.Fb.Bca, day.Abc},
		Bca: func(b B, day Invar_Day[G, H, A]) B {
			return day.Bca(day.Fb.Bca(b, day.Gc), day.Abc)
		},
		Abc: func(b B) (B, Invar_Day[G, H, A]) {
			a, c := day.Abc(day.Bca(day.Fb.Abc(b), day.Gc))
			return b, Invar_Day[G, H, A]{day.Fb.Gc, c, day.Fb.Bca, a}
		},
	}
}

func swapped[F, G, A any](day Invar_Day[F, G, A]) Invar_Day[G, F, A] {
	return Invar_Day[G, F, A]{
		Fb: day.Gc,
		Gc: day.Fb,
		Bca: func(c G, b F) A {
			return day.Bca(interface{}(b).(A), interface{}(c).(C))
		},
		Abc: func(a A) (G, F) {
			b, c := day.Abc(a)
			return interface{}(c).(G), interface{}(b).(F)
		},
	}
}

func intro1[F, A any](fa F) Invar_Day[unit, F, A] {
	return Invar_Day[unit, F, A]{
		Fb:  unit{},
		Gc:  fa,
		Bca: func(_ unit, a A) A {
	    return a
		},
		Abc: func(a A) (unit, A) {
		    return unit{}, a
		},
	}
}

func intro2[F, A any](fa F) Invar_Day[F, unit, A] {
    return Invar_Day[F, unit, A]{
        Fb: fa,
        Gc: unit{},
        Bca: func(a A, _ unit) A {
            return a
        },
        Abc: func(a A) (A, unit) {
            return a, unit{}
        },
    }
}

func elim1[F, A any](day Invar_Day[unit, F, A]) F {
    return invmap(day.Bca(unit{}), func(a A) A { return a }, day.Gc)
}

func elim2[F, A any](day Invar_Day[F, unit, A]) F {
    return invmap(func(a A) A { return day.Bca(a, unit{}) }, func(_ unit) unit { return unit{} }, day.Fb)
}

func trans1[F, G, H, A, B any](fg func(F) G, day Invar_Day[F, H, A]) Invar_Day[G, H, A] {
    return Invar_Day[G, H, A]{
        Fb: fg(day.Fb),
        Gc: day.Gc,
        Bca: day.Bca,
        Abc: day.Abc,
    }
}

func trans2[F, G, H, A, B any](gh func(G) H, day Invar_Day[F, G, A]) Invar_Day[F, H, A] {
    return Invar_Day[F, H, A]{
        Fb: day.Fb,
        Gc: gh(day.Gc),
        Bca: day.Bca,
        Abc: day.Abc,
    }
}

type Contravariant[F, G, A any] struct {
    Fb  F
    Gc  G
    Abc func(a A) (b any, c any)
}

func toContravariant[F, G, A any](day Invar_Day[F, G, A]) Contravariant[F, G, A] {
    return Contravariant[F, G, A]{
        Fb:  day.Fb,
        Gc:  day.Gc,
        Abc: day.Abc,
    }
}

type Covariant[F, G, A any] struct {
    Fb  F
    Gc  G
    Bca func(b any, c any) A
}

func toCovariant[F, G, A any](day Invar_Day[F, G, A]) Covariant[F, G, A] {
    return Covariant[F, G, A]{
        Fb:  day.Fb,
        Gc:  day.Gc,
        Bca: day.Bca,
    }
}