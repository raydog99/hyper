package capitan

type Day[F any, G any, A any] struct {
	f F
	g G
	apply func(F, G) A
}

func DayPure[A any](x A) Day[struct{}, struct{}, A] {
	return Day[struct{}, struct{}, A]{apply: func(_, _ struct{}) A { return x }}
}

func DayFmap[F any, G any, A any, B any](f func(A) B, day Day[F, G, A]) Day[F, G, B] {
	return Day[F, G, B]{day.f, day.g, func(fb F, gc G) B { return f(day.apply(fb, gc)) }}
}

func DayAp[F any, G any, A any, B any](day1 Day[F, G, func(A) B], day2 Day[F, G, A]) Day[F, G, B] {
	return Day[F, G, B]{
		day1.f, day2.g,
		func(fa F, gc G) B {
			return day1.apply(fa, day1.g)(day2.apply(day1.f, gc))
		},
	}
}

func DayAssoc[F any, G any, H any, A any](day Day[F, Day[G, H, A], A]) Day[Day[F, G, A], H, A] {
	return Day[Day[F, G, A], H, A]{
		Day[F, G, A]{day.f, day.g.f, day.apply},
		day.g.g,
		func(fga Day[F, G, A], hc H) A {
			return day.g.apply(fga.g, hc)(fga.apply(fga.f, fga.g))
		},
	}
}

func DayDisassoc[F any, G any, H any, A any](day Day[Day[F, G, A], H, A]) Day[F, Day[G, H, A], A] {
	return Day[F, Day[G, H, A], A]{
		day.f.f,
		Day[G, H, A]{day.f.g, day.g, day.apply},
		func(fa F, gha Day[G, H, A]) A {
			return day.apply(Day[F, G, A]{fa, gha.f, gha.apply}, gha.g)
		},
	}
}

func DaySwapped[F any, G any, A any](day Day[F, G, A]) Day[G, F, A] {
	return Day[G, F, A]{day.g, day.f, func(ga G, fa F) A { return day.apply(fa, ga) }}
}

func DayIntro1[F any, A any](f F) Day[struct{}, F, A] {
	return Day[struct{}, F, A]{struct{}{}, f, func(_ struct{}, fa F) A { return fa }}
}

func DayIntro2[G any, A any](g G) Day[G, struct{}, A] {
	return Day[G, struct{}, A]{g, struct{}{}, func(ga G, _ struct{}) A { return ga }}
}

func DayElim1[F any, A any](day Day[struct{}, F, A]) F {
	return day.g
}

func DayElim2[G any, A any](day Day[G, struct{}, A]) G {
	return day.f
}

func DayTrans1[F any, G any, A any, B any](f func(A) B, day Day[F, G, A]) Day[F, G, B] {
	return Day[F, G, B]{day.f, day.g, func(fa F, ga G) B { return f(day.apply(fa, ga)) }}
}

func DayTrans2[F any, G any, A any, B any](g func(A) B, day Day[F, G, A]) Day[F, G, B] {
	return Day[F, G, B]{day.f, g(day.g), func(fa F, gb G) B { return day.apply(fa, gb) }}
}