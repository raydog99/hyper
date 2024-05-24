package combinators

type pu[A any] func(A)

func pair[A, B any](pa pu[A], pb pu[B]) pu[struct{ A; B }] {
	return func(p struct{ A; B }) {
		pa(p.A)
		pb(p.B)
	}
}

func triple[A, B, C any](pa pu[A], pb pu[B], pc pu[C]) pu[struct{ A; B; C }] {
	return func(p struct{ A; B; C }) {
		pa(p.A)
		pb(p.B)
		pc(p.C)
	}
}

func quad[A, B, C, D any](pa pu[A], pb pu[B], pc pu[C], pd pu[D]) pu[struct{ A; B; C; D }] {
	return func(p struct{ A; B; C; D }) {
		pa(p.A)
		pb(p.B)
		pc(p.C)
		pd(p.D)
	}
}

func wrap[A, B any](i func(A) B, j func(B) A) pu[B] {
	return func(b B) {
		pu[A](j(b))(i(j(b)))
	}
}

func zeroTo(n int) pu[int] {
	if n == 0 {
		return func(int) {}
	}
	return wrap(func(i int) int { return i/256 }, func(i int) int { return i%256 })(zeroTo(n / 256))
}

func unit[A any]() pu[A] {
	return func(A) {}
}

var char = wrap(func(c byte) rune { return rune(c) }, func(r rune) byte { return byte(r) })(zeroTo(255))
var bool = wrap(func(b bool) int { if b { return 1 } else { return 0 } }, func(i int) bool { return i != 0 })(zeroTo(1))

func nat() pu[int] {
	half := 128
	return func(n int) {
		if n < half {
			pu[int](func(int) {})(n)
		} else {
			wrap(func(i int) int { return half + i%half }, func(i int) int { return (i - half) / half })(nat())(n)
		}
	}
}

func fixedList[A any](pa pu[A], n int) pu[[]A] {
	if n == 0 {
		return func([]A) {}
	}
	return wrap(func(p struct{ A; []A }) []A { return append([]A{p.A}, p.[]A...) }, func(l []A) struct{ A; []A } { return struct{ A; []A }{l[0], l[1:]} })(pair(pa, fixedList(pa, n-1)))
}

func list[A any](pa pu[A]) pu[[]A] {
	return func(l []A) {
		n := len(l)
		nat()(n)
		fixedList(pa, n)(l)
	}
}

var string = list(char)

func alt[A any](tag func(A) int, ps []pu[A]) pu[A] {
	if len(ps) == 0 {
		panic("alt: empty list")
	}
	return func(a A) {
		n := tag(a)
		ps[n](a)
	}
}

func pMaybe[A any](pa pu[A]) pu[*A] {
	return alt(func(p *A) int {
		if p == nil {
			return 0
		}
		return 1
	}, []pu[*A]{
		func(*A) {},
		wrap(func(a A) *A { return &a }, func(p *A) A { return *p })(pa),
	})
}

func pEither[A, B any](pa pu[A], pb pu[B]) pu[either] {
	type either struct {
		isLeft bool
		left   A
		right  B
	}
	return alt(func(e either) int {
		if e.isLeft {
			return 0
		}
		return 1
	}, []pu[either]{
		wrap(func(a A) either { return either{true, a, zero(B)} }, func(e either) A { return e.left })(pa),
		wrap(func(b B) either { return either{false, zero(A), b} }, func(e either) B { return e.right })(pb),
	})
}