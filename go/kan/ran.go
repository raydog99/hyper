package capitan

type Identity[A any] struct {
	Value A
}

type Adjunction[T, G any] struct {
	Unit           func(g G) T
	RightAdjoint   func(t T) G
	LeftAdjunct    func(f func(g G) T) func(T) T
	RightAdjunctor func(f func(T) G) func(G) G
}

type Composition[T, G any] struct {
	Compose   func(t T) Compose[T, G]
	Decompose func(c Compose[T, G]) T
}

func (c Compose[T, G]) Map[A any](f func(T) A) Compose[A, G] {
	return Compose[A, G]{
		Compose: func(a A) Compose[A, G] {
			return Compose[A, G]{
				Value: c.Compose(f(a)),
			}
		},
		Decompose: func(c Compose[A, G]) A {
			return f(c.Value.Decompose())
		},
	}
}

func ToRan[G, H, T, A any](f func(H) T, r Ran[G, H, A]) T {
	return f(r.unRan(func(b H) (G, A) {
		return Identity[G]{}, b
	}))
}

func FromRan[G, H, T, A any](s func(Ran[G, H, A]) T, h H) T {
	return s(Ran[G, H, A]{
		unRan: func(k func(b H) (c G, d A)) A {
			c, d := k(h)
			return d
		},
	})
}

func AdjointToRan[G, T, A any](adj Adjunction[T, G], g G) Ran[T, Identity[A], A] {
	return Ran[T, Identity[A], A]{
		unRan: func(k func(b Identity[A]) (c T, d A)) A {
			c, d := k(Identity[A]{Value: adj.Unit(g)})
			return d
		},
	}
}

func RanToAdjoint[G, T, A any](adj Adjunction[T, G], r Ran[T, Identity[A], A]) T {
	return adj.RightAdjoint(ToRan(func(i Identity[A]) A { return i.Value }, r))
}

func RanToComposedAdjoint[G, T, H, A any](adj Adjunction[T, G], r Ran[T, H, A]) H {
	return ToRan(func(h H) H {
		return h.Map(adj.RightAdjoint)
	}, r)
}

func ComposedAdjointToRan[G, T, H, A any](adj Adjunction[T, G], h H) Ran[T, H, A] {
	return Ran[T, H, A]{
		unRan: func(k func(b H) (c T, d A)) A {
			c, d := k(h.Map(adj.Unit))
			return d
		},
	}
}

func ComposeRan[T, G, H, A any](comp Composition[T, G], r Ran[T, Ran[G, H, A], A]) Ran[Compose[T, G], H, A] {
	return Ran[Compose[T, G], H, A]{
		unRan: func(k func(b H) (c Compose[T, G], d A)) A {
			return r.unRan(func(r Ran[G, H, A]) (Compose[T, G], A) {
				return Compose[T, G]{
					Value: comp.Compose(ToRan(func(h H) T {
						return r.unRan(func(b H) (G, A) {
							return Identity[G]{Value: b}, b
						})
					}, r)),
				}, r.unRan(k)
			})
		},
	}
}

func DecomposeRan[T, G, H, A any](comp Composition[T, G], r Ran[Compose[T, G], H, A]) Ran[T, Ran[G, H, A], A] {
	return Ran[T, Ran[G, H, A], A]{
		unRan: func(k func(b Ran[G, H, A]) (c T, d A)) A {
			return r.unRan(func(b H) (Compose[T, G], A) {
				c, d := k(Ran[G, H, A]{
					unRan: func(k func(b H) (c G, d A)) A {
						c, d := k(b)
						return d
					},
				})
				return Compose[T, G]{Value: comp.Decompose(c)}, d
			})
		},
	}
}

func Gran[H, A any](h H) Ran[Identity[A], H, A] {
	return Ran[Identity[A], H, A]{
		unRan: func(k func(b H) (c Identity[A], d A)) A {
			c, d := k(h)
			return d
		},
	}
}