from typing import Any, Callable, Generic, Tuple, TypeVar

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')
D = TypeVar('D')
E = TypeVar('E')
F = TypeVar('F')
G = TypeVar('G')
H = TypeVar('H')

class Invar_Day(Generic[F, G, A]):
    def __init__(self, fb: F, gc: G, bca: Callable[[Any, Any], A], abc: Callable[[A], Tuple[Any, Any]]):
        self.fb = fb
        self.gc = gc
        self.bca = bca
        self.abc = abc

    @staticmethod
    def day(fa: F, gb: G) -> 'Day[F, G, Tuple[B, C]]':
        def bca(b: Any, c: Any) -> Tuple[B, C]:
            return b, c
        def abc(a: Tuple[B, C]) -> Tuple[Any, Any]:
            return a[0], a[1]
        return Day(fa, gb, bca, abc)

    def invmap(self, f: Callable[[A], B], g: Callable[[C], D]) -> 'Day[F, G, B]':
        def bca(b: Any, c: Any) -> B:
            return f(self.bca(b, g(c)))
        def abc(a: B) -> Tuple[Any, Any]:
            b, c = self.abc(a)
            return b, g(c)
        return Day(self.fb, self.gc, bca, abc)

    def assoc(self) -> 'Day[Day[F, G, D], H, A]':
        def f(a: A) -> Tuple['Day[F, G, D]', H]:
            b, c = self.abc(a)
            d, e = self.gc.abc(c)
            return Day(self.fb, d, lambda b, c: self.bca(b, c), lambda a: (b, a)), e
        def g(bc: 'Day[F, G, D]', e: H) -> A:
            return self.bca(bc.fb, self.gc.bca(bc.gc, e))
        return Day(Day(self.fb, self.gc.gc, lambda b, c: b, lambda c: (c, ())), self.gc.gc, g, f)

    def disassoc(self) -> 'Day[F, Day[G, H, A], B]':
        def f(b: B, day: 'Day[G, H, A]') -> B:
            return day.bca(self.fb.bca(b, day.gc), day.abc)
        def g(b: B) -> Tuple[B, 'Day[G, H, A]']:
            a, c = self.abc(self.bca(self.fb.abc(b), self.gc))
            return b, Day(self.fb.gc, c, self.fb.bca, a)
        return Day(self.fb.fb, Day(self.fb.gc, self.gc, self.fb.bca, self.abc), f, g)

    def swapped(self) -> 'Day[G, F, A]':
        def bca(c: G, b: F) -> A:
            return self.bca(b, c)
        def abc(a: A) -> Tuple[G, F]:
            b, c = self.abc(a)
            return c, b
        return Day(self.gc, self.fb, bca, abc)

    def intro1(self, fa: F) -> 'Day[tuple, F, B]':
        def bca(_, a: B) -> B:
            return a
        def abc(a: B) -> Tuple[tuple, B]:
            return (), a
        return Day((), fa, bca, abc)

    def intro2(self, fa: F) -> 'Day[F, tuple, B]':
        def bca(a: B, _) -> B:
            return a
        def abc(a: B) -> Tuple[B, tuple]:
            return a, ()
        return Day(fa, (), bca, abc)

    def elim1(self) -> F:
        return self.invmap(lambda a: self.bca((), a), lambda _:()).gc

    def elim2(self) -> F:
        return self.invmap(lambda a: self.bca(a, ()), lambda _: ()).fb

    def trans1(self, fg: Callable[[F], G]) -> 'Day[G, H, A]':
        return Day(fg(self.fb), self.gc, self.bca, self.abc)

    def trans2(self, gh: Callable[[G], H]) -> 'Day[F, H, A]':
        return Day(self.fb, gh(self.gc), self.bca, self.abc)

    def to_contravariant(self):
        return Contravariant(self.fb, self.gc, self.abc)

    def to_covariant(self):
        return Covariant(self.fb, self.gc, self.bca)

class Contravariant(Generic[F, G, A]):
    def __init__(self, fb: F, gc: G, abc: Callable[[A], Tuple[Any, Any]]):
        self.fb = fb
        self.gc = gc
        self.abc = abc

class Covariant(Generic[F, G, A]):
    def __init__(self, fb: F, gc: G, bca: Callable[[Any, Any], A]):
        self.fb = fb
        self.gc = gc
        self.bca = bca