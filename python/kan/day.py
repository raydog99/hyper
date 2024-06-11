from typing import Callable, Generic, TypeVar

F = TypeVar('F')
G = TypeVar('G')
A = TypeVar('A')
B = TypeVar('B')

class Day(Generic[F, G, A]):
    def __init__(self, f: F, g: G, apply: Callable[[F, G], A]):
        self.f = f
        self.g = g
        self.apply = apply

    def fmap(self, f: Callable[[A], B]) -> 'Day[F, G, B]':
        return Day(self.f, self.g, lambda ff, gg: f(self.apply(ff, gg)))

    @staticmethod
    def pure(x: A) -> 'Day[None, None, A]':
        return Day(None, None, lambda _, __: x)

    def ap(self, other: 'Day[F, G, Callable[[A], B]]') -> 'Day[F, G, B]':
        return Day(
            (self.f, other.f),
            (self.g, other.g),
            lambda ff, gg: other.apply(ff[0], gg[0])(self.apply(ff[1], gg[1]))
        )

def assoc(day: Day[F, Day[G, H, A], A]) -> Day[Day[F, G, A], H, A]:
    return Day(
        Day(day.f, day.g.f, lambda ff, gg: day.apply(ff, (gg, day.g.g))),
        day.g.g,
        lambda fga, hc: day.g.apply(fga.g, hc)(fga.apply(fga.f, fga.g))
    )

def disassoc(day: Day[Day[F, G, A], H, A]) -> Day[F, Day[G, H, A], A]:
    return Day(
        day.f.f,
        Day(day.f.g, day.g, lambda gg, hh: day.apply((gg, hh.f), hh.g)),
        lambda ff, gha: day.apply((ff, gha.f), gha.g)(gha.apply(gha.f, gha.g))
    )

def swapped(day: Day[F, G, A]) -> Day[G, F, A]:
    return Day(day.g, day.f, lambda gg, ff: day.apply(ff, gg))

def intro1(f: F) -> Day[None, F, A]:
    return Day(None, f, lambda _, ff: ff)

def intro2(g: G) -> Day[G, None, A]:
    return Day(g, None, lambda gg, _: gg)

def elim1(day: Day[None, F, A]) -> F:
    return day.g

def elim2(day: Day[G, None, A]) -> G:
    return day.f

def trans1(f: Callable[[A], B], day: Day[F, G, A]) -> Day[F, G, B]:
    return Day(day.f, day.g, lambda ff, gg: f(day.apply(ff, gg)))

def trans2(g: Callable[[G], H], day: Day[F, G, A]) -> Day[F, H, A]:
    return Day(day.f, g(day.g), lambda ff, hh: day.apply(ff, hh))