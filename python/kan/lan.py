from typing import Callable, TypeVar, Generic

F = TypeVar('F')
G = TypeVar('G')
A = TypeVar('A')
B = TypeVar('B')

class LanF(Generic[F, G, A]):
    def __init__(self, f: Callable[[Callable[[F, A], A], G], A]):
        self.f = f

    def fmap(self, g: Callable[[A], B]) -> 'LanF[F, G, B]':
        return LanF(lambda k, h: g(self.f(lambda f, a: k(f, a), h)))

    def apply(self, f: 'LanF[F, G, Callable[[A], B]]', x: 'LanF[F, G, A]') -> 'LanF[F, G, B]':
        return LanF(lambda k, h: f.f(lambda f, r: lambda a: r(k(f, a)), h)(x.f(lambda f, a: k(f, a), h)))

    def pure(a: A) -> 'LanF[F, G, A]':
        return LanF(lambda _, _: a)

    def to_lan(self, phi: Callable[[G], A]) -> A:
        return self.f(lambda f, a: phi(f, a), id)

    def from_lan(self, phi: Callable[['LanF[F, G, A]'], A]) -> A:
        return phi(LanF(lambda k, h: k(id, h)))