from typing import Callable, TypeVar, Generic

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')
G = TypeVar('G')
H = TypeVar('H')

class Lift(Generic[G, H, A]):
    def __init__(self, value: Callable[[Callable[[A], B], G], H]):
        self.value = value

    def dimap(self, f: Callable[[B], A], g: Callable[[A], C]) -> 'Lift[G, H, C]':
        def h(bac: Callable[[C], B], a: G) -> H:
            return self.value(lambda a: bac(g(a)), a)
        return Lift(h)

    def divide(self, f: Callable[[A], B], s: 'Lift[G, H, B]') -> 'Lift[G, H, C]':
        def h(bac: Callable[[C], B], a: G) -> H:
            return s.value(lambda b: bac(b), self.value(lambda a: bac(f(a)), a))
        return Lift(h)