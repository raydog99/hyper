from typing import Callable, Generic, TypeVar

F = TypeVar('F')
A = TypeVar('A')
B = TypeVar('B')

class Coyoneda(Generic[F, A]):
    def __init__(self, f: Callable[[A], F], m: F):
        self.f = f
        self.m = m

    def contramap(self, g: Callable[[B], A]) -> 'Coyoneda[F, B]':
        return Coyoneda(lambda b: self.f(g(b)), self.m)