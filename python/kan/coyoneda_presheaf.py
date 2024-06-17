from typing import Callable, TypeVar

A = TypeVar('A')
B = TypeVar('B')
F = TypeVar('F', bound=Callable)

class Coyoneda(Generic[F, A]):
    def __init__(self, run_coyoneda: Callable[[Callable[[A], B]], F]):
        self.run_coyoneda = run_coyoneda

    def contramap(self, f: Callable[[B], A]) -> 'Coyoneda[Callable[[B], B], B]':
        return Coyoneda(lambda k: self.run_coyoneda(lambda a: k(f(a))))

    @staticmethod
    def lift(x: F) -> 'Coyoneda[F, A]':
        return Coyoneda(lambda f: f(x))

    def lower(self) -> F:
        return self.run_coyoneda(lambda a: a)

    def hoist(self, f: Callable[[F], G]) -> 'Coyoneda[G, A]':
        return Coyoneda(lambda k: f(self.run_coyoneda(lambda a: k(a))))

Presheaf = Callable[[A], int]

def contramap(f: Callable[[B], A], g: Presheaf[A]) -> Presheaf[B]:
    return lambda b: g(f(b))

def tabulate(a: A) -> Presheaf[A]:
    return lambda _: 0