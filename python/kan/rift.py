from typing import Callable, TypeVar, Generic

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')
G = TypeVar('G')
H = TypeVar('H')

class Rift(Generic[G, H, A]):
    def __init__(self, run_rift: Callable[[Callable[[B], tuple[A, C]], G], H]):
        self.run_rift = run_rift

    def contramap(self, f: Callable[[B], A]) -> 'Rift[G, H, B]':
        def new_run_rift(bac: Callable[[B], tuple[B, C]], g: G) -> H:
            return self.run_rift(lambda b: (f(bac(b)[0]), bac(b)[1]), g)
        return Rift(new_run_rift)

def divide(f: Callable[[A], B], g: Rift[G, G, A], h: Rift[G, G, B]) -> Rift[G, G, C]:
    def new_run_rift(bac: Callable[[B], tuple[C, C]], g: G) -> G:
        return g.run_rift(lambda a: (f(bac(a)[0]), bac(a)[1]), g)
    return Rift(new_run_rift)