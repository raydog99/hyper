from typing import Callable, TypeVar

F = TypeVar('F')
A = TypeVar('A')
R = TypeVar('R')

class Yoneda(object):
    def __init__(self, run_yoneda: Callable[[Callable[[A], R]], F]):
        self.run_yoneda = run_yoneda

    def __call__(self, k: Callable[[A], R]) -> F:
        return self.run_yoneda(k)

def lift_yoneda(fa: F) -> Yoneda[F, A]:
    def run_yoneda(k: Callable[[A], R]) -> F:
        return fa.contramap(k)
    return Yoneda(run_yoneda)

def lower_yoneda(m: Yoneda[F, A]) -> F:
    return m(lambda x: x)

def contramap(g: Callable[[B], A], m: Yoneda[F, A]) -> Yoneda[F, B]:
    def run_yoneda(k: Callable[[B], R]) -> F:
        return m(lambda x: k(g(x)))
    return Yoneda(run_yoneda)