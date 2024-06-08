from typing import Callable, Generic, TypeVar

A = TypeVar('A')
B = TypeVar('B')
M = TypeVar('M')

class Codensity(Generic[M, A]):
    def __init__(self, value: Callable[[Callable[[A], M[B]]], M[B]]):
        self.value = value

    @staticmethod
    def return_(x: A) -> 'Codensity[M, A]':
        return Codensity(lambda k: k(x))

    def bind(self, f: Callable[[A], 'Codensity[M, B]']) -> 'Codensity[M, B]':
        return Codensity(lambda k: self.value(lambda x: f(x).value(k)))

    def map(self, f: Callable[[A], B]) -> 'Codensity[M, B]':
        return Codensity(lambda k: self.value(lambda x: k(f(x))))

    def lowerCodensity(self) -> M[A]:
        return self.value(lambda x: x)

    @staticmethod
    def reset(m: 'Codensity[M, A]') -> M[A]:
        return m.lowerCodensity()

    @staticmethod
    def shift(f: Callable[[Callable[[A], 'Codensity[M, B]']], 'Codensity[M, A]']) -> 'Codensity[M, A]':
        def aux(k: Callable[[A], M[A]]) -> M[A]:
            return f(lambda x: Codensity.return_(x)).value(k)
        return Codensity(aux)