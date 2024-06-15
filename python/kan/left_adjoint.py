from typing import Callable, Generic, TypeVar

A = TypeVar('A')
B = TypeVar('B')
F = TypeVar('F')
G = TypeVar('G')

class LeftAdjoint(Generic[F, G, A]):
    def __init__(self, f: Callable[[Callable[[A], G]], F], g: Callable[[G], G]):
        self.f = f
        self.g = g

    def fmap(self, f: Callable[[A], B]) -> 'LeftAdjoint[F, G, B]':
        return LeftAdjoint(
            lambda g: self.f(lambda a: g(f(a))),
            self.g
        )

    def apply(self, other: 'LeftAdjoint[F, G, B]') -> 'LeftAdjoint[F, G, B]':
        return LeftAdjoint(
            lambda g: self.f(lambda f: other.f(lambda x: g(f(x)))),
            lambda x: self.g(other.g(x))
        )

    @classmethod
    def pure(cls, a: A) -> 'LeftAdjoint[F, G, A]':
        return LeftAdjoint(
            lambda g: g(a),
            lambda x: x
        )

class Adjunction(Generic[F, U]):
    def __init__(self, left_adjunct: Callable[[Callable[[F], U]], U], right_adjunct: Callable[[U], F]):
        self.left_adjunct = left_adjunct
        self.right_adjunct = right_adjunct

    def adjoint_to_left_adjoint(self, u: U) -> LeftAdjoint[F, lambda _: None, A]:
        return LeftAdjoint(
            lambda g: self.right_adjunct(u)(lambda a: g(a)),
            lambda _: None
        )

    def left_adjoint_to_adjoint(self, left_adjoint: LeftAdjoint[F, lambda _: None, A]) -> U:
        return self.left_adjunct(lambda f: left_adjoint.f(lambda a: f(a)))