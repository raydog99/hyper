from typing import Callable, Generic, TypeVar

A = TypeVar('A')
B = TypeVar('B')

class Density(Generic[A]):
    def __init__(self, f: Callable[[Callable[[A], B]], B]):
        self.f = f

    def return_(a: A) -> 'Density[A]':
        return Density(lambda k: k(a))

    def bind(self, k: Callable[[A], 'Density[B]']) -> 'Density[B]':
        def g(h: Callable[[B], C]) -> C:
            d = k(self.f(lambda x: x))
            return d.f(h)
        return Density(g)

    def map(self, f: Callable[[A], B]) -> 'Density[B]':
        def g(h: Callable[[B], C]) -> C:
            return self.f(lambda x: h(f(x)))
        return Density(g)

    def extract(self) -> A:
        return self.f(lambda x: x)

    def duplicate(self) -> 'Density[Density[A]]':
        def g(h: Callable[['Density[A]'], C]) -> C:
            def fh(a: A) -> 'Density[A]':
                return Density(lambda k: k(a))
            return h(self.map(fh))
        return Density(g)

    def extend(self, f: Callable[['Density[A]'], B]) -> 'Density[B]':
        def g(h: Callable[[B], C]) -> C:
            def fh(a: A) -> B:
                def f2(k: Callable[[A], A]) -> A:
                    return k(a)
                return f(Density(f2))
            return self.f(fh)
        return Density(g)