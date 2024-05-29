from typing import Callable, TypeVar

T = TypeVar('T')

class Poly(list):
    def __init__(self, coeffs: list[T]):
        super().__init__(coeffs)

    def eval(self, x: T) -> T:
        result = self[0]
        for i, coeff in enumerate(self[1:], start=1):
            result += coeff * x ** i
        return result

    def map(self, f: Callable[[T], T]) -> 'Poly[T]':
        return Poly(f(coeff) for coeff in self)

    def __add__(self, other: 'Poly[T]') -> 'Poly[T]':
        maxlen = max(len(self), len(other))
        coeffs = [self[i] + other[i] if i < len(self) and i < len(other) else self[i] if i < len(self) else other[i] for i in range(maxlen)]
        return Poly(coeffs)

    def __mul__(self, other: 'Poly[T]') -> 'Poly[T]':
        coeffs = [T() for _ in range(len(self) + len(other) - 1)]
        for i, coeff1 in enumerate(self):
            for j, coeff2 in enumerate(other):
                coeffs[i + j] += coeff1 * coeff2
        return Poly(coeffs)