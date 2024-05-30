from typing import Generic, TypeVar, Tuple, Callable, TypeVar

T = TypeVar('T', covariant = True)

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

class Comonoid(Generic[T]):
    def counit(self) -> None:
        raise NotImplementedError

    def comult(self) -> Tuple[T, T]:
        raise NotImplementedError

class PolyTerm(Generic[T]):
    def __init__(self, coeff: T, exp: int):
        self.coeff = coeff
        self.exp = exp

Poly = list[PolyTerm[T]]

def poly_counit(p: Poly[T]) -> None:
    if not p:
        return
    if p[0].exp == 0:
        p[0].coeff.counit()
    else:
        raise ValueError("Polynomial must be constant to have a counit")

def poly_comult(p: Poly[T]) -> Tuple[Poly[T], Poly[T]]:
    xs = []
    ys = []
    for term in p:
        if term.exp == 0:
            c1, c2 = term.coeff.comult()
            xs.append(PolyTerm(c1, 0))
            ys.append(PolyTerm(c2, 0))
        else:
            xs.append(term)
            ys.append(PolyTerm(term.coeff.counit(), term.exp))
    return xs, ys

class PolyComonoid(Comonoid[Poly[T]]):
    def __init__(self, p: Poly[T]):
        self.poly = p

    def counit(self) -> None:
        poly_counit(self.poly)

    def comult(self) -> Tuple[Poly[T], Poly[T]]:
        return poly_comult(self.poly)