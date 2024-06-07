from typing import Callable, Generic, TypeVar

A = TypeVar('A')
B = TypeVar('B')
G = TypeVar('G')
H = TypeVar('H')
T = TypeVar('T')

class Identity(Generic[A]):
    def __init__(self, value: A):
        self.value = value

class Adjunction(Generic[T, G]):
    def __init__(self, unit: Callable[[G], T], right_adjoint: Callable[[T], G],
                 left_adjunct: Callable[[Callable[[G], T]], Callable[[T], T]],
                 right_adjunctor: Callable[[Callable[[T], G]], Callable[[G], G]]):
        self.unit = unit
        self.right_adjoint = right_adjoint
        self.left_adjunct = left_adjunct
        self.right_adjunctor = right_adjunctor

class Composition(Generic[T, G]):
    def __init__(self, compose: Callable[[T], 'Compose[T, G]'], decompose: Callable[['Compose[T, G]'], T]):
        self.compose = compose
        self.decompose = decompose

class Compose(Generic[T, G]):
    def __init__(self, value: T):
        self.value = value

    def map(self, f: Callable[[T], 'A']) -> 'Compose[A, G]':
        return Compose(f(self.value))

class Ran(Generic[G, H, A]):
    def __init__(self, un_ran: Callable[[Callable[[H], tuple[G, A]]], A]):
        self.un_ran = un_ran

    def map(self, f: Callable[[A], B]) -> 'Ran[G, H, B]':
        return Ran(lambda k: f(self.un_ran(lambda h: k(h))))

    def apply(self, f: 'Ran[G, H, Callable[[A], B]]') -> 'Ran[G, H, B]':
        return Ran(lambda k: f.un_ran(lambda h: k(h))(self.un_ran(lambda h: k(h))))

def pure(a: A) -> 'Ran[G, H, A]':
    return Ran(lambda _: a)

def to_ran(f: Callable[[H], T], ran: 'Ran[G, H, A]') -> T:
    return f(ran.un_ran(lambda h: (Identity(h), h)))

def from_ran(s: Callable[['Ran[G, H, A]'], T], h: H) -> T:
    return s(Ran(lambda k: k(h)[1]))

def adjoint_to_ran(adj: Adjunction[T, G], g: G) -> 'Ran[T, Identity[A], A]':
    return Ran(lambda k: k(Identity(adj.unit(g)))[1])

def ran_to_adjoint(adj: Adjunction[T, G], ran: 'Ran[T, Identity[A], A]') -> T:
    return adj.right_adjoint(to_ran(lambda i: i.value, ran))

def ran_to_composed_adjoint(adj: Adjunction[T, G], ran: 'Ran[T, H, A]') -> H:
    return to_ran(lambda h: h.map(adj.right_adjoint), ran)

def composed_adjoint_to_ran(adj: Adjunction[T, G], h: H) -> 'Ran[T, H, A]':
    return Ran(lambda k: k(h.map(adj.unit)))

def compose_ran(comp: Composition[T, G], ran: 'Ran[T, Ran[G, H, A], A]') -> 'Ran[Compose[T, G], H, A]':
    return Ran(lambda k: ran.un_ran(lambda r: (comp.compose(to_ran(lambda h: r.un_ran(lambda h: (Identity(h), h)), r)), r.un_ran(lambda h: k(h)[1]))))

def decompose_ran(comp: Composition[T, G], ran: 'Ran[Compose[T, G], H, A]') -> 'Ran[T, Ran[G, H, A], A]':
    return Ran(lambda k: ran.un_ran(lambda h: k(Ran(lambda k: (Identity(comp.decompose(h.map(lambda c: c.value))), k(h)[1])))[1]))

def gran(h: H) -> 'Ran[Identity[A], H, A]':
    return Ran(lambda k: k(h)[1])