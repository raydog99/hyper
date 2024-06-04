from typing import Callable, Generic, TypeVar

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')

class Arena(Generic[A, B]):
    def __init__(self, pos: A, dis: Callable[[A], B]):
        self.pos = pos
        self.dis = dis

class Lens(Generic[Dom, Cod]):
    def __init__(self, observe: Callable[[Dom], Cod], interpret: Callable[[Dom, Callable[[Cod], Cod]], Callable[[Dom], Dom]]):
        self.observe = observe
        self.interpret = interpret

def id_lens(a: Arena[A, A]) -> Lens[A, A]:
    return Lens(lambda x: x, lambda p, f: lambda x: f(x))

def compose(lens23: Lens[A2, A3], lens12: Lens[A1, A2]) -> Lens[A1, A3]:
    obs = lambda p: lens23.observe(lens12.observe(p))
    int_ = lambda p, f: lens12.interpret(p, lambda x: lens23.interpret(x, lambda y: f(y)))
    return Lens(obs, int_)

Display = tuple[A, Callable[[A], B]]
AsFunctor = tuple[A, Callable[[Callable[[A], B]], C]]

A1 = TypeVar('A1', bound=Arena)
A2 = TypeVar('A2', bound=Arena)
B1 = TypeVar('B1', bound=Arena)
B2 = TypeVar('B2', bound=Arena)

Lens = Tuple[Callable[[Tuple[Tuple[A1, A2], Tuple[B1, B2]]], Tuple[Tuple[A1, B1], Callable[[Dis], Tuple[A2, B2]]]], Callable[[Tuple[Tuple[A1, A2], Tuple[B1, B2]], Dis], Dis]]

def duoidal(a1: A1, a2: A2, b1: B1, b2: B2) -> Lens:
    x = (combine(a1, a2), combine(b1, b2))
    y = (pair(a1, b1), combine(pair(a2, b2)))

    def o(p: Tuple[Tuple[A1, A2], Tuple[B1, B2]]) -> Tuple[Tuple[A1, B1], Callable[[Dis], Tuple[A2, B2]]]:
        p1, p2 = p
        q1, q2 = p1

        def pp(d: Dis) -> Tuple[A2, B2]:
            return (apply(p2, fst(d)), apply(q2, snd(d)))

        return (p1, q1), pp

    def i(p: Tuple[Tuple[A1, A2], Tuple[B1, B2]], d: Dis) -> Dis:
        p1, p2 = p
        q1, q2 = p1
        de1, de2 = d
        return (pair(fst(de1), fst(de2)), pair(snd(de1), snd(de2)))

    return o, i