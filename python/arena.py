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