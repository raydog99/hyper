from typing import Callable, Generic, TypeVar

M = TypeVar('M')
A = TypeVar('A')
B = TypeVar('B')

class TP(Generic[M, A]):
    def __init__(self, apply: Callable[[M], A]):
        self.apply = apply

    def __call__(self, m: M) -> A:
        return self.apply(m)

class TU(Generic[M, A]):
    def __init__(self, apply: Callable[[M], A]):
        self.apply = apply

    def __call__(self, m: M) -> A:
        return self.apply(m)

# Strategy application
def apply_tp(tp: TP[M, A], m: M) -> A:
    return tp(m)

def apply_tu(tu: TU[M, A], m: M) -> A:
    return tu(m)

# Strategy construction
def poly_tp(m: M) -> TP[M, M]:
    return TP(lambda x: x)

def poly_tu(m: M) -> TU[M, M]:
    return TU(lambda x: x)

def adhoc_tp(m: Callable[[A], B], t: A) -> TP[M, B]:
    return TP(lambda _: m(t))

def adhoc_tu(m: Callable[[A], B], t: A) -> TU[M, B]:
    return TU(lambda _: m(t))

# Sequential composition
def seq_tp(m: TP[M, TP[M, A]]) -> TP[M, A]:
    def apply(x: M) -> A:
        return apply_tp(m(x), x)
    return TP(apply)

def let_tp(m: TU[M, TP[M, A]]) -> TP[M, A]:
    def apply(x: M) -> A:
        return apply_tp(m(x), x)
    return TP(apply)

def seq_tu(m: TP[M, TU[M, A]]) -> TU[M, A]:
    def apply(x: M) -> A:
        return apply_tu(m(x), x)
    return TU(apply)

def let_tu(m: TU[M, TU[M, A]]) -> TU[M, A]:
    def apply(x: M) -> A:
        return apply_tu(apply_tu(m(x), x), x)
    return TU(apply)

# Choice
def choice_tp(m: Callable[[], TP[M, A]]) -> TP[M, A]:
    def apply(x: M) -> A:
        return m()(x)
    return TP(apply)

def choice_tu(m: Callable[[], TU[M, A]]) -> TU[M, A]:
    def apply(x: M) -> A:
        return m()(x)
    return TU(apply)

# Traversal combinators
def all_tp(m: M) -> TP[M, M]:
    return TP(lambda x: x)

def one_tp(m: M) -> TP[M, M]:
    return TP(lambda x: x)

def all_tu(m: M, a: A) -> TU[M, A]:
    return TU(lambda _: a)

def one_tu(m: M) -> TU[M, M]:
    return TU(lambda _: m)