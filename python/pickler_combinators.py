from typing import Callable, Optional, Union

Pu = Callable

def pair(pa: Pu[A], pb: Pu[B]) -> Pu[tuple[A, B]]:
    def pair_impl(p: tuple[A, B]) -> None:
        pa(p[0])
        pb(p[1])
    return pair_impl

def triple(pa: Pu[A], pb: Pu[B], pc: Pu[C]) -> Pu[tuple[A, B, C]]:
    def triple_impl(p: tuple[A, B, C]) -> None:
        pa(p[0])
        pb(p[1])
        pc(p[2])
    return triple_impl

def quad(pa: Pu[A], pb: Pu[B], pc: Pu[C], pd: Pu[D]) -> Pu[tuple[A, B, C, D]]:
    def quad_impl(p: tuple[A, B, C, D]) -> None:
        pa(p[0])
        pb(p[1])
        pc(p[2])
        pd(p[3])
    return quad_impl

def wrap(i: Callable[[A], B], j: Callable[[B], A]) -> Pu[B]:
    def wrap_impl(b: B) -> None:
        a = j(b)
        b = i(a)
    return wrap_impl

def zero_to(n: int) -> Pu[int]:
    if n == 0:
        return lambda _: None
    return wrap(lambda i: i // 256, lambda i: i % 256)(zero_to(n // 256))

def unit() -> Pu[Any]:
    return lambda _: None

char: Pu[str] = wrap(chr, ord)(zero_to(255))

bool_: Pu[bool] = wrap(bool, int)(zero_to(1))

def nat() -> Pu[int]:
    half = 128
    def nat_impl(n: int) -> None:
        if n < half:
            unit()(n)
        else:
            wrap(lambda i: half + i % half, lambda i: (i - half) // half)(nat())(n)
    return nat_impl

def fixed_list(pa: Pu[A], n: int) -> Pu[list[A]]:
    if n == 0:
        return lambda _: []
    return wrap(
        lambda p: [p[0]] + p[1],
        lambda l: (l[0], l[1:])
    )(pair(pa, fixed_list(pa, n - 1)))

def list(pa: Pu[A]) -> Pu[list[A]]:
    def list_impl(l: list[A]) -> None:
        n = len(l)
        nat()(n)
        fixed_list(pa, n)(l)
    return list_impl

string: Pu[str] = list(char)

def alt(tag: Callable[[A], int], ps: list[Pu[A]]) -> Pu[A]:
    if not ps:
        raise Exception("alt: empty list")
    def alt_impl(a: A) -> None:
        n = tag(a)
        ps[n](a)
    return alt_impl

def p_maybe(pa: Pu[A]) -> Pu[Optional[A]]:
    return alt(
        lambda p: 1 if p is not None else 0,
        [
            lambda _: None,
            wrap(lambda a: a, lambda p: p)(pa)
        ]
    )

def p_either(pa: Pu[A], pb: Pu[B]) -> Pu[Union[A, B]]:
    return alt(
        lambda e: 0 if isinstance(e, A) else 1,
        [
            wrap(lambda a: a, lambda a: a)(pa),
            wrap(lambda b: b, lambda b: b)(pb)
        ]
    )