from typing import Callable, Union

Ident = str

class Envcomp:
    pass

class Var(Envcomp):
    def __init__(self, x: Ident):
        self.x = x

class Lam(Envcomp):
    def __init__(self, x: Ident, e: Envcomp):
        self.x = x
        self.e = e

class App(Envcomp):
    def __init__(self, f: Envcomp, a: Envcomp):
        self.f = f
        self.a = a

class LamD(Envcomp):
    def __init__(self, x: Ident, e: Envcomp):
        self.x = x
        self.e = e

class AppD(Envcomp):
    def __init__(self, f: Envcomp, a: Envcomp):
        self.f = f
        self.a = a

Exp = Union[Var, Lam, App, LamD, AppD]

class Fun:
    def __init__(self, f: Callable[[Value], Value]):
        self.f = f

class Code:
    def __init__(self, e: Exp):
        self.e = e

Value = Union[Fun, Code]

def spec(e: Exp, r: dict[Ident, Value]) -> Value:
    if isinstance(e, Var):
        return r.get(e.x, Code(e))
    elif isinstance(e, Lam):
        return Fun(lambda y: spec(e.e, {**r, e.x: y}))
    elif isinstance(e, App):
        f, a = spec(e.f, r), spec(e.a, r)
        if isinstance(f, Fun):
            return f.f(a)
        else:
            raise TypeError("Type error")
    elif isinstance(e, LamD):
        xx = gensym()
        r[e.x] = Code(Var(xx))
        body = spec(e.e, r)
        if isinstance(body, Code):
            return Code(Lam(xx, body.e))
        else:
            raise TypeError("Type error")
    elif isinstance(e, AppD):
        f, a = spec(e.f, r), spec(e.a, r)
        if isinstance(f, Code) and isinstance(a, Code):
            return Code(App(f.e, a.e))
        else:
            raise TypeError("Type error")