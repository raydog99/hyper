package combinators

import "fmt"

type Ident = string

type Envcomp interface{}

type Var struct {
    x Ident
}

type Lam struct {
    x Ident
    e Envcomp
}

type App struct {
    f, a Envcomp
}

type LamD struct {
    x Ident
    e Envcomp
}

type AppD struct {
    f, a Envcomp
}

type Exp interface{}

type Var' struct {
    x Ident
}

type Lam' struct {
    x Ident
    e Exp
}

type App' struct {
    f, a Exp
}

type LamD' struct {
    x Ident
    e Exp
}

type AppD' struct {
    f, a Exp
}

type Value interface{}

type Fun struct {
    f func(Value) Value
}

type Code struct {
    e Exp
}

func spec(e Exp, r map[Ident]Value) Value {
    switch exp := e.(type) {
    case Var':
        if v, ok := r[exp.x]; ok {
            return v
        } else {
            return Code{Var'{exp.x}}
        }
    case Lam':
        return Fun{func(y Value) Value {
            r[exp.x] = y
            return spec(exp.e, r)
        }}
    case App':
        f, a := spec(exp.f, r), spec(exp.a, r)
        if ff, ok := f.(Fun); ok {
            return ff.f(a)
        } else {
            panic("Type error")
        }
    case LamD':
        xx := gensym()
        r[exp.x] = Code{Var'{xx}}
        body := spec(exp.e, r)
        return Code{Lam'{xx, body.(Code).e}}
    case AppD':
        f, a := spec(exp.f, r), spec(exp.a, r)
        ff, ok1 := f.(Code)
        aa, ok2 := a.(Code)
        if ok1 && ok2 {
            return Code{App'{ff.e, aa.e}}
        } else {
            panic("Type error")
        }
    default:
        panic("Invalid expression")
    }
}