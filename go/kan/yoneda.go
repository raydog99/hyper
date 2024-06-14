package capitan

type Yoneda struct {
    RunYoneda func(interface{}) interface{}
}

func LiftYoneda(fa interface{}) Yoneda {
    return Yoneda{func(k interface{}) interface{} {
        return k.(func(interface{}) interface{})(fa)
    }}
}

func LowerYoneda(m Yoneda) interface{} {
    return m.RunYoneda(func(x interface{}) interface{} { return x })
}

func Contramap(g func(interface{}) interface{}, m Yoneda) Yoneda {
    return Yoneda{func(k interface{}) interface{} {
        return m.RunYoneda(func(x interface{}) interface{} {
            return k(g(x))
        })
    }}
}