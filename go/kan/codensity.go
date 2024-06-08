package capitan

type Codensity struct {
    Value func(func(interface{}) interface{}) interface{}
}

func Return(x interface{}) Codensity {
    return Codensity{func(k func(interface{}) interface{}) interface{} {
        return k(x)
    }}
}

func (m Codensity) Bind(f func(interface{}) Codensity) Codensity {
    return Codensity{func(k func(interface{}) interface{}) interface{} {
        return m.Value(func(x interface{}) interface{} {
            return f(x).Value(k)
        })
    }}
}

func (m Codensity) Map(f func(interface{}) interface{}) Codensity {
    return Codensity{func(k func(interface{}) interface{}) interface{} {
        return m.Value(func(x interface{}) interface{} {
            return k(f(x))
        })
    }}
}

func LowerCodensity(m Codensity) interface{} {
    return m.Value(func(x interface{}) interface{} { return x })
}

func Reset(m Codensity) interface{} {
    return LowerCodensity(m)
}

func Shift(f func(func(interface{}) Codensity) Codensity) Codensity {
    aux := func(k func(interface{}) interface{}) interface{} {
        return f(func(x interface{}) Codensity {
            return Return(x)
        }).Value(k)
    }
    return Codensity{func(k func(interface{}) interface{}) interface{} {
        return aux(k)
    }}
}