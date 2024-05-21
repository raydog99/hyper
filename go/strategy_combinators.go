package efficient

type TP struct {
    apply func(interface{}) interface{}
}

type TU struct {
    apply func(interface{}) interface{}
}

// Strategy application
func (t TP) ApplyTP(m interface{}) interface{} {
    return t.apply(m)
}

func (t TU) ApplyTU(m interface{}) interface{} {
    return t.apply(m)
}

// Strategy construction
func PolyTP(m interface{}) TP {
    return TP{func(x interface{}) interface{} { return m }}
}

func PolyTU(m interface{}) TU {
    return TU{func(x interface{}) interface{} { return m }}
}

func AdhocTP(m interface{}, t interface{}) TP {
    return TP{func(x interface{}) interface{} {
        return (PolyTP(m).ApplyTP(t)).(func(interface{}) interface{})(x)
    }}
}

func AdhocTU(m interface{}, t interface{}) TU {
    return TU{func(x interface{}) interface{} {
        return (PolyTU(m).ApplyTU(t)).(func(interface{}) interface{})(x)
    }}
}

// Sequential composition
func SeqTP(m TP) TP {
    return TP{func(x interface{}) interface{} {
        return m.ApplyTP(x)
    }}
}

func LetTP(m TU) TP {
    return TP{func(a interface{}) interface{} {
        return m.ApplyTU(a)
    }}
}

func SeqTU(m TP) TU {
    return TU{func(x interface{}) interface{} {
        return m.ApplyTP(x)
    }}
}

func LetTU(m TU) TU {
    return TU{func(a interface{}) interface{} {
        return m.ApplyTU(TU{func(b interface{}) interface{} {
            return (TU{m.apply(a)}).ApplyTU(b)
        }})
    }}
}

// Choice
func ChoiceTP(m interface{}) TP {
    return TP{func(x interface{}) interface{} {
        return m.(func(interface{}) interface{})(x)
    }}
}

func ChoiceTU(m interface{}) TU {
    return TU{func(x interface{}) interface{} {
        return m.(func(interface{}) interface{})(x)
    }}
}

// Traversal combinators
func AllTP(m interface{}) TP {
    return TP{func(x interface{}) interface{} { return x }}
}

func OneTP(m interface{}) TP {
    return TP{func(x interface{}) interface{} { return x }}
}

func AllTU(m interface{}, a interface{}) TU {
    return TU{func(x interface{}) interface{} { return x }}
}

func OneTU(m interface{}) TU {
    return TU{func(x interface{}) interface{} { return x }}
}