package capitan

import "fmt"

type LeftAdjoint struct {
    f func(interface{}) interface{}
    g func(interface{}) interface{}
}

func NewLeftAdjoint(f, g func(interface{}) interface{}) *LeftAdjoint {
    return &LeftAdjoint{f, g}
}

func (la *LeftAdjoint) FMap(f func(interface{}) interface{}) *LeftAdjoint {
    return NewLeftAdjoint(
        func(a interface{}) interface{} {
            return f(la.f(a))
        },
        la.g,
    )
}

func (la *LeftAdjoint) Apply(other *LeftAdjoint) *LeftAdjoint {
    return NewLeftAdjoint(
        func(a interface{}) interface{} {
            return la.f(a).(func(interface{}) interface{})(other.f(a))
        },
        func(a interface{}) interface{} {
            return la.g(other.g(a))
        },
    )
}

func (la *LeftAdjoint) Pure(a interface{}) *LeftAdjoint {
    return NewLeftAdjoint(
        func(b interface{}) interface{} {
            return b.(func(interface{}) interface{})(a)
        },
        func(c interface{}) interface{} {
            return c
        },
    )
}

type Adjunction struct {
    LeftAdjunct  func(interface{}) interface{}
    RightAdjunct func(interface{}) interface{}
}

func AdjointToLeftAdjoint(adj Adjunction, u interface{}) *LeftAdjoint {
    return NewLeftAdjoint(
        func(g interface{}) interface{} {
            return adj.RightAdjunct(g.(func(interface{}) interface{})(u))
        },
        func(interface{}) interface{} {
            return fmt.Errorf("not implemented")
        },
    )
}

func LeftAdjointToAdjoint(la *LeftAdjoint) Adjunction {
    return Adjunction{
        LeftAdjunct: func(f interface{}) interface{} {
            return la.f(f)
        },
        RightAdjunct: func(interface{}) interface{} {
            return fmt.Errorf("not implemented")
        },
    }
}