package capitan

type Coyoneda struct {
    f func(interface{}) interface{}
    m interface{}
}

func NewCoyoneda(f func(interface{}) interface{}, m interface{}) *Coyoneda {
    return &Coyoneda{f, m}
}

func (c *Coyoneda) ContraMap(g func(interface{}) interface{}) *Coyoneda {
    return &Coyoneda{
        f: func(x interface{}) interface{} {
            return g(c.f(x))
        },
        m: c.m,
    }
}