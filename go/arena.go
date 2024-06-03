package categories

type Arena struct {
    pos interface{}
    dis func(interface{}) interface{}
}

type Lens struct {
    dom, cod Arena
    observe  func(interface{}) interface{}
    interpret func(interface{}, func(interface{}) interface{}) interface{}
}

func idLens(a Arena) Lens {
    return Lens{
        dom: a, cod: a,
        observe:  func(p interface{}) interface{} { return p },
        interpret: func(p interface{}, f func(interface{}) interface{}) interface{} { return f(p) },
    }
}

func (lens23 Lens) compose(lens12 Lens) Lens {
    obs := func(p interface{}) interface{} { return lens23.observe(lens12.observe(p)) }
    int := func(p interface{}, f func(interface{}) interface{}) interface{} {
        return lens12.interpret(p, func(x interface{}) interface{} {
            return lens23.interpret(lens12.observe(p), f.(func(interface{}) interface{}))
        })
    }
    return Lens{dom: lens12.dom, cod: lens23.cod, observe: obs, interpret: int}
}

type Display interface {
    pos() interface{}
    dis(interface{}) interface{}
}

type AsFunctor struct {
    pos interface{}
    dis func(interface{}) interface{}
}