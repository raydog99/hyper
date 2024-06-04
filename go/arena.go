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

func duoidal(a1, a2, b1, b2 Arena) Lens {
    x := Pair(Combine(a1, a2), Combine(b1, b2))
    y := Combine(Pair(a1, b1), Pair(a2, b2))

    o := func(p Pos) Pos {
        p1, p2 := p.First, p.Second
        q1, q2 := p1.First, p1.Second

        pp := func(d Dis) Pos {
            return Pair(Combine(d.First, Apply(p2, d.First)), Combine(d.Second, Apply(q2, d.Second)))
        }

        return Pair(Pair(p1, q1), pp)
    }

    i := func(p Pos) func(d Dis) Dis {
        p1, p2 := p.First, p.Second
        q1, q2 := p1.First, p1.Second

        return func(d Dis) Dis {
            de1, de2 := d.First, d.Second

            return Pair(Pair(de1.First, de2.First), Pair(de1.Second, de2.Second))
        }
    }

    return Lens{o, i}
}