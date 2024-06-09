package capitan

type Rift struct {
    runRift func(bac func(b interface{}) (interface{}, interface{}), gc interface{}) interface{}
}

func (r Rift) ContraMap(f func(a interface{}) interface{}) Rift {
    return Rift{
        runRift: func(bac func(b interface{}) (interface{}, interface{}), gc interface{}) interface{} {
            return r.runRift(func(b interface{}) (interface{}, interface{}) {
                a, c := bac(b)
                return f(a), c
            }, gc)
        },
    }
}

func Divide(f func(a interface{}) interface{}, g Rift, h Rift) Rift {
    return Rift{
        runRift: func(bac func(b interface{}) (interface{}, interface{}), gc interface{}) interface{} {
            return g.runRift(func(a interface{}) (interface{}, interface{}) {
                c, d := bac(a)
                return f(c), d
            }, gc)
        },
    }
}