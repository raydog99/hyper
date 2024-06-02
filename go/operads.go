package categories

type Operad struct {
    Colors      []interface{}
    Operations  map[interface{}]int
    Composition func(interface{}, int, []int) (int, bool)
    Identity    func(interface{}) int
}

func MakeOperad(colors []interface{}, operations []interface{}, composition func(interface{}, int, []int) (int, bool), identity func(interface{}) int) Operad {
    ops := make(map[interface{}]int)
    for i, op := range operations {
        ops[op] = i
    }
    return Operad{colors, ops, composition, identity}
}

func FindOperation(operad Operad, profile interface{}) (int, bool) {
    op, ok := operad.Operations[profile]
    return op, ok
}

func Compose(operad Operad, opIndex int, operands []int) (int, bool) {
    return operad.Composition(nil, opIndex, operands)
}

func IdentityOp(operad Operad, color interface{}) int {
    return operad.Identity(color)
}