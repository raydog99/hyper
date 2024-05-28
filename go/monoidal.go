package combinators

type Object interface{}
type Morphism interface {
    Apply(Object) Object
}

type MonoidalCategory struct{}

func (mc MonoidalCategory) Id(obj Object) Morphism {
    return IdMorphism{obj}
}

func (mc MonoidalCategory) Compose(f, g Morphism) Morphism {
    return ComposedMorphism{f, g}
}

func (mc MonoidalCategory) Tensor(a, b Object) Object {
    return TensorObject{a, b}
}

func (mc MonoidalCategory) TensorMor(f, g Morphism) Morphism {
    return TensorMorphism{f, g}
}

type IdMorphism struct {
    Object Object
}

func (m IdMorphism) Apply(obj Object) Object {
    return m.Object
}

type ComposedMorphism struct {
    F, G Morphism
}

func (m ComposedMorphism) Apply(obj Object) Object {
    return m.F.Apply(m.G.Apply(obj))
}

type TensorObject struct {
    A, B Object
}

type TensorMorphism struct {
    F, G Morphism
}

func (m TensorMorphism) Apply(obj Object) Object {
    pair := obj.(TensorObject)
    return TensorObject{m.F.Apply(pair.A), m.G.Apply(pair.B)}
}