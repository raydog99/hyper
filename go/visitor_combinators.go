package combinators

type IndependComp struct {
    vis1, vis2 FVisitor
}

func (v *IndependComp) Before(o interface{}) FVisitor {
    v1 := v.vis1.Before(o)
    v2 := v.vis2.Before(o)
    return &IndependComp{v1, v2}
}

func (v *IndependComp) After(o interface{}, fv FVisitor) FVisitor {
    v1 := v.vis1.After(o, fv.(*IndependComp).vis1)
    v2 := v.vis2.After(o, fv.(*IndependComp).vis2)
    return &IndependComp{v1, v2}
}

type ThreadedComp struct {
    vis1, vis2 FVisitor
}

func (v *ThreadedComp) Before(o interface{}) FVisitor {
    v1 := v.vis1.Before(o)
    v2 := v.vis2.Before(o, v.vis1.ExportVisitor())
    return &ThreadedComp{v1, v2}
}

func (v *ThreadedComp) After(o interface{}, fv FVisitor) FVisitor {
    v1 := v.vis1.After(o, fv.(*ThreadedComp).vis1)
    v2 := v.vis2.After(o, fv.(*ThreadedComp).vis2)
    return &ThreadedComp{v1, v2}
}

func (v *ThreadedComp) ExportVisitor() FVisitor {
    return v.vis2.ExportVisitor()
}

type ConditionalComp struct {
    vis1, vis2 FVisitor
}

func (v *ConditionalComp) Before(o interface{}) FVisitor {
    v1 := v.vis1.Before(o)
    if v1.ContinueVisit() {
        v2 := v.vis2.Before(o)
        return &ConditionalComp{v1, v2}
    }
    return &ConditionalComp{v1, v.vis2}
}

func (v *ConditionalComp) After(o interface{}, fv FVisitor) FVisitor {
    v1 := v.vis1.After(o, fv.(*ConditionalComp).vis1)
    if v1.ContinueVisit() {
        v2 := v.vis2.After(o, fv.(*ConditionalComp).vis2)
        return &ConditionalComp{v1, v2}
    }
    return &ConditionalComp{v1, v.vis2}
}