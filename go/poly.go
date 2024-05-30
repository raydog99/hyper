package combinators

type Poly []float64

func CreatePoly(coeffs []float64) Poly {
    return Poly(coeffs)
}

func (p Poly) Eval(x float64) float64 {
    result := 0.0
    for i, coeff := range p {
        term := coeff * math.Pow(x, float64(i))
        result += term
    }
    return result
}

func (p Poly) Map(f func(float64) float64) Poly {
    result := make(Poly, len(p))
    for i, coeff := range p {
        result[i] = f(coeff)
    }
    return result
}

func (p Poly) Sum(q Poly) Poly {
    maxLen := max(len(p), len(q))
    result := make(Poly, maxLen)
    for i := range result {
        if i < len(p) {
            result[i] += p[i]
        }
        if i < len(q) {
            result[i] += q[i]
        }
    }
    return result
}

func (p Poly) Product(q Poly) Poly {
    result := make(Poly, len(p)+len(q)-1)
    for i, coeff1 := range p {
        for j, coeff2 := range q {
            result[i+j] += coeff1 * coeff2
        }
    }
    return result
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

type Comonoid interface {
    Counit() interface{}
    Comult() (interface{}, interface{})
}

type Poly []PolyTerm

type PolyTerm struct {
    Coeff interface{}
    Exp   int
}

func (p Poly) Counit() interface{} {
    if len(p) == 0 {
        return nil
    }
    if p[0].Exp == 0 {
        return p[0].Coeff.(Comonoid).Counit()
    }
    panic("Polynomial must be constant to have a counit")
}

func (p Poly) Comult() (Poly, Poly) {
    var xs, ys Poly
    for _, term := range p {
        if term.Exp == 0 {
            c1, c2 := term.Coeff.(Comonoid).Comult()
            xs = append(xs, PolyTerm{c1, 0})
            ys = append(ys, PolyTerm{c2, 0})
        } else {
            xs = append(xs, term)
            ys = append(ys, PolyTerm{term.Coeff.(Comonoid).Counit(), term.Exp})
        }
    }
    return xs, ys
}