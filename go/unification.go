package combinators

import (
	"fmt"
	"strings"
)

type Id = string

type Term struct {
	id   Id
	args []Term
}

func (t Term) isVar() bool {
	return len(t.args) == 0
}

type Substitution map[Id]Term

func occurs(x Id, t Term) bool {
	if t.isVar() {
		return x == t.id
	}
	for _, arg := range t.args {
		if occurs(x, arg) {
			return true
		}
	}
	return false
}

func subst(s Term, x Id, t Term) Term {
	if t.isVar() {
		if x == t.id {
			return s
		}
		return t
	}
	newArgs := make([]Term, len(t.args))
	for i, arg := range t.args {
		newArgs[i] = subst(s, x, arg)
	}
	return Term{t.id, newArgs}
}

func apply(s Substitution, t Term) Term {
	if t.isVar() {
		if sub, ok := s[t.id]; ok {
			return sub
		}
		return t
	}
	newArgs := make([]Term, len(t.args))
	for i, arg := range t.args {
		newArgs[i] = apply(s, arg)
	}
	return Term{t.id, newArgs}
}

func unifyOne(s, t Term) (Substitution, bool) {
	if s.isVar() {
		if t.isVar() {
			if s.id == t.id {
				return nil, true
			}
			return Substitution{s.id: t}, true
		}
		if occurs(s.id, t) {
			return nil, false
		}
		return Substitution{s.id: t}, true
	}
	if t.isVar() {
		if occurs(t.id, s) {
			return nil, false
		}
		return Substitution{t.id: s}, true
	}
	if s.id != t.id || len(s.args) != len(t.args) {
		return nil, false
	}
	sub := make(Substitution)
	for i := range s.args {
		s1, ok := unifyOne(s.args[i], t.args[i])
		if !ok {
			return nil, false
		}
		for k, v := range s1 {
			sub[k] = v
		}
	}
	return sub, true
}

func unify(pairs []Term) (Substitution, bool) {
	sub := make(Substitution)
	for i := range pairs {
		s, t := pairs[i], pairs[i]
		s1, ok := unifyOne(apply(sub, s), apply(sub, t))
		if !ok {
			return nil, false
		}
		for k, v := range s1 {
			sub[k] = v
		}
	}
	return sub, true
}

func main() {
	x, y := Term{"x", nil}, Term{"y", nil}
	f := Term{"f", []Term{x, y}}
	g := Term{"g", []Term{y, x}}
	pairs := []Term{f, g}
	sub, ok := unify(pairs)
	if !ok {
		fmt.Println("Not unifiable")
		return
	}
	for k, v := range sub {
		fmt.Printf("%s => %v\n", k, v)
	}
}