package efficient

import (
	"fmt"
)

type Term interface {
	Eval(env map[string]interface{}) interface{}
}

type Var struct {
	Index int
}

func (v Var) Eval(env map[string]interface{}) interface{} {
	return env[fmt.Sprintf("%d", v.Index)]
}

type App struct {
	Fn Term
	Arg Term
}

func (a App) Eval(env map[string]interface{}) interface{} {
	fn := a.Fn.Eval(env).(func(interface{}) interface{})
	return fn(a.Arg.Eval(env))
}

type Lam struct {
	Var  string
	Body Term
}

func (l Lam) Eval(env map[string]interface{}) interface{} {
	return func(arg interface{}) interface{} {
		newEnv := make(map[string]interface{})
		for k, v := range env {
			newEnv[k] = v
		}
		newEnv[l.Var] = arg
		return l.Body.Eval(newEnv)
	}
}

func fix(t func(Term) Term) Term {
	return Lam{"x", App(t(Var{0}), Var{0})}
}

func id() func(interface{}) interface{} {
	return func(x interface{}) interface{} { return x }
}

func k() func(interface{}) func(interface{}) interface{} {
	return func(x interface{}) func(interface{}) interface{} {
		return func(y interface{}) interface{} { return x }
	}
}

func b() func(interface{}) func(interface{}) func(interface{}) interface{} {
	return func(x interface{}) func(interface{}) func(interface{}) interface{} {
		return func(y interface{}) func(interface{}) interface{} {
			return func(z interface{}) interface{} { return y }
		}
	}
}