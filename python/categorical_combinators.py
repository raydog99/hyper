from typing import Any, Callable

class Term:
  def __init__(self, value):
    self.value = value

  def eval(self, env: dict[str, Any]) -> Any:
    match self.value:
      case int(n):
        return env[str(n)]
      case (f, arg):
        return f.eval(env)(arg.eval(env))
      case (var, body):
        new_env = {**env, var: arg}
        return body.eval(new_env)

def fix(f: Callable[[Term], Term]) -> Term:
  return Term((lambda x: f(Term((x, Term(x))))))

def id() -> Callable[[Any], Any]:
  return lambda x: x

def k() -> Callable[[Any], Callable[[Any], Any]]:
  return lambda x: lambda y: x

def b() -> Callable[[Any], Callable[[Any], Callable[[Any], Any]]]:
  return lambda x: lambda y: lambda z: y