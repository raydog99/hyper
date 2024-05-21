data Term a = Var Nat | App (Term a) (Term a) | Lam String (Term a)

eval : Term a -> Env a -> a
eval (Var n) env = env! n
eval (App t1 t2) env = eval t1 env (eval t2 env)
eval (Lam x body) env = \v -> eval body (Env.insert env x v)

fix : (Term a -> Term a) -> Term a
fix f = Lam "x" (App (f (Var 0)) (Var 0))

id : a -> a
id = \x -> x

k : a -> a -> a
k = \x -> \y -> x

b : a -> a -> a -> a
b = \x -> \y -> \z -> y