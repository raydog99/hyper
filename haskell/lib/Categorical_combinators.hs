data Term a = Var Int | App (Term a) (Term a) | Lam String (Term a)

eval :: Term a -> [(String, a)] -> a
eval (Var n) env = env !! n
eval (App t1 t2) env = let v1 = eval t1 env in let v2 = eval t2 env in v1 v2
eval (Lam x t) env = \v -> eval t ((x, v) : env)

fix :: (Term a -> Term a) -> Term a
fix f = Lam "x" (App (f (Var 0)) (Var 0))

id = Lam "x" (Var 0)

k = Lam "x" (Lam "y" (Var 0))

b = Lam "x" (Lam "y" (Lam "z" (Var 1)))