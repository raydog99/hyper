import Data.SortedMap

%default total

public export
Ident : Type
Ident = String

public export
data Envcomp : Type where
  Var : Ident -> Envcomp
  Lam : Ident -> Envcomp -> Envcomp
  App : Envcomp -> Envcomp -> Envcomp
  LamD : Ident -> Envcomp -> Envcomp
  AppD : Envcomp -> Envcomp -> Envcomp

public export
data Exp : Type where
  Var' : Ident -> Exp
  Lam' : Ident -> Exp -> Exp
  App' : Exp -> Exp -> Exp
  LamD' : Ident -> Exp -> Exp
  AppD' : Exp -> Exp -> Exp

public export
data Value : Type where
  Fun : (Value -> Value) -> Value
  Code : Exp -> Value

public export
spec : Exp -> SortedMap Ident Value -> Value
spec (Var' x) r = case lookup x r of
                    Nothing => Code (Var' x)
                    Just v => v
spec (Lam' x e) r = Fun (\y => spec e (insert x y r))
spec (App' f a) r = case (spec f r, spec a r) of
                      (Fun ff, a') => ff a'
                      _ => Code (App' f a)
spec (LamD' x e) r = let xx = gensym in
                         let body = spec e (insert x (Code (Var' xx)) r) in
                         Code (LamD' xx body)
spec (AppD' f a) r = case (spec f r, spec a r) of
                       (Code ff, Code aa) => Code (AppD' ff aa)
                       _ => Code (AppD' f a)