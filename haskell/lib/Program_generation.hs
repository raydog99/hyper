import Data.Map (Map, empty, insert, findWithDefault)

type Ident = String

data Envcomp = Var Ident
             | Lam Ident Envcomp
             | App Envcomp Envcomp
             | LamD Ident Envcomp
             | AppD Envcomp Envcomp
             deriving (Show)

data Exp = Var' Ident
         | Lam' Ident Exp
         | App' Exp Exp
         | LamD' Ident Exp
         | AppD' Exp Exp
         deriving (Show)

data Value = Fun (Value -> Value)
           | Code Exp
           deriving (Show)

spec :: Exp -> Map Ident Value -> Value
spec (Var' x) r = findWithDefault (Code (Var' x)) x r
spec (Lam' x e) r = Fun (\y -> spec e (insert x y r))
spec (App' f a) r = case (spec f r, spec a r) of
                      (Fun ff, a') -> ff a'
                      _ -> error "Type error"
spec (LamD' x e) r = let xx = gensym ()
                         body = spec e (insert x (Code (Var' xx)) r)
                     in Code (Lam' xx body)
spec (AppD' f a) r = case (spec f r, spec a r) of
                       (Code ff, Code aa) -> Code (App' ff aa)
                       _ -> error "Type error"