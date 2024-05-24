type Id = String

data Term = Var Id | Term Id [Term]

type Substitution = [(Id, Term)]

occurs :: Id -> Term -> Bool
occurs x (Var y) = x == y
occurs x (Term _ ts) = any (occurs x) ts

subst :: Term -> Id -> Term -> Term
subst s x (Var y) = if x == y then s else Var y
subst s x (Term f ts) = Term f (map (subst s x) ts)

apply :: Substitution -> Term -> Term
apply s t = foldr (\(x, u) -> subst u x) t s

unifyOne :: Term -> Term -> Substitution
unifyOne (Var x) (Var y)
  | x == y = []
  | otherwise = [(x, Var y)]
unifyOne (Term f sc) (Term g tc)
  | f == g && length sc == length tc = unify $ zip sc tc
  | otherwise = error "not unifiable: head symbol conflict"
unifyOne (Var x) t@(Term _ _)
  | occurs x t = error "not unifiable: circularity"
  | otherwise = [(x, t)]
unifyOne t@(Term _ _) (Var x)
  | occurs x t = error "not unifiable: circularity"
  | otherwise = [(x, t)]

unify :: [(Term, Term)] -> Substitution
unify [] = []
unify ((x, y):ts) = let t2 = unify ts
                        t1 = unifyOne (apply t2 x) (apply t2 y)
                    in t1 ++ t2