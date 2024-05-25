import Data.List
import Data.Vect

%default total

data Term : Type where
  Var : String -> Term
  Term : String -> Vect n Term -> Term

Term2Str : Term -> String
Term2Str (Var x) = x
Term2Str (Term f args) = f ++ "(" ++ showSep "," (map Term2Str (toList args)) ++ ")"

Substitution : Type
Substitution = List (String, Term)

occurs : String -> Term -> Bool
occurs x (Var y) = x == y
occurs x (Term _ args) = any (occurs x) (toList args)

subst : Term -> String -> Term -> Term
subst s x (Var y) = if x == y then s else Var y
subst s x (Term f args) = Term f (map (subst s x) (toList args))

apply : Substitution -> Term -> Term
apply s t = foldl (\ t' => subst t' . snd) t s

unifyOne : Term -> Term -> Maybe Substitution
unifyOne (Var x) (Var y) =
  if x == y
    then pure []
    else pure [(x, Var y)]
unifyOne (Term f sc) (Term g tc) =
  if f /= g || length sc /= length tc
    then Nothing
    else foldl (>>=) (pure []) $ zipWith unifyOne (apply [] <$> toList sc) (apply [] <$> toList tc)
unifyOne (Var x) t@(Term _ _) =
  if occurs x t
    then Nothing
    else pure [(x, t)]
unifyOne t@(Term _ _) (Var x) =
  if occurs x t
    then Nothing
    else pure [(x, t)]

unify : List (Term, Term) -> Maybe Substitution
unify [] = pure []
unify ((x, y) :: ts) = do
  s2 <- unify ts
  s1 <- unifyOne (apply s2 x) (apply s2 y)
  pure (s1 ++ s2)