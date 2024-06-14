module Yoneda

data Yoneda : (f : Type -> Type) -> (a : Type) -> Type where
  MkYoneda : ((r : Type) -> (a -> r) -> f r) -> Yoneda f a

runYoneda : Yoneda f a -> ((r : Type) -> (a -> r) -> f r)
runYoneda (MkYoneda y) = y

liftYoneda : f a -> Yoneda f a
liftYoneda fa = MkYoneda (\r, k => fa >>= k)

lowerYoneda : Yoneda f a -> f a
lowerYoneda y = runYoneda y _ id

contramap : (g : b -> a) -> Yoneda f a -> Yoneda f b
contramap g (MkYoneda y) = MkYoneda (\r, k => y r (k . g))