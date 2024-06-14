module Yoneda where

newtype Yoneda f a = Yoneda { runYoneda :: forall r. (a -> r) -> f r }

liftYoneda :: f a -> Yoneda f a
liftYoneda fa = Yoneda (\k -> fmap k fa)

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda m) = m id

contramap :: (b -> a) -> Yoneda f a -> Yoneda f b
contramap g (Yoneda m) = Yoneda (\k -> m (k . g))