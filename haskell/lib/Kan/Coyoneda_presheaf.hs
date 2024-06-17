module CoyonedaPresheaf where

import Data.Functor.Contravariant

newtype Coyoneda f a = Coyoneda { runCoyoneda :: (a -> b) -> f b }

contramap :: (b -> a) -> Coyoneda f a -> Coyoneda f b
contramap f (Coyoneda g) = Coyoneda (\k -> g (k . f))

lift :: f a -> Coyoneda f a
lift x = Coyoneda (\k -> fmap k x)

lower :: Contravariant f => Coyoneda f a -> f a
lower (Coyoneda g) = contramap id g

hoist :: (f a -> g a) -> Coyoneda f b -> Coyoneda g b
hoist n (Coyoneda g) = Coyoneda (\k -> n (g (k . id)))

type Presheaf a = a -> Int

contramap' :: (b -> a) -> Presheaf a -> Presheaf b
contramap' f g = g . f

tabulate :: a -> Presheaf a
tabulate = const