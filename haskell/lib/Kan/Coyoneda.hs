module Coyoneda where

import Control.Applicative

newtype Coyoneda f a = Coyoneda { unCoyoneda :: forall b. (a -> b) -> f b }

instance Functor (Coyoneda f) where
  fmap f (Coyoneda m) = Coyoneda $ \k -> fmap (k . f) (m k)

lowerCoyoneda :: Functor f => f a -> Coyoneda f a
lowerCoyoneda fa = Coyoneda $ \k -> fmap k fa

coyoneda :: (Functor f) => (a -> b) -> f b -> Coyoneda f a
coyoneda f m = Coyoneda $ \k -> fmap (k . f) m