{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module LeftAdjoint
  ( LeftAdjoint (LeftAdjoint, runLeftAdjoint)
  , fmap
  , liftA2
  , apply
  , pure
  ) where

import Control.Applicative (liftA2)
import qualified Control.Category as C
import Control.Monad ((>=>))
import Data.Functor.Identity (Identity (Identity))

newtype LeftAdjoint g h a = LeftAdjoint { runLeftAdjoint :: g ((->) a) -> h () }

instance (Functor g, Functor h) => Functor (LeftAdjoint g h) where
  fmap f (LeftAdjoint c) = LeftAdjoint $ \g -> fmap (fmap f) (c (fmap (fmap ($ ())) g))

instance (Applicative g, Applicative h) => Applicative (LeftAdjoint g h) where
  pure a = LeftAdjoint $ \g -> pure (const a) <*> g
  LeftAdjoint f <*> LeftAdjoint x = LeftAdjoint $ \g -> liftA2 (liftA2 ($)) (f (fmap (fmap (,)) g)) (x (fmap (fmap snd) g))

liftA2 :: (Applicative g, Applicative h) => (a -> b -> c) -> LeftAdjoint g h a -> LeftAdjoint g h b -> LeftAdjoint g h c
liftA2 f (LeftAdjoint c1) (LeftAdjoint c2) = LeftAdjoint $ \g ->
  liftA2 (liftA2 ($)) (c1 (fmap (fmap (,)) g)) (c2 (fmap (fmap snd) g)) >>= \f' x' ->
    pure (f' x')

apply :: (Applicative g, Applicative h) => LeftAdjoint g h (a -> b) -> LeftAdjoint g h a -> LeftAdjoint g h b
apply = liftA2 ($)

data Adjunction f u = Adjunction
  { leftAdjunct :: forall x. (f x -> u x) -> u x
  , rightAdjunct :: forall x. u x -> f x
  }

adjointToLeftAdjoint :: (Adjunction f u) => u a -> LeftAdjoint f Identity a
adjointToLeftAdjoint ua = LeftAdjoint $ \g -> Identity $ rightAdjunct (fmap ($()) ua) g

leftAdjointToAdjoint :: (Adjunction f u) => LeftAdjoint f Identity a -> u a
leftAdjointToAdjoint (LeftAdjoint c) = leftAdjunct $ \g -> runIdentity $ c (Identity . g)