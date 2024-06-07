module RightKanExtension where

import Control.Applicative (liftA2)
import Data.Functor.Identity (Identity(..))

newtype Ran g h a = Ran { unRan :: forall b. (b -> g a) -> h b -> Ran g h a }

instance Functor (Ran g h) where
  fmap f (Ran ran) = Ran (\k -> fmap f . ran k)

instance Applicative (Ran g h) where
  pure a = Ran (\k -> pure (k (pure a)))
  Ran ranf <*> Ran ranx = Ran (\k -> liftA2 id (ranf (fmap fst k)) (ranx (fmap snd k)))

toRan :: (h a -> t a) -> Ran g h a -> t a
toRan f (Ran ran) = f (ran id)

fromRan :: (Ran g h a -> t a) -> h a -> t a
fromRan s h = s (Ran (\k -> k id h))

adjointToRan :: Adjunction t g -> g a -> Ran t Identity a
adjointToRan Adjunction{..} g = Ran (\k -> k (runIdentity . adjunctUnit g))

ranToAdjoint :: Adjunction t g -> Ran t Identity a -> t a
ranToAdjoint Adjunction{..} (Ran ran) = adjunctRightAdjoint (ran runIdentity)

ranToComposedAdjoint :: Adjunction t g -> Ran t h a -> h (t a)
ranToComposedAdjoint Adjunction{..} (Ran ran) = fmap adjunctRightAdjoint (ran id)

composedAdjointToRan :: Adjunction t g -> h (g a) -> Ran t h a
composedAdjointToRan Adjunction{..} h = Ran (\k -> k adjunctUnit h)

composeRan :: Functor.Composition t g -> Ran t (Ran g h) a -> Ran (Functor.Compose t g) h a
composeRan Functor.Composition{..} (Ran ran) = Ran (\k -> ran (\f -> unCompose . fmap f . compose) k)

decomposeRan :: Functor.Composition t g -> Ran (Functor.Compose t g) h a -> Ran t (h (g a))
decomposeRan Functor.Composition{..} (Ran ran) = Ran (\k -> ran (\f -> k (compose . fmap f . runIdentity)))

gran :: h a -> Ran Identity h a
gran h = Ran (\k -> k runIdentity h)