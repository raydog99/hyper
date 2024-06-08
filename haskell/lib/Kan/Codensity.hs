module Codensity where

newtype Codensity m a = Codensity { runCodensity :: (a -> m b) -> m b }

instance Monad m => Monad (Codensity m) where
  return x = Codensity ($ x)
  m >>= f = Codensity (\k -> runCodensity m (\a -> runCodensity (f a) k))

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity m = runCodensity m return

reset :: Monad m => Codensity m a -> m a
reset = lowerCodensity

shift :: Monad m => ((a -> Codensity m b) -> Codensity m a) -> Codensity m a
shift f = Codensity (\k -> runCodensity (f (\a -> Codensity k)) return)