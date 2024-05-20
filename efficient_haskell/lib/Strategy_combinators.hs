module Strategies where

import Control.Monad

-- Strategy types
newtype TP m a = TP { unTP :: m -> a }
newtype TU m a = TU { unTU :: m -> a }

-- Strategy application
applyTP :: TP m a -> m -> a
applyTP (TP f) = f

applyTU :: TU m a -> m -> a
applyTU (TU f) = f

-- Strategy construction
polyTP :: Monad m => m a -> TP m a
polyTP m = TP (>>= id)

polyTU :: Monad m => m a -> TU m a
polyTU m = TU (>>= id)

adhocTP :: Monad m => m (a -> b) -> a -> TP m b
adhocTP m a = TP $ (m >>= ($ a))

adhocTU :: Monad m => m (a -> b) -> a -> TU m b
adhocTU m a = TU $ (m >>= ($ a))

-- Sequential composition
seqTP :: Monad m => TP m (TP m a) -> TP m a
seqTP (TP f) = TP $ (f >>= unTP)

letTP :: Monad m => TU m (TP m a) -> TP m a
letTP (TU f) = TP $ (f >>= unTP)

seqTU :: Monad m => TP m (TU m a) -> TU m a
seqTU (TP f) = TU $ (f >>= unTU)

letTU :: Monad m => TU m (TU m a) -> TU m a
letTU (TU f) = TU $ (f >>= unTU)

-- Choice
choiceTP :: MonadPlus m => m (TP m a) -> TP m a
choiceTP m = TP $ (m >>= unTP)

choiceTU :: MonadPlus m => m (TU m a) -> TU m a
choiceTU m = TU $ (m >>= unTU)

-- Traversal combinators
allTP :: Monad m => TP m a -> TP m a
allTP = id

oneTP :: MonadPlus m => TP m a -> TP m a
oneTP = id

allTU :: Monad m => TU m a -> TU m a
allTU = id

oneTU :: MonadPlus m => TU m a -> TU m a
oneTU = id