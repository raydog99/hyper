module Strategies

%default total

data TP : (m : Type) -> (a : Type) -> Type where
  MkTP : (apply : m -> a) -> TP m a

data TU : (m : Type) -> (a : Type) -> Type where
  MkTU : (apply : m -> a) -> TU m a

-- Strategy application
applyTP : TP m a -> m -> a
applyTP (MkTP f) m = f m

applyTU : TU m a -> m -> a
applyTU (MkTU f) m = f m

-- Strategy construction
polyTP : Monad m => m a -> TP m a
polyTP m = MkTP (>>= id)

polyTU : Monad m => m a -> TU m a
polyTU m = MkTU (>>= id)

adhocTP : (Monad m, Functor m) => m (a -> b) -> a -> TP m b
adhocTP m a = MkTP (\x => (m >>= (flip ($) a)) x)

adhocTU : (Monad m, Functor m) => m (a -> b) -> a -> TU m b
adhocTU m a = MkTU (\x => (m >>= (flip ($) a)) x)

-- Sequential composition
seqTP : (Monad m, Applicative m) => TP m (TP m a) -> TP m a
seqTP (MkTP f) = MkTP (\x => applyTP (f x) x)

letTP : (Monad m, Applicative m) => TU m (TP m a) -> TP m a
letTP (MkTU f) = MkTP (\x => applyTP (f x) x)

seqTU : (Monad m, Applicative m) => TP m (TU m a) -> TU m a
seqTU (MkTP f) = MkTU (\x => applyTU (f x) x)

letTU : (Monad m, Applicative m) => TU m (TU m a) -> TU m a
letTU (MkTU f) = MkTU (\x => applyTU (f x) (applyTU (f x) x))

-- Choice
choiceTP : (MonadPlus m, Functor m) => m (TP m a) -> TP m a
choiceTP m = MkTP (\x => applyTP (join $ fmap MkTP m) x)

choiceTU : (MonadPlus m, Functor m) => m (TU m a) -> TU m a
choiceTU m = MkTU (\x => applyTU (join $ fmap MkTU m) x)

-- Traversal combinators
allTP : Monad m => TP m a -> TP m a
allTP = id

oneTP : MonadPlus m => TP m a -> TP m a
oneTP = id

allTU : Monad m => TU m a -> TU m a
allTU = id

oneTU : MonadPlus m => TU m a -> TU m a
oneTU = id