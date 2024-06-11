module DayConvolution where

import Control.Applicative (Applicative(..))

data Day f g a = Day (f b) (g c) (b -> c -> a)

instance (Functor f, Functor g) => Functor (Day f g) where
  fmap f (Day fb gc bca) = Day fb gc (f . bca)

instance (Functor f, Functor g) => Applicative (Day f g) where
  pure x = Day (pure ()) (pure ()) (const (const x))
  Day fa ga u <*> Day fb gb v = Day (fa <*> fb) (ga <*> gb) (\(a, c) (b, d) -> u a b (v c d))

assoc :: Day f (Day g h) a -> Day (Day f g) h a
assoc (Day f (Day g h bca) bgca) = Day (Day f g) h (\((a, b), c) -> bgca a (bca b c))

disassoc :: Day (Day f g) h a -> Day f (Day g h) a
disassoc (Day (Day f g) h bgca) = Day f (Day g h (\b c -> bgca (f, b) c))

swapped :: Day f g a -> Day g f a
swapped (Day f g bca) = Day g f (\b a -> bca a b)

intro1 :: Functor f => f a -> Day () f a
intro1 f = Day () f (const id)

intro2 :: Functor g => g a -> Day g () a
intro2 g = Day g () (const id)

elim1 :: Day () f a -> f a
elim1 (Day () f bca) = bca () f

elim2 :: Day f () a -> f a
elim2 (Day f () bca) = f

trans1 :: (Functor f, Functor g) => (a -> b) -> Day a g c -> Day b g c
trans1 f (Day a g bca) = Day (fmap f a) g bca

trans2 :: (Functor f, Functor g) => (a -> b) -> Day f a c -> Day f b c
trans2 f (Day g a bca) = Day g (fmap f a) bca