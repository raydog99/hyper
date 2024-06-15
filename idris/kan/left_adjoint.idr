module LeftAdjoint

import Data.Functor
import Data.Applicative

%default total

LeftAdjoint : (Functor f, Functor g) => f ((->) a) -> g () -> Type
LeftAdjoint f g = f ((->) a) -> g ()

fmap : (Functor f, Functor g) => (a -> b) -> LeftAdjoint f g a -> LeftAdjoint f g b
fmap f (LeftAdjoint c) = LeftAdjoint (\g => map (map (. f)) (c (map (map ($ ())) g)))

apply : (Applicative f, Applicative g) => LeftAdjoint f g (a -> b) -> LeftAdjoint f g a -> LeftAdjoint f g b
apply (LeftAdjoint f) (LeftAdjoint x) = LeftAdjoint (\g => liftA2 (liftA2 ($)) (f (map (map (,)) g)) (x (map (map snd) g)))

pure : (Applicative f, Applicative g) => a -> LeftAdjoint f g a
pure a = LeftAdjoint (\g => pure (const a) <*> g)

liftA2 : (Applicative f, Applicative g) => (a -> b -> c) -> LeftAdjoint f g a -> LeftAdjoint f g b -> LeftAdjoint f g c
liftA2 f (LeftAdjoint c1) (LeftAdjoint c2) = LeftAdjoint (\g => liftA2 (liftA2 ($)) (c1 (map (map (,)) g)) (c2 (map (map snd) g)) >>= \f' x' => pure (f' x'))

data Adjunction : (Functor f, Functor u) => (f : Type -> Type) -> (u : Type -> Type) -> Type where
  Adj : (forall x . (f x -> u x) -> u x) -> (forall x . u x -> f x) -> Adjunction f u

adjointToLeftAdjoint : (Functor f, Functor u) => Adjunction f u -> u a -> LeftAdjoint f (const ()) a
adjointToLeftAdjoint (Adj l r) ua = LeftAdjoint (\g => (r ua) $ const $ g ())

leftAdjointToAdjoint : (Functor f, Functor u) => LeftAdjoint f (const ()) a -> Adjunction f u
leftAdjointToAdjoint (LeftAdjoint c) = Adj (\g => c (g . const ())) (\_ => ?rhs)