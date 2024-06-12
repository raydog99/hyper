module Coyoneda

import Data.Functor

%default total

public export
record Coyoneda (f : Type -> Type) (a : Type) where
  constructor MkCoyoneda
  func : (a -> b) -> f b
  val : f a

public export
coyoneda : Functor f => (a -> b) -> f b -> Coyoneda f a
coyoneda f m = MkCoyoneda (\k => map k m) m

public export
contramap : Coyoneda f a -> (b -> a) -> Coyoneda f b
contramap (MkCoyoneda func val) f = MkCoyoneda (\k => func (k . f)) val