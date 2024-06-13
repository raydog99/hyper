module Density

import Data.Unit

%default total

public export
record Density : Type -> Type where
  constructor MkDensity
  unDensity : (forall b . (a -> b) -> b)

public export
return : a -> Density a
return a = MkDensity (\f => f a)

public export
bind : Density a -> (a -> Density b) -> Density b
bind (MkDensity f) k = MkDensity (\g => let (MkDensity h) = k (f id) in h g)

public export
map : (a -> b) -> Density a -> Density b
map f (MkDensity g) = MkDensity (\h => g (\x => h (f x)))

public export
extract : Density a -> a
extract (MkDensity f) = f id

public export
duplicate : Density a -> Density (Density a)
duplicate (MkDensity f) = MkDensity (\g => let h = (\x => g (MkDensity (\k => k x))) in f h)

public export
extend : (Density a -> b) -> Density a -> Density b
extend f (MkDensity g) = MkDensity (\h => let h' = (\x => h (f (MkDensity (\k => k x)))) in g h')