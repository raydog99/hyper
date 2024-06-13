module Density (
    Density(..),
    return,
    bind,
    map,
    extract,
    duplicate,
    extend
) where

newtype Density a = Density { unDensity :: (b -> a, b) }

return :: a -> Density a
return a = Density (\_ -> a, ())

bind :: Density a -> (a -> Density b) -> Density b
bind (Density (f, x)) k = let (g, y) = unDensity (k (f x)) in Density (g, y)

map :: (a -> b) -> Density a -> Density b
map f (Density (g, x)) = Density (\y -> f (g y), x)

extract :: Density a -> a
extract (Density (f, x)) = f x

duplicate :: Density a -> Density (Density a)
duplicate (Density (f, x)) = Density (\g -> Density (\y -> g (f y), y), x)

extend :: (Density a -> b) -> Density a -> Density b
extend f (Density (g, x)) = Density (\y -> f (Density (g, y)), x)