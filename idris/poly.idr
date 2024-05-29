module Poly

import Data.Vect

%default total

Poly : Type -> Type
Poly a = Vect n a

eval : Num a => Poly a -> a -> a
eval p x = sum $ zipWith (*) (toList p) (map (^x) [0..len-1])
  where
    len = length p

map : (a -> b) -> Poly a -> Poly b
map f p = map f p

sum : Num a => Poly a -> Poly a -> Poly a
sum p q = zipWith (+) (pad p) (pad q)
  where
    pad : Poly a -> Poly a
    pad p = p ++ replicate (max 0 (length q - length p)) neutral

    neutral : a
    neutral = neutral

product : Num a => Poly a -> Poly a -> Poly a
product p q = conv $ concatMap (\(x, y) => map (*x) (drop y q)) zipPairs
  where
    conv : Vect n (Poly a) -> Poly a
    conv ps = foldl sum neutral ps

    zipPairs : Vect (length p) (a, Nat)
    zipPairs = zipWith (\x, y => (index y p, y)) [0..length p-1]