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


module PolyComonoid

import Poly
import Control.Comonoid

%default total

PolyComonoid : Comonoid a => Comonoid (Poly a)
PolyComonoid = MkComonoid PolyCounit PolyComult where
  
  PolyCounit : Poly a -> Unit
  PolyCounit [] = ()
  PolyCounit ((c, 0) :: _) = counit c
  PolyCounit _ = fail "Polynomial must be constant to have a counit"
  
  PolyComult : Poly a -> (Poly a, Poly a)
  PolyComult p = aux p [] [] where
    aux : Poly a -> Poly a -> Poly a -> (Poly a, Poly a)
    aux [] xs ys = (xs, ys)
    aux ((c, 0) :: rest) xs ys =
      let (c1, c2) = comult c
      in aux rest ((c1, 0) :: xs) ((c2, 0) :: ys)
    aux ((c, n) :: rest) xs ys =
      aux rest ((c, n) :: xs) ((counit c, n) :: ys)

data Poly : Type -> Type where
  Nil : Poly a
  (::) : a -> Poly a -> Poly a

corolla : Poly a -> Poly a
corolla (x :: xs) = 0 :: x :: xs
corolla Nil = Nil