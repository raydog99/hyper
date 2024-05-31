module Poly where

import Data.List (unfoldr)

newtype Poly a = Poly [a]

instance Functor Poly where
  fmap f (Poly xs) = Poly (map f xs)

instance Num a => Num (Poly a) where
  (Poly xs) + (Poly ys) = Poly $ zipWith (+) (xs ++ repeat 0) (ys ++ repeat 0)
  (Poly xs) * (Poly ys) = Poly $ concatMap (multiplyByCoefficients xs) (unfoldr nextCoefficient ys)
    where
      nextCoefficient [] = Nothing
      nextCoefficient (y:ys) = Just (y, ys)
      multiplyByCoefficients xs ys = zipWith (*) (0 : xs) (ys ++ repeat 0)

eval :: Num a => Poly a -> a -> a
eval (Poly xs) x = sum $ zipWith (*) xs (map (x ^) [0..])

module PolyComonoid (C : Comonoid) where

import Prelude hiding (counit)

newtype Poly a = Poly { getPoly :: [(a, Int)] }

instance Comonoid a => Comonoid (Poly a) where
  counit (Poly p) =
    case p of
      [] -> ()
      (c, 0) : _ -> counit c
      _ -> error "Polynomial must be constant to have a counit"

  comult (Poly p) =
    let aux [] xs ys = (Poly xs, Poly ys)
        aux ((c, 0) : rest) xs ys =
          let (c1, c2) = comult c
           in aux rest ((c1, 0) : xs) ((c2, 0) : ys)
        aux ((c, n) : rest) xs ys =
          aux rest ((c, n) : xs) ((counit c, n) : ys)
     in aux p [] []

data Poly a = Poly [a] a deriving (Show, Eq)

corolla :: Num a => Poly a -> Poly a
corolla (Poly coef const) = Poly (const : coef) 0