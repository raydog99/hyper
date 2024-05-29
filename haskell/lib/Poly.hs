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