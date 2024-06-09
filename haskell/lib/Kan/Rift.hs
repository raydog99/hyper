module Rift where

newtype Rift g h a = Rift { unRift :: forall b c. (b -> (a, c)) -> g c -> h b }

contramap :: (b -> a) -> Rift g h a -> Rift g h b
contramap f (Rift m) = Rift $ \bac gc ->
  m (\b -> let (a, c) = bac b in (f a, c)) gc

divide :: (a -> b) -> Rift g g a -> Rift g g b -> Rift g g c
divide f (Rift g) (Rift h) = Rift $ \bac gc ->
  g (\a -> let (c, d) = bac a in (f c, d)) gc