module LeftKanExtension where

import Control.Applicative (liftA2)

newtype LanF f g a = LanF { runLanF :: (forall b. (f b -> a) -> g b -> a) }

fmap :: (a -> b) -> LanF f g a -> LanF f g b
fmap f (LanF g) = LanF $ \k h -> f (g k h)

(<.>*) :: LanF f g (a -> b) -> LanF f g a -> LanF f g b
(LanF f) <.>* (LanF x) = LanF $ \k h -> f (\g -> k g h) (\a -> x (\_ -> a) h)

pure :: a -> LanF f g a
pure a = LanF $ \_ _ -> a

(<*>) :: LanF f g (a -> b) -> LanF f g a -> LanF f g b
(LanF f) <*> (LanF x) = LanF $ \k h -> f (\g -> k g h) (\a -> x (\_ -> a) h)

toLan :: (forall a. g a -> t a) -> LanF g h b -> t b
toLan phi (LanF lan) = lan (fmap phi) id

fromLan :: (forall a. LanF g h a -> t a) -> g b -> t b
fromLan phi g = phi (LanF $ \k -> k id g)