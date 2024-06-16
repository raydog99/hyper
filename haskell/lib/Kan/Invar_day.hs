module Invar_day where

import Control.Arrow ((&&&))

data Day f g a = Day (f b) (g c) (b -> c -> a) (a -> (b, c))

day :: f a -> g b -> Day f g (a, b)
day fa gb = Day fa gb (,) (\(a, b) -> (a, b))

invmap :: (a -> b) -> (c -> d) -> Day f g a -> Day f g b
invmap f g (Day fb gc bca abc) = Day fb gc (f *** g >>> bca) (abc >>> (\a -> let (b, c) = (g &&& id) a in (b, c)))

assoc :: Day f (Day g h c) a -> Day (Day f g c) h a
assoc (Day fb (Day gd he dec cde) bca abc) =
  let f a = let (b, c) = abc a in let (d, e) = cde c in ((b, d), e)
      g (b, d) e = bca b (dec d e)
  in Day (Day fb gd (id &&& fst) snd) he g f

disassoc :: Day (Day f g b) h a -> Day f (Day g h a) b
disassoc (Day (Day fb gc deb bde) hd bca abc) =
  let f e (d, c) = bca (deb e d) c
      g a = let (b, c) = abc a in let (d, e) = bde b in (d, (e, c))
  in Day fb (Day gc hd snd (id &&& fst)) f g

swapped :: Day f g a -> Day g f a
swapped (Day fb gc bca abc) = Day gc fb (bca *** id) (abc >>> (\a -> let (b, c) = a in (c, b)))

intro1 :: f a -> Day () f a
intro1 fa = Day () fa (const id) (id &&& snd)

intro2 :: f a -> Day f () a
intro2 fa = Day fa () (fst &&& const ())

elim1 :: Day () f b -> f b
elim1 (Day () fc bca abc) = bca () <$> fc

elim2 :: Day f () b -> f b
elim2 (Day fb () bca abc) = flip bca () <$> fb

trans1 :: (f a -> g b) -> Day f h a -> Day g h a
trans1 fg (Day fb hc bca abc) = Day (fg fb) hc bca abc

trans2 :: (g a -> h b) -> Day f g a -> Day f h a
trans2 gh (Day fb gc bca abc) = Day fb (gh gc) bca abc

toContravariant :: Day f g a -> Contravariant.Day f g a
toContravariant (Day fb gc _ abc) = Contravariant.Day fb gc abc

toCovariant :: Day f g a -> Covariant.Day f g a
toCovariant (Day fb gc bca _) = Covariant.Day fb gc bca