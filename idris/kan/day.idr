module DayConvolution

data Day : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  MkDay : f b -> g c -> (b -> c -> a) -> Day f g a

fmap : (a -> b) -> Day f g a -> Day f g b
fmap f (MkDay fb gc bca) = MkDay fb gc (\b, c => f (bca b c))

pure : a -> Day f g a
pure x = MkDay () () (\_, _ => x)

ap : Day f g (a -> b) -> Day f g a -> Day f g b
ap (MkDay fa fb u) (MkDay gc gd v) = MkDay (fa, gc) (fb, gd) (\(a, c), (b, d) => u a b (v c d))

assoc : Day f (Day g h a) a -> Day (Day f g a) h a
assoc (MkDay fb (MkDay gd he dec) bca) = MkDay (MkDay fb gd (\b, d => bca b (dec d))) he (\(MkDay b d bca), e => bca (b, d) e)

disassoc : Day (Day f g a) h a -> Day f (Day g h a) a
disassoc (MkDay (MkDay fb gc bca) hd eda) = MkDay fb (MkDay gc hd (\c, d => eda (bca c) d))

swapped : Day f g a -> Day g f a
swapped (MkDay fb gc abc) = MkDay gc fb (\c, b => abc b c)

intro1 : (f : Type -> Type) -> f a -> Day () f a
intro1 _ f = MkDay () f (\_, fa => fa)

intro2 : (g : Type -> Type) -> g a -> Day g () a
intro2 _ g = MkDay g () (\ga, _ => ga)

elim1 : Day () f a -> f a
elim1 (MkDay () fc bca) = bca () fc

elim2 : Day f () a -> f a
elim2 (MkDay fb () bca) = fb

trans1 : (a -> b) -> Day f g a -> Day f g b
trans1 fg (MkDay fb hc bca) = MkDay fb hc (\b, c => fg (bca b c))

trans2 : (a -> b) -> Day f a c -> Day f b c
trans2 gh (MkDay fb gc bca) = MkDay fb (gh gc) (\b, c => bca b c)