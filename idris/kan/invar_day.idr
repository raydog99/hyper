module InvarDay

%default total

data Day : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  MkDay : (f : Type -> Type) -> (g : Type -> Type) ->
          (b : Type) -> (c : Type) ->
          f b -> g c -> (b -> c -> a) -> (a -> (b, c)) -> Day f g a

day : (f : Type -> Type) -> (g : Type -> Type) ->
      (a : Type) -> (b : Type) -> f a -> g b -> Day f g (a, b)
day f g a b fa gb = MkDay f g a b fa gb (\a, b => (a, b)) (\(a, b) => (a, b))

invmap : (f : Type -> Type) -> (g : Type -> Type) ->
         (a : Type) -> (b : Type) -> (c : Type) -> (d : Type) ->
         (a -> b) -> (c -> d) -> Day f g a -> Day f g b
invmap f g a b c d f' g' (MkDay f' g' a c fb gc bca abc) =
  MkDay f' g' b d fb gc (\ b, c => f' (bca b (g' c))) (\ a => let (b, c) = abc a in (b, g' c))

assoc : (f : Type -> Type) -> (g : Type -> Type) ->
        (h : Type -> Type) -> (a : Type) ->
        (b : Type) -> (c : Type) -> Day f (Day g h c) a ->
        Day (Day f g c) h a
assoc f g h a b c (MkDay f (Day g h c) a b fb (MkDay g h c b gd gc dec cde) bca abc) =
  let f : a -> (Day f g c, h)
      f a = let (b, c) = abc a in
            let (d, e) = cde c in
            (MkDay f g c d fb d (\ b, d => bca b (dec d e)) (\ a => (b, a)), e)
      g : (Day f g c, h) -> a
      g (MkDay f g c b fb gc bca abc, e) = bca fb (dec gc e)
  in MkDay (Day f g c) h a (MkDay f g c b fb gc (\ b, c => b) (\ c => (c, ()))) gc g f

disassoc : (f : Type -> Type) -> (g : Type -> Type) ->
           (h : Type -> Type) -> (a : Type) ->
           (b : Type) -> Day (Day f g b) h a ->
           Day f (Day g h a) b
disassoc f g h a b (MkDay (Day f g b) h a (MkDay f g b c fb gc deb bde) hd bca abc) =
  let f : b -> Day g h a -> b
      f b (MkDay g h a c gc hd bca abc) = bca (deb b gc) abc
      g : b -> (b, Day g h a)
      g b = let (a, c) = abc (bca (bde b) hd) in
            (b, MkDay g h a c gc c bca a)
  in MkDay f (Day g h a) b fb (MkDay g h a b gc hd bca abc) f g

swapped : (f : Type -> Type) -> (g : Type -> Type) ->
          (a : Type) -> Day f g a -> Day g f a
swapped f g a (MkDay f g a b fb gc bca abc) =
  MkDay g f a b gc fb (\ c, b => bca b c) (\ a => let (b, c) = abc a in (c, b))

intro1 : (f : Type -> Type) -> (a : Type) -> f a -> Day () f a
intro1 f a fa = MkDay () f a a () fa (\ (), a => a) (\ a => ((), a))

intro2 : (f : Type -> Type) -> (a : Type) -> f a -> Day f () a
intro2 f a fa = MkDay f () a a fa () (\ a, () => a) (\ a => (a, ()))

elim1 : Day () f a -> f a
elim1 (MkDay () f a a () fc bca abc) = invmap () f a a () () (\ (), a => a) (\ a => a) (MkDay () f a a () fc bca abc).gc

elim2 : Day f () a -> f a
elim2 (MkDay f () a a fb () bca abc) = invmap f () a a () () (\ a, () => a) (\ () => ()) (MkDay f () a a fb () bca abc).fb

trans1 : (f : Type -> Type) -> (g : Type -> Type) ->
         (h : Type -> Type) -> (a : Type) ->
         (fg : f ~> g) -> Day f h a -> Day g h a
trans1 f g h a fg (MkDay f h a b fb gc bca abc) =
  MkDay g h a b (fg fb) gc bca abc

trans2 : (f : Type -> Type) -> (g : Type -> Type) ->
         (h : Type -> Type) -> (a : Type) ->
         (gh : g ~> h) -> Day f g a -> Day f h a
trans2 f g h a gh (MkDay f g a b fb gc bca abc) =
  MkDay f h a b fb (gh gc) bca abc

data Contravariant : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  MkContravariant : (f : Type -> Type) -> (g : Type -> Type) ->
                    (a : Type) -> f a -> g a -> (a -> (b, c)) -> Contravariant f g a

toContravariant : Day f g a -> Contravariant f g a
toContravariant (MkDay f g a b fb gc bca abc) =
  MkContravariant f g a fb gc abc

data Covariant : (Type -> Type) -> (Type -> Type) -> Type -> Type where
  MkCovariant : (f : Type -> Type) -> (g : Type -> Type) ->
                (a : Type) -> f a -> g a -> (b -> c -> a) -> Covariant f g a

toCovariant : Day f g a -> Covariant f g a
toCovariant (MkDay f g a b fb gc bca abc) =
  MkCovariant f g a fb gc bca