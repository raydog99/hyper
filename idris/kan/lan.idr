%default total

data LanF : (f : Type -> Type) -> (g : Type -> Type) -> (a : Type) -> Type where
  MkLanF : ((forall b . (f b -> a) -> g b -> a)) -> LanF f g a

fmap : {0 f, g : Type -> Type} -> {0 a, b : Type} ->
       (a -> b) -> LanF f g a -> LanF f g b
fmap f (MkLanF g) = MkLanF (\k, h => f (g k h))

(<.>*) : {0 f, g : Type -> Type} -> {0 a, b : Type} ->
         LanF f g (a -> b) -> LanF f g a -> LanF f g b
(MkLanF f) <.>* (MkLanF x) = MkLanF (\k, h => f (\g => k g h) (\a => x (\_ => a) h))

pure : {0 f, g : Type -> Type} -> {0 a : Type} ->
       a -> LanF f g a
pure a = MkLanF (\_, _ => a)

(<*>) : {0 f, g : Type -> Type} -> {0 a, b : Type} ->
        LanF f g (a -> b) -> LanF f g a -> LanF f g b
(MkLanF f) <*> (MkLanF x) = MkLanF (\k, h => f (\g => k g h) (\a => x (\_ => a) h))

toLan : {0 f, g : Type -> Type} -> {0 a : Type} ->
        (forall b . g b -> t b) -> LanF f g a -> t a
toLan phi (MkLanF lan) = lan (\k => phi . k) id

fromLan : {0 f, g : Type -> Type} -> {0 a : Type} ->
          (forall b . LanF f g b -> t b) -> g a -> t a
fromLan phi g = phi (MkLanF (\k => k id g))