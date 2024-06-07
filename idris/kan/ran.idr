module RightKanExtension

%default total

Identity : Type -> Type
Identity a = a

Adjunction : (Type -> Type) -> (Type -> Type) -> Type
Adjunction t g = (
  (g a -> t a),
  (t a -> g a),
  ((g b -> t b) -> t a -> t a),
  ((t b -> g b) -> g a -> g a)
)

Composition : (Type -> Type) -> (Type -> Type) -> Type
Composition t g = (
  (t a -> Compose t g a),
  (Compose t g a -> t a)
)

Compose : (Type -> Type) -> (Type -> Type) -> Type -> Type
Compose t g a = Either (t a) (g a)

Ran : (Type -> Type) -> (Type -> Type) -> Type -> Type
Ran g h a = (forall b . ((b -> g a) -> h b -> a))

map : (a -> b) -> Ran g h a -> Ran g h b
map f ran = \k => f (ran (\u => k (fst u) (snd u)))

apply : Ran g h (a -> b) -> Ran g h a -> Ran g h b
apply ranf ranx = \k => ranf (\u => fst (k u)) (ranx (\u => snd (k u)))

pure : a -> Ran g h a
pure a = \k => a

toRan : (h a -> t a) -> Ran g h a -> t a
toRan f ran = f (ran (\_ => id))

fromRan : (Ran g h a -> t a) -> h a -> t a
fromRan s h = s (\k => k id h)

adjointToRan : Adjunction t g -> g a -> Ran t Identity a
adjointToRan (unit, rightAdjoint, leftAdjunct, rightAdjunctor) g = \k => k (unit g) ()

ranToAdjoint : Adjunction t g -> Ran t Identity a -> t a
ranToAdjoint (unit, rightAdjoint, leftAdjunct, rightAdjunctor) ran = ran (\u => rightAdjoint u) ()

ranToComposedAdjoint : Adjunction t g -> Ran t h a -> h (t a)
ranToComposedAdjoint (unit, rightAdjoint, leftAdjunct, rightAdjunctor) ran = map rightAdjoint (ran id)

composedAdjointToRan : Adjunction t g -> h (g a) -> Ran t h a
composedAdjointToRan (unit, rightAdjoint, leftAdjunct, rightAdjunctor) h = \k => k (map unit h) h

composeRan : Composition t g -> Ran t (Ran g h) a -> Ran (Compose t g) h a
composeRan (compose, decompose) ran = \k => ran (\f => compose (toRan (\r => toRan (\b => r (\u => (Identity (u b), u b))) f) f)) k

decomposeRan : Composition t g -> Ran (Compose t g) h a -> Ran t (Ran g h a)
decomposeRan (compose, decompose) ran = \k => ran (\x => k (fromRan (\r => toRan (\b => r (\u => (fst (u b), snd (u b)))) (decompose x)) x))

gran : h a -> Ran Identity h a
gran h = \k => k id h