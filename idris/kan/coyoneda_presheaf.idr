module Coyoneda

%default total

Coyoneda : (Type -> Type) -> Type -> Type
Coyoneda f a = (a -> b) -> f b

contramap : (b -> a) -> Coyoneda f a -> Coyoneda f b
contramap f (Coyoneda g) = Coyoneda (\k => g (k . f))

lift : f a -> Coyoneda f a
lift x = Coyoneda (\k => map k x)

lower : Coyoneda f a -> f a
lower (Coyoneda g) = g id

hoist : (f ~> g) -> Coyoneda f b -> Coyoneda g b
hoist n (Coyoneda g) = Coyoneda (\k => n (g (k . id)))

Presheaf : Type -> Type
Presheaf a = a -> Int

contramap' : (b -> a) -> Presheaf a -> Presheaf b
contramap' f g = g . f

tabulate : a -> Presheaf a
tabulate _ = \_ => 0