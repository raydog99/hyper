module Rift

%default total

Rift : (Type -> Type) -> (Type -> Type) -> Type -> Type
Rift g h a = (b -> (a, c)) -> g c -> h b

contramap : (b -> a) -> Rift g h a -> Rift g h b
contramap f (Rift m) = Rift $ \bac => m $ \b => let (a, c) = bac b in (f a, c)

divide : (Contravariant g, g = h) => (a -> b) -> Rift g h a -> Rift g h b -> Rift g h c
divide f (Rift g) (Rift h) = Rift $ \bac => g $ \a => let (c, d) = bac a in (f c, d)