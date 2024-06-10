module Lift

Lift : (Type -> Type) -> (Type -> Type) -> Type -> Type
Lift g h a = (a -> b) -> g a -> h b

dimap : (b -> a) -> (a -> c) -> Lift g h a -> Lift g h c
dimap f g (Lift m) = Lift $ \bac => m (bac . g) . f

divide : (a -> b) -> Lift g h a -> Lift g h b -> Lift g h c
divide f (Lift g) (Lift h) = Lift $ \bac => h (bac . f) . g bac