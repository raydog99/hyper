import Data.Vect

%default total

data Operad : (Type -> Type) -> Type where
  MkOperad : (colors : Vect n a)
          -> (operations : Vect m (Vect ns a, a, Int))
          -> (composition : (Vect ns a, Int, Vect ns Int) -> Maybe Int)
          -> (identity : a -> Int)
          -> Operad a

makeOperad : (colors : Vect n a)
          -> (operations : Vect m (Vect ns a, a, Int))
          -> (composition : (Vect ns a, Int, Vect ns Int) -> Maybe Int)
          -> (identity : a -> Int)
          -> Operad a
makeOperad colors operations composition identity =
  MkOperad colors operations composition identity

findOperation : Operad a -> Vect ns a -> a -> Maybe Int
findOperation (MkOperad _ operations _ _) profile color =
  maybe Nothing id $ lookup (profile, color) operations
  where
    lookup : (Vect ns a, a) -> Vect m (Vect ns a, a, Int) -> Maybe Int
    lookup p [] = Nothing
    lookup p ((ps, c, i) :: ops) =
      if p == (ps, c)
        then Just i
        else lookup p ops

compose : Operad a -> Int -> Vect ns Int -> Maybe Int
compose (MkOperad _ _ composition _) op operands =
  composition ([], op, operands)

identityOp : Operad a -> a -> Int
identityOp (MkOperad _ _ _ identity) color =
  identity color