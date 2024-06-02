import qualified Data.Map as Map

type Operad a = (
    [a],                       -- colors
    Map.Map (Seq a, a) Int,    -- operations
    (Seq a, Int, [Int]) -> Maybe Int,  -- composition
    a -> Int                   -- identity
  )

type Seq a = [a]

makeOperad :: [a] -> [(Seq a, a, Int)] -> ((Seq a, Int, [Int]) -> Maybe Int) -> (a -> Int) -> Operad a
makeOperad colors ops comp identity = (colors, Map.fromList $ zip (map (\(i,o,_) -> (i,o)) ops) [0..], comp, identity)

findOperation :: Operad a -> Seq a -> a -> Maybe Int
findOperation (_, ops, _, _) i o = Map.lookup (i, o) ops

compose :: Operad a -> Int -> [Int] -> Maybe Int
compose (_, _, comp, _) op ops = comp (undefined, op, ops)

identityOp :: Operad a -> a -> Int
identityOp (_, _, _, identity) = identity