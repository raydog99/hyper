module Heuristic

import Data.SortedMap
import VectorPack
import Packer
import Solver

%default total

cSplat : Float
cSplat = 1.0

cPerm : Float
cPerm = 2.0

cInsert : Float
cInsert = 2.0

cShuffle : Float
cShuffle = 2.0

cExtract : Float
cExtract = 1.0

record HeuristicConfig where
  constructor MkHeuristicConfig
  allowDeinterleave : Bool
  allowTranspose : Bool

record Solution where
  constructor MkSolution
  cost : Float
  packs : List VectorPack

record Heuristic where
  constructor MkHeuristic
  solutions : SortedMap OperandPack Solution
  scalarCosts : SortedMap Value Float
  pkr : Packer
  config : HeuristicConfig

getCost : Heuristic -> VectorPack -> Float
getCost h vp =
  let baseCost = getProducingCost vp
      opCost op = if all isCmpInst (toList op)
                  then 0
                  else getCost h op
  in foldl (\acc, op => acc + opCost op) baseCost (getOperandPacks vp)

deinterleave : VectorPackContext -> OperandPack -> Nat -> List OperandPack
deinterleave vpCtx op stride = []

transpose : VectorPackContext -> OperandPack -> Nat -> Maybe OperandPack
transpose vpCtx op n =
  if length op `mod` n /= 0
  then Nothing
  else
    let m = length op `div` n
        t = [index (j * m + i) op | i <- [0..m-1], j <- [0..n-1]]
    in Just $ getCanonicalOperandPack vpCtx t

solve : Heuristic -> OperandPack -> Solution
solve h@(MkHeuristic solutions scalarCosts pkr config) op =
  case lookup op solutions of
    Just sol -> sol
    Nothing ->
      let baseSol = MkSolution 0 []
          (cost, inserted) = foldl insertCost (0, empty) (zip [0..] (toList op))
          insertCost (acc, ins) (_, v) =
            if isConstant v || contains v ins
            then (acc, ins)
            else (acc + getCostValue h v + cInsert, insert v ins)
          sol =
            if cost == 0
            then baseSol
            else
              let broadcastCost = getCostValue h (head (toList op)) + cSplat
              in if isSplat op && cost > broadcastCost
                 then MkSolution broadcastCost []
                 else
                   let vpCtx = getContext pkr
                       deduped = dedupOperandPack vpCtx op
                       extraCost = if deduped /= op then cShuffle else 0
                       opi = getProducerInfo pkr deduped
                       producerSols = map (\vp -> MkSolution (getCost h vp + extraCost) [vp]) (getProducers opi)
                       transposeSols =
                         if allowTranspose config
                         then [MkSolution (getCost h vp + cPerm) [vp] |
                               n <- [2, 4, 8],
                               Just t <- [transpose vpCtx op n],
                               vp <- getProducers (getProducerInfo pkr t)]
                         else []
                       deinterleaveSols =
                         if allowDeinterleave config
                         then [MkSolution sumCost (concatMap (.packs) subSols) |
                               stride <- [2, 4, 8],
                               length deduped `mod` stride == 0,
                               let subOps = deinterleave vpCtx deduped stride
                                   subSols = map (solve h) subOps
                                   sumCost = cShuffle * cast (length subOps) + sum (map (.cost) subSols),
                               sumCost < sol.cost]
                         else []
                   in minimumBy (comparing .cost) (sol :: producerSols ++ transposeSols ++ deinterleaveSols)
      in insert op sol solutions
      sol

getCostValue : Heuristic -> Value -> Float
getCostValue h@(MkHeuristic solutions scalarCosts pkr config) v =
  case lookup v scalarCosts of
    Just cost -> cost
    Nothing ->
      let cost = getScalarCost pkr v + sum (map (getCostValue h) (operands v))
      in insert v cost scalarCosts
      cost

isCmpInst : Value -> Bool
isCmpInst _ = False

isConstant : Value -> Bool
isConstant _ = False

minimumBy : (a -> a -> Ordering) -> List a -> a
minimumBy _ [] = error "Empty list"
minimumBy cmp (x::xs) = foldl (\acc, y -> if cmp y acc == LT then y else acc) x xs

comparing : (a -> b) -> (a -> a -> Ordering)
comparing f x y = compare (f x) (f y)