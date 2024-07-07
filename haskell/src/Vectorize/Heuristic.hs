{-# LANGUAGE RecordWildCards #-}

module Heuristic where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (foldl')
import LLVM.AST
import VectorPack
import Packer
import Solver

cSplat, cPerm, cInsert, cShuffle, cExtract :: Float
cSplat = 1.0
cPerm = 2.0
cInsert = 2.0
cShuffle = 2.0
cExtract = 1.0

data HeuristicConfig = HeuristicConfig
  { allowDeinterleave :: Bool
  , allowTranspose :: Bool
  }

data Solution = Solution
  { solutionCost :: Float
  , solutionPacks :: [VectorPack]
  }

data Heuristic = Heuristic
  { heuristicSolutions :: HM.HashMap OperandPack Solution
  , heuristicScalarCosts :: HM.HashMap Operand Float
  , heuristicPacker :: Packer
  , heuristicConfig :: HeuristicConfig
  }

getCost :: Heuristic -> VectorPack -> Float
getCost h vp =
  let baseCost = vectorPackProducingCost vp
      opCost op
        | all (maybe False isCmpInst . valueContent) (operandPackToList op) = 0
        | otherwise = getCost h op
  in foldl' (\acc op -> acc + opCost op) baseCost (vectorPackOperandPacks vp)

deinterleave :: VectorPackContext -> OperandPack -> Int -> [OperandPack]
deinterleave = undefined -- Implementation of deinterleave

transpose :: VectorPackContext -> OperandPack -> Int -> Maybe OperandPack
transpose vpCtx op n
  | length op `mod` n /= 0 = Nothing
  | otherwise =
      let m = length op `div` n
          t = [op !! (j * m + i) | i <- [0..m-1], j <- [0..n-1]]
      in Just $ getCanonicalOperandPack vpCtx t

solve :: Heuristic -> OperandPack -> Solution
solve h@Heuristic{..} op = case HM.lookup op heuristicSolutions of
  Just sol -> sol
  Nothing -> do
    let baseSol = Solution 0 []
    HM.insert op baseSol heuristicSolutions
    let (cost, inserted) = foldl' insertCost (0, HS.empty) (zip [0..] op)
        insertCost (acc, ins) (_, v)
          | isConstant v = (acc, ins)
          | v `HS.member` ins = (acc, ins)
          | otherwise = (acc + getCostValue h v + cInsert, HS.insert v ins)
        sol = if cost == 0
              then baseSol
              else do
                let broadcastCost = getCostValue h (head op) + cSplat
                if isSplat op && cost > broadcastCost
                then Solution broadcastCost []
                else do
                  let vpCtx = packerContext heuristicPacker
                      deduped = dedupOperandPack vpCtx op
                      extraCost = if deduped /= op then cShuffle else 0
                      opi = getProducerInfo heuristicPacker deduped
                      producerSols = map (\vp -> Solution (getCost h vp + extraCost) [vp]) (getProducers opi)
                      transposeSols
                        | allowTranspose heuristicConfig =
                            [ Solution (getCost h vp + cPerm) [vp]
                            | n <- [2, 4, 8]
                            , Just t <- [transpose vpCtx op n]
                            , vp <- getProducers (getProducerInfo heuristicPacker t)
                            ]
                        | otherwise = []
                      deinterleaveSols
                        | allowDeinterleave heuristicConfig =
                            [ Solution sumCost (concatMap solutionPacks subSols)
                            | stride <- [2, 4, 8]
                            , length deduped `mod` stride == 0
                            , let subOps = deinterleave vpCtx deduped stride
                                  subSols = map (solve h) subOps
                                  sumCost = cShuffle * fromIntegral (length subOps) + sum (map solutionCost subSols)
                            , sumCost < solutionCost sol
                            ]
                        | otherwise = []
                  minimumBy (comparing solutionCost) (sol : producerSols ++ transposeSols ++ deinterleaveSols)
    HM.insert op sol heuristicSolutions
    sol

getCostValue :: Heuristic -> Operand -> Float
getCostValue h@Heuristic{..} v = case valueContent v of
  InstructionC i -> case HM.lookup v heuristicScalarCosts of
    Just cost -> cost
    Nothing -> do
      HM.insert v 0 heuristicScalarCosts
      let cost = getScalarCost heuristicPacker i + sum (map (getCostValue h) (instructionOperands i))
      HM.insert v cost heuristicScalarCosts
      cost
  _ -> 0