{-# LANGUAGE RecordWildCards #-}

module MatchManager where

import qualified Data.Map.Strict as Map
import Data.List (sort, sortBy)
import Data.Ord (comparing)

import LLVM.AST
import LLVM.AST.Instruction
import qualified Operation

data MatchManager = MatchManager
  { opMatches :: Map.Map Operation.Operation [Operation.Match]
  }

sortByOutput :: Operation.Match -> Operation.Match -> Ordering
sortByOutput = comparing Operation.output

matchValue :: MatchManager -> Operand -> MatchManager
matchValue MatchManager{..} v =
  MatchManager $ Map.mapWithKey (\op matches -> Operation.match op v matches) opMatches

create :: [InstBinding] -> Function -> MatchManager
create insts f =
  let initMatches = Map.fromList
        [ (LaneOp.getOperation laneOp, [])
        | inst <- insts
        , laneOp <- InstBinding.getLaneOps inst
        ]
      mm = MatchManager initMatches
      mm' = foldl (flip matchValue) mm (concatMap basicBlockInstructions (basicBlocks f))
  in MatchManager $ Map.map (sortBy sortByOutput) (opMatches mm')

createWithInstructions :: [InstBinding] -> [Instruction] -> MatchManager
createWithInstructions insts toMatch =
  let initMatches = Map.fromList
        [ (LaneOp.getOperation laneOp, [])
        | inst <- insts
        , laneOp <- InstBinding.getLaneOps inst
        ]
      mm = MatchManager initMatches
      mm' = foldl (flip matchValue) mm toMatch
  in MatchManager $ Map.map (sortBy sortByOutput) (opMatches mm')

getMatches :: MatchManager -> Operation.Operation -> [Operation.Match]
getMatches MatchManager{..} op = 
  Map.findWithDefault [] op opMatches

getMatchesForOutput :: MatchManager -> Operation.Operation -> Operand -> [Operation.Match]
getMatchesForOutput mm op output =
  let matches = getMatches mm op
      dummyMatch = Operation.Match False [] output
      (lower, _) = span (\m -> sortByOutput m dummyMatch < EQ) matches
      (equal, _) = span (\m -> sortByOutput m dummyMatch == EQ) (drop (length lower) matches)
  in equal