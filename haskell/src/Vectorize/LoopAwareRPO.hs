{-# LANGUAGE RecordWildCards #-}

module LoopAwareRPO where

import qualified LLVM.AST as AST
import qualified LLVM.Analysis.LoopInfo as LI
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map

computeRPO :: AST.Function -> LI.LoopInfo -> [AST.BasicBlock]
computeRPO f li = reverse $ fst $ foldl' processBlock ([], Set.empty) [AST.entryBlock f]
  where
    processBlock (rpo, visited) bb
      | Set.member bb visited = (rpo, visited)
      | otherwise = 
          let visited' = Set.insert bb visited
              succs = AST.successors bb
              (rpo', visited'') = foldl' processSucc (rpo, visited') succs
          in (bb:rpo', visited'')
    
    processSucc (rpo, visited) succ
      | isJust (LI.getLoopFor li succ) = (rpo, visited)
      | otherwise = processBlock (rpo, visited) succ

computeLoopRPO :: LI.LoopInfo -> LI.Loop -> [AST.BasicBlock]
computeLoopRPO li l = reverse $ fst $ foldl' processBlock ([], Set.empty) [LI.getHeader l]
  where
    processBlock (rpo, visited) bb
      | Set.member bb visited = (rpo, visited)
      | otherwise =
          let visited' = Set.insert bb visited
              succs = AST.successors bb
              (rpo', visited'') = foldl' processSucc (rpo, visited') succs
          in (bb:rpo', visited'')
    
    processSucc (rpo, visited) succ
      | LI.contains l succ =
          if succ /= LI.getHeader l
          then processBlock (rpo, visited) succ
          else (rpo, visited)
      | otherwise = (rpo, visited)

computeNestedLoopRPO :: LI.LoopInfo -> LI.Loop -> [AST.BasicBlock]
computeNestedLoopRPO li l = 
  let (rpo, subLoops) = foldl' processBlock ([], []) [LI.getHeader l]
      rpo' = foldl' (\r subL -> computeLoopRPO li subL ++ r) rpo subLoops
  in reverse rpo'
  where
    processBlock (rpo, subLoops) bb
      | Set.member bb (Set.fromList rpo) = (rpo, subLoops)
      | otherwise =
          let succs = AST.successors bb
              (rpo', subLoops') = foldl' processSucc (rpo, subLoops) succs
          in (bb:rpo', subLoops')
    
    processSucc (rpo, subLoops) succ
      | LI.contains l succ =
          case LI.getLoopFor li succ of
            Just succLoop | LI.getParentLoop succLoop == Just l ->
              (rpo, succLoop:subLoops)
            _ | succ /= LI.getHeader l -> processBlock (rpo, subLoops) succ
            _ -> (rpo, subLoops)
      | otherwise = (rpo, subLoops)

laRPORecurse :: LI.LoopInfo -> LI.Loop -> [AST.BasicBlock] -> [AST.BasicBlock]
laRPORecurse li l rpo
  | LI.isOutermostLoop li l = computeRPO (LI.getHeader l) li ++ rpo
  | LI.hasNestedLoops l = computeNestedLoopRPO li l ++ rpo
  | otherwise = computeLoopRPO li l ++ rpo

computeLARPO :: AST.Function -> LI.LoopInfo -> [AST.BasicBlock]
computeLARPO f li =
  let topLevelLoops = LI.getTopLevelLoops li
      baseRPO = computeRPO f li
  in foldl' (\r l -> laRPORecurse li l r) baseRPO topLevelLoops