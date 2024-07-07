module LoopAwareRPO

import Data.SortedSet
import Data.Vect

%default total

record LoopInfo where
  constructor MkLoopInfo

record Loop where
  constructor MkLoop

computeRPO : Function -> LoopInfo -> List BasicBlock -> List BasicBlock
computeRPO f li rpo =
  let worklist = [entryBasicBlock f]
      visited = empty
      
      process : List BasicBlock -> SortedSet BasicBlock -> List BasicBlock -> (List BasicBlock, List BasicBlock)
      process [] visited acc = (acc, rpo)
      process (bb :: rest) visited acc =
        if contains bb visited
        then process rest visited acc
        else
          let visited' = insert bb visited
              succs = successors bb
              (newWorklist, acc') = foldl (\(wl, acc), succ ->
                                      if isJust (getLoopFor li succ)
                                      then (wl ++ [succ], acc)
                                      else (succ :: wl, acc)
                                   ) (rest, bb :: acc) succs
          in process newWorklist visited' acc'
      
      (newRPO, _) = process worklist empty []
  in newRPO ++ rpo

computeLoopRPO : LoopInfo -> Loop -> List BasicBlock -> List BasicBlock
computeLoopRPO li l rpo =
  let header = getHeader li l
      worklist = [header]
      visited = empty
      
      process : List BasicBlock -> SortedSet BasicBlock -> List BasicBlock -> (List BasicBlock, List BasicBlock)
      process [] visited acc = (acc, rpo)
      process (bb :: rest) visited acc =
        if contains bb visited
        then process rest visited acc
        else
          let visited' = insert bb visited
              succs = successors bb
              (newWorklist, acc') = foldl (\(wl, acc), succ ->
                                      if contains l succ li
                                      then if succ /= header
                                           then (wl ++ [succ], acc)
                                           else (wl, acc)
                                      else (succ :: wl, acc)
                                   ) (rest, bb :: acc) succs
          in process newWorklist visited' acc'
      
      (newRPO, _) = process worklist empty []
  in newRPO ++ rpo

computeNestedLoopRPO : LoopInfo -> Loop -> List BasicBlock -> List BasicBlock
computeNestedLoopRPO li l rpo =
  let header = getHeader li l
      worklist = [header]
      visited = empty
      
      process : List BasicBlock -> SortedSet BasicBlock -> List Loop -> List BasicBlock -> (List BasicBlock, List Loop, List BasicBlock)
      process [] visited subLoops acc = (acc, subLoops, rpo)
      process (bb :: rest) visited subLoops acc =
        if contains bb visited
        then process rest visited subLoops acc
        else
          let visited' = insert bb visited
              succs = successors bb
              (newWorklist, subLoops', acc') = foldl (\(wl, sl, acc), succ ->
                                                  if contains l succ li
                                                  then case getLoopFor li succ of
                                                         Just succLoop =>
                                                           if getParentLoop li succLoop == Just l
                                                           then (wl, succLoop :: sl, acc)
                                                           else if succ /= header
                                                                then (wl ++ [succ], sl, acc)
                                                                else (wl, sl, acc)
                                                         Nothing =>
                                                           if succ /= header
                                                           then (wl ++ [succ], sl, acc)
                                                           else (wl, sl, acc)
                                                  else (succ :: wl, sl, acc)
                                               ) (rest, subLoops, bb :: acc) succs
          in process newWorklist visited' subLoops' acc'
      
      (newRPO, subLoops, _) = process worklist empty [] []
      finalRPO = foldl (\acc, subLoop -> computeLoopRPO li subLoop acc) newRPO (reverse subLoops)
  in finalRPO ++ rpo

laRPORecurse : LoopInfo -> Loop -> List BasicBlock -> List BasicBlock
laRPORecurse li l rpo =
  if isOutermostLoop li l
  then computeRPO (functionOf (getHeader li l)) li rpo
  else if hasNestedLoops li l
       then computeNestedLoopRPO li l rpo
       else computeLoopRPO li l rpo

computeLARPO : Function -> LoopInfo -> List BasicBlock
computeLARPO f li =
  let baseRPO = computeRPO f li []
      topLevelLoops = getTopLevelLoops li
  in foldl (\acc, l -> laRPORecurse li l acc) baseRPO topLevelLoops