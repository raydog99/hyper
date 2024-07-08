module LoopUnrolling

import Data.SortedMap
import Data.SortedSet
import Control.Monad.State

%default total

data BasicBlock = MkBasicBlock String
data Instruction = MkInstruction String
data Value = BBValue BasicBlock | InstValue Instruction | ConstValue Int

record Loop where
  constructor MkLoop
  header : BasicBlock
  latch : BasicBlock
  blocks : List BasicBlock
  exits : List BasicBlock

record UnrollLoopOptions where
  constructor MkUnrollLoopOptions
  count : Nat
  tripCount : Nat
  tripMultiple : Nat
  peelCount : Nat
  allowRuntime : Bool
  allowExpensiveTripCount : Bool
  force : Bool
  unrollRemainder : Bool
  forgetAllSCEV : Bool

data LoopUnrollResult = Unmodified | PartiallyUnrolled | FullyUnrolled

record UnrolledValue where
  constructor MkUnrolledValue
  iteration : Nat
  original : Value

unrollRuntimeEpilog : Bool
unrollRuntimeEpilog = False

needToInsertPhisForLCSSA : Loop -> List BasicBlock -> (BasicBlock -> Maybe Loop) -> Bool
needToInsertPhisForLCSSA l blocks getLoopFor =
  any (\bb => 
    case getLoopFor bb of
      Just loop => loop /= l
      Nothing => False
  ) blocks

simplifyLoopAfterUnroll2 : Loop -> Bool -> Bool
simplifyLoopAfterUnroll2 l simplifyIVs = 
  True

isEpilogProfitable : Loop -> Bool
isEpilogProfitable (MkLoop header _ _ _) =
  True

unrollLoopWithVMap : Loop 
                  -> UnrollLoopOptions 
                  -> (BasicBlock -> Maybe Loop) 
                  -> State (SortedMap Value UnrolledValue) LoopUnrollResult
unrollLoopWithVMap l opts getLoopFor = do
  if isNothing (getLoopPreheader l) || isNothing (getLoopLatch l) || not (isSafeToClone l)
    then pure Unmodified
    else do
      let completelyUnroll = opts.count == opts.tripCount
      let runtimeTripCount = opts.tripCount == 0 && opts.count > 0 && opts.allowRuntime

      newBlocks <- replicateM (cast opts.count) (cloneLoopBlocks l)

      updatePhiNodes l completelyUnroll (opts.count > 1)

      connectUnrolledIterations l newBlocks

      let simplified = simplifyLoopAfterUnroll2 l (not completelyUnroll && (opts.count > 1))

      pure $ if completelyUnroll then FullyUnrolled else PartiallyUnrolled

  where
    getLoopPreheader : Loop -> Maybe BasicBlock
    getLoopPreheader _ = Just (MkBasicBlock "preheader")

    getLoopLatch : Loop -> Maybe BasicBlock
    getLoopLatch (MkLoop _ latch _ _) = Just latch

    isSafeToClone : Loop -> Bool
    isSafeToClone _ = True

    cloneLoopBlocks : Loop -> State (SortedMap Value UnrolledValue) (List BasicBlock)
    cloneLoopBlocks (MkLoop _ _ blocks _) = do
      traverse (\bb => do
        let newBB = MkBasicBlock (show bb ++ "_cloned")
        modify (insert (BBValue bb) (MkUnrolledValue 0 (BBValue bb)))
        pure newBB
      ) blocks

    updatePhiNodes : Loop -> Bool -> Bool -> State (SortedMap Value UnrolledValue) ()
    updatePhiNodes _ _ _ = pure ()

    connectUnrolledIterations : Loop -> List (List BasicBlock) -> State (SortedMap Value UnrolledValue) ()
    connectUnrolledIterations _ _ = pure ()