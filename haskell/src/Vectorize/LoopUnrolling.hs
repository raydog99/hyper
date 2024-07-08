{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module LoopUnrolling where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Constant as IR
import qualified LLVM.PassManager as PM
import qualified LLVM.Transforms as T
import qualified LLVM.Analysis as A
import Control.Monad.State
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)
import Data.List (foldl')
import System.IO
import Options.Applicative

data Loop = Loop
  { loopHeader :: AST.Name
  , loopLatch :: AST.Name
  , loopBlocks :: [AST.Name]
  , loopExits :: [AST.Name]
  }

data UnrollLoopOptions = UnrollLoopOptions
  { uloCount :: Int
  , uloTripCount :: Int
  , uloTripMultiple :: Int
  , uloPeelCount :: Int
  , uloAllowRuntime :: Bool
  , uloAllowExpensiveTripCount :: Bool
  , uloForce :: Bool
  , uloUnrollRemainder :: Bool
  , uloForgetAllSCEV :: Bool
  }

data LoopUnrollResult
  = Unmodified
  | PartiallyUnrolled
  | FullyUnrolled

data UnrolledValue = UnrolledValue
  { uvIteration :: Int
  , uvOriginal :: AST.Operand
  }

unrollRuntimeEpilog :: IORef Bool
unrollRuntimeEpilog = unsafePerformIO $ newIORef False

needToInsertPhisForLCSSA :: Loop -> [AST.Name] -> LoopInfo -> Bool
needToInsertPhisForLCSSA loop blocks li =
  any checkBlock blocks
  where
    checkBlock bb
      | loopContains loop bb = False
      | otherwise = any checkInstr (blockInstructions bb)
    checkInstr instr = any checkOperand (instructionOperands instr)
    checkOperand op =
      case valueAsInstruction op of
        Just defInstr ->
          let defLoop = getLoopFor li (instructionParent defInstr)
          in isJust defLoop && loopContains (fromJust defLoop) loop
        Nothing -> False

simplifyLoopAfterUnroll2 :: Loop -> Bool -> LoopInfo -> ScalarEvolution -> DominatorTree -> AssumptionCache -> TargetTransformInfo -> IRBuilderT ModuleBuilder ()
simplifyLoopAfterUnroll2 loop simplifyIVs li se dt ac tti = do
  when (isJust se && simplifyIVs) $ do
    deadInsts <- simplifyLoopIVs loop se dt li tti
    mapM_ deleteInstruction deadInsts

  dl <- asks getDataLayout
  forM_ (loopBlocks loop) $ \bb -> do
    mapM_ simplifyInstr (blockInstructions bb)
  where
    simplifyInstr instr = do
      simplified <- simplifyInstruction instr dl Nothing dt ac
      case simplified of
        Just v | replacementPreservesLCSSAForm li instr v ->
          replaceAllUsesWith instr v
        _ | isInstructionTriviallyDead instr ->
          deleteInstruction instr
        _ -> return ()

isEpilogProfitable :: Loop -> Bool
isEpilogProfitable loop =
  any isConstantPHI (phiNodes $ loopHeader loop)
  where
    isConstantPHI phi =
      case incomingValueForBlock phi (loopPreheader loop) of
        Just v -> isConstant v
        Nothing -> False

unrollLoopWithVMap :: Loop -> UnrollLoopOptions -> LoopInfo -> Maybe ScalarEvolution ->
                      Maybe DominatorTree -> Maybe AssumptionCache ->
                      Maybe TargetTransformInfo -> Bool ->
                      Map AST.Operand UnrolledValue -> Maybe Loop ->
                      IRBuilderT ModuleBuilder LoopUnrollResult
unrollLoopWithVMap loop@Loop{..} ulo@UnrollLoopOptions{..} li se dt ac tti preserveLCSSA unrollToOrigMap remainderLoop = do
  preheader <- getLoopPreheader loop
  latch <- getLoopLatch loop
  if isNothing preheader || isNothing latch || not (isSafeToClone loop) || hasAddressTaken loopHeader
    then return Unmodified
    else do
      let ulo' = if uloTripCount /= 0 && uloCount > uloTripCount
                   then ulo { uloCount = uloTripCount }
                   else ulo
      if uloTripCount == 0 && uloCount < 2 && uloPeelCount == 0
        then return Unmodified
        else do
          let completelyUnroll = uloCount == uloTripCount
              runtimeTripCount = uloTripCount == 0 && uloCount > 0 && uloAllowRuntime

          peeled <- if uloPeelCount > 0
                      then peelLoop loop uloPeelCount li se dt ac preserveLCSSA
                      else return False
          let ulo'' = if peeled
                        then let exitingBlock = fromJust latch
                                 newTripCount = getSmallConstantTripCount se loop exitingBlock
                                 newTripMultiple = getSmallConstantTripMultiple se loop exitingBlock
                             in ulo' { uloTripCount = newTripCount, uloTripMultiple = newTripMultiple }
                        else ulo'

          when (isJust se) $
            if uloForgetAllSCEV
              then forgetAllLoops se
              else forgetTopmostLoop se loop

          (unrolledBlocks, lastValueMap) <- foldM cloneIteration ([], Map.empty) [1..uloCount]

          updatePhiNodes loop completelyUnroll (uloCount > 1) lastValueMap

          connectUnrolledIterations loop unrolledBlocks

          simplifyLoopAfterUnroll2 loop (not completelyUnroll && (uloCount > 1 || peeled)) li se dt ac tti

          when (preserveLCSSA && completelyUnroll) $
            forM_ loopExits $ \exitBB ->
              forM_ (phiNodes exitBB) $ \phi ->
                forM_ (incomingBlocks phi) $ \block ->
                  unless (loopContains loop block) $
                    removeIncoming phi block

          when completelyUnroll $
            eraseLoop li loop

          return $ if completelyUnroll then FullyUnrolled else PartiallyUnrolled

  where
    cloneIteration (blocks, valueMap) i = do
      (newBlocks, newValueMap) <- cloneLoopBlocks loop valueMap
      return (newBlocks : blocks, newValueMap)