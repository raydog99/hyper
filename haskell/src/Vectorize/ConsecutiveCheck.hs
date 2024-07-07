{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module ConsecutiveCheck where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Monad.Reader

import LoopInfo
import ScalarEvolution
import DataLayout

getLoopNest :: LoopInfo -> AST.Instruction -> [Loop]
getLoopNest li instr = 
  let bb = AST.basicBlock instr
  in reverse $ unfoldr (\l -> (,) l <$> loopParent l) (loopFor li bb)

getAddressSpaceOperand :: AST.Instruction -> Maybe Word32
getAddressSpaceOperand instr = case instr of
  AST.Load {..} -> Just $ AST.pointerAddressSpace address
  AST.Store {..} -> Just $ AST.pointerAddressSpace address
  _ -> Nothing

data AddRecLoopRewriter = AddRecLoopRewriter
  { rewriterSE :: ScalarEvolution
  , rewriterLoops :: Map.Map Loop Loop
  , rewriterSuccess :: Bool
  }

rewriteAddRec :: AddRecLoopRewriter -> SCEV -> SCEV
rewriteAddRec AddRecLoopRewriter{..} expr = case expr of
  SCEVAddRec loop ops flags -> 
    let newLoop = fromMaybe loop $ Map.lookup loop rewriterLoops
        newOps = map (rewriteAddRec AddRecLoopRewriter{..}) ops
    in if all (\op -> scevIsAvailableAtLoopEntry rewriterSE op newLoop) newOps
       then scevAddRec rewriterSE newOps newLoop flags
       else expr
  _ -> expr

isEquivalent :: AST.Operand -> AST.Operand -> ScalarEvolution -> LoopInfo -> Bool
isEquivalent ptrA ptrB se li = 
  let scevA = getSCEV se ptrA
      scevB = getSCEV se ptrB
  in if scevA == scevB
     then True
     else case (ptrA, ptrB) of
       (AST.LocalReference _ _, AST.LocalReference _ _) ->
         let loopNest1 = getLoopNest li ptrA
             loopNest2 = getLoopNest li ptrB
         in if length loopNest1 /= length loopNest2
            then False
            else let loops = Map.fromList $ zip loopNest2 loopNest1
                     rewriter = AddRecLoopRewriter se loops True
                     newScevB = rewriteAddRec rewriter scevB
                 in scevA == newScevB
       _ -> False

isConsecutive :: AST.Instruction -> AST.Instruction -> DataLayout -> ScalarEvolution -> LoopInfo -> Bool
isConsecutive a b dl se li = 
  let ptrA = getLoadStorePointerOperand a
      ptrB = getLoadStorePointerOperand b
      asA = getAddressSpaceOperand a
      asB = getAddressSpaceOperand b
  in case (ptrA, ptrB, asA, asB) of
    (Just ptrA, Just ptrB, Just asA, Just asB) ->
      let loopNest1 = getLoopNest li ptrA
          loopNest2 = getLoopNest li ptrB
      in if length loopNest1 /= length loopNest2
         then False
         else let loops = Map.fromList $ zip loopNest2 loopNest1
                  rewriter = AddRecLoopRewriter se loops True
                  idxWidth = indexSizeInBits dl asA
                  ty = AST.elementType $ AST.typeOf ptrA
                  (offsetA, strippedPtrA) = stripAndAccumulateInBoundsConstantOffsets dl ptrA
                  (offsetB, strippedPtrB) = stripAndAccumulateInBoundsConstantOffsets dl ptrB
                  size = C.Int idxWidth $ toInteger $ storeSize dl ty
                  offsetScevA = scevConstant se offsetA
                  offsetScevB = scevConstant se offsetB
                  offsetDeltaScev = scevMinus se offsetScevB offsetScevA
                  offsetDelta = scevConstantInt offsetDeltaScev
              in if strippedPtrA == strippedPtrB
                 then offsetDelta == size
                 else let sizeScev = scevConstant se size
                          baseDelta = scevMinus se sizeScev offsetDeltaScev
                          ptrScevA = getSCEV se strippedPtrA
                          ptrScevB = rewriteAddRec rewriter $ getSCEV se strippedPtrB
                          x = scevAdd se [ptrScevA, baseDelta]
                      in x == ptrScevB
    _ -> False

findConsecutiveAccesses :: ScalarEvolution -> DataLayout -> LoopInfo -> [AST.Instruction] -> EquivalenceClasses AST.Instruction -> Int -> [(AST.Instruction, AST.Instruction)]
findConsecutiveAccesses se dl li accesses equivalentAccesses numFingerprints =
  if null accesses then []
  else
    let ptr = fromJust $ getLoadStorePointerOperand $ head accesses
        ty = AST.elementType $ AST.typeOf ptr
        size = storeSize dl ty
        sg = newSizeGenerator
        igs = replicate numFingerprints newIterationGenerator
        (consecutiveAccesses, _, _) = foldl' (processAccess se dl li size sg igs) ([], Map.empty, Map.empty) accesses
    in consecutiveAccesses

processAccess :: ScalarEvolution -> DataLayout -> LoopInfo -> Integer -> SizeGenerator -> [IterationGenerator] -> ([(AST.Instruction, AST.Instruction)], Map.Map Int64 [AST.Instruction], Map.Map AST.Instruction [Int64]) -> AST.Instruction -> ([(AST.Instruction, AST.Instruction)], Map.Map Int64 [AST.Instruction], Map.Map AST.Instruction [Int64])
processAccess se dl li size sg igs (consecutiveAccesses, fingerprintsToAccesses, accessToFingerprints) i =
  let ptr = fromJust $ getLoadStorePointerOperand i
      ptrSCEV = getSCEV se ptr
      fingerprints = fingerprintSCEV se ptrSCEV sg igs numFingerprints
      left = head fingerprints - size
      leftAccesses = Map.findWithDefault [] left fingerprintsToAccesses
      newConsecutiveAccesses = foldr (\leftI acc ->
        let leftFingerprints = fromJust $ Map.lookup leftI accessToFingerprints
        in if all (\(a, b) -> a + size == b) (zip leftFingerprints fingerprints) &&
              isConsecutive leftI i dl se li
           then (leftI, i) : acc
           else acc
      ) consecutiveAccesses leftAccesses
      equivAccesses = Map.findWithDefault [] (head fingerprints) fingerprintsToAccesses
      updatedEquivalentAccesses = foldr (\i2 acc ->
        let fingerprints2 = fromJust $ Map.lookup i2 accessToFingerprints
        in if all (uncurry (==)) (zip fingerprints2 fingerprints) &&
              isEquivalent (fromJust $ getLoadStorePointerOperand i) (fromJust $ getLoadStorePointerOperand i2) se li
           then unionEquivalenceClasses acc i i2
           else acc
      ) equivalentAccesses equivAccesses
      right = head fingerprints + size
      rightAccesses = Map.findWithDefault [] right fingerprintsToAccesses
      finalConsecutiveAccesses = foldr (\rightI acc ->
        let rightFingerprints = fromJust $ Map.lookup rightI accessToFingerprints
        in if all (\(a, b) -> a == b + size) (zip rightFingerprints fingerprints) &&
              isConsecutive i rightI dl se li
           then (i, rightI) : acc
           else acc
      ) newConsecutiveAccesses rightAccesses
      updatedFingerprintsToAccesses = Map.insertWith (++) (head fingerprints) [i] fingerprintsToAccesses
      updatedAccessToFingerprints = Map.insert i fingerprints accessToFingerprints
  in (finalConsecutiveAccesses, updatedFingerprintsToAccesses, updatedAccessToFingerprints)

fingerprintSCEV :: ScalarEvolution -> SCEV -> SizeGenerator -> [IterationGenerator] -> Int -> [Int64]
fingerprintSCEV se expr sg igs n =
  let fingerprinter = SCEVFingerprinter se sg (head igs)
      result = scevVisit fingerprinter expr
  in map (\ig -> 
       let fingerprinter' = SCEVFingerprinter se sg ig
       in fromIntegral $ scevConstantInt $ scevVisit fingerprinter' expr
     ) igs

data SCEVFingerprinter = SCEVFingerprinter
  { fpSE :: ScalarEvolution
  , fpSG :: SizeGenerator
  , fpIG :: IterationGenerator
  }

instance SCEVVisitor SCEVFingerprinter where
  visitAddRecExpr fp expr@SCEVAddRec{..} = do
    ops <- mapM (scevVisit fp) scevAddRecOperands
    let l = scevAddRecLoop expr
        x = scevAddRec (fpSE fp) ops l (scevAddRecFlags expr)
    case x of
      SCEVAddRec{} -> scevEvaluateAddRecAtIteration (fpSE fp) x (scevConstant (fpSE fp) $ fromIntegral $ getIteration (fpIG fp) l)
      _ -> x
  
  visitUnknown fp expr@SCEVUnknown{} =
    scevConstant (fpSE fp) $ fromIntegral $ getSize (fpSG fp) expr