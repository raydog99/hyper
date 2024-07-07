{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module DependenceAnalysis where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Control.Monad.Reader

import ScalarEvolution
import LoopInfo
import AliasAnalysis
import LazyValueInfo
import VectorPackContext

isLessThan :: ScalarEvolution -> SCEV -> SCEV -> Bool
isLessThan se a b = isKnownNegative se (getMinusSCEV se a b)

refineWithRange :: ScalarEvolution -> SCEV -> ConstantRange -> SCEV
refineWithRange se expr cr =
  let smin = getSignedMinValue (getBitWidth cr)
      umin = getMinValue (getBitWidth cr)
      smax = getSignedMaxValue (getBitWidth cr)
      umax = getMaxValue (getBitWidth cr)
      expr1 = if getSignedMin cr /= smax && contains cr (ConstantRange (getSignedMin cr) smax)
              then getSMaxExpr se expr (getConstant se (getSignedMin cr))
              else expr
      expr2 = if getUnsignedMin cr /= umax && contains cr (ConstantRange (getUnsignedMin cr) umax)
              then getUMaxExpr se expr1 (getConstant se (getUnsignedMin cr))
              else expr1
      expr3 = if getUpper cr /= smin && contains cr (ConstantRange smin (getUpper cr))
              then getSMinExpr se expr2 (getConstant se (getSignedMax cr))
              else expr2
      expr4 = if umin /= getUpper cr && contains cr (ConstantRange umin (getUpper cr))
              then getUMinExpr se expr3 (getConstant se (getUnsignedMax cr))
              else expr3
  in expr4

refineWithRanges :: ScalarEvolution -> SCEV -> Map.Map AST.Value ConstantRange -> SCEV
refineWithRanges se expr ranges =
  let valueToSCEV = Map.map (\r -> refineWithRange se (getSCEV se r) r) ranges
  in SCEVParameterRewriter.rewrite expr se valueToSCEV

newtype UnknownSCEVCollector = UnknownSCEVCollector
  { values :: Map.Map AST.Value () }

instance SCEVVisitor UnknownSCEVCollector where
  visitUnknown collector expr
    | AST.isIntegerTy (AST.typeOf (SCEVUnknown.getValue expr)) =
        collector { values = Map.insert (SCEVUnknown.getValue expr) () (values collector) }
    | otherwise = collector

getLocation :: AST.Instruction -> MemoryLocation
getLocation inst = case inst of
  AST.Store _ _ ptr _ _ _ -> MemoryLocation.get ptr
  AST.Load _ ptr _ _ _ -> MemoryLocation.get ptr
  _ -> MemoryLocation.empty

isSimple :: AST.Instruction -> Bool
isSimple inst = case inst of
  AST.Load _ _ _ _ _ -> not $ AST.volatile inst
  AST.Store _ _ _ _ _ -> not $ AST.volatile inst
  AST.Call {AST.functionAttributes = attrs} -> not $ any (== AST.Volatile) attrs
  _ -> True

getLoopForPointer :: LoopInfo -> AST.Value -> Maybe Loop
getLoopForPointer li ptr = case ptr of
  AST.LocalReference _ _ -> getLoopFor li (AST.getParent ptr)
  _ -> Nothing

getBaseValue :: SCEV -> Maybe AST.Value
getBaseValue scev = case scev of
  SCEVAddRecExpr ar -> getBaseValue (SCEVAddRecExpr.getStart ar)
  SCEVAddExpr a ->
    let last = SCEVAddExpr.getOperand a (SCEVAddExpr.getNumOperands a - 1)
    in if AST.isPointerTy (AST.typeOf last) then getBaseValue last else Nothing
  SCEVUnknown u -> Just (SCEVUnknown.getValue u)
  _ -> Nothing

isAliased :: AST.Instruction -> AST.Instruction -> AliasAnalysis -> ScalarEvolution -> LoopInfo -> LazyValueInfo -> Bool
isAliased i1 i2 aa se li lvi = 
  let loc1 = getLocation i1
      loc2 = getLocation i2
      f = AST.getParent (AST.getParent i1)
  in if isJust (MemoryLocation.getPointer loc1) && isJust (MemoryLocation.getPointer loc2) && isSimple i1 && isSimple i2
     then 
       let result = alias aa loc1 loc2
       in if result /= MayAlias
          then result
          else
            let ptr1 = getLoadStorePointerOperand i1
                ptr2 = getLoadStorePointerOperand i2
            in case (ptr1, ptr2) of
                 (Just ptr1, Just ptr2) ->
                   let ptr1SCEV = getSCEV se ptr1
                       ptr2SCEV = getSCEV se ptr2
                       base1 = getBaseValue ptr1SCEV
                       base2 = getBaseValue ptr2SCEV
                   in case (base1, base2) of
                        (Just base1, Just base2) | base1 /= base2 ->
                          alias aa (MemoryLocation.getBeforeOrAfter base1) (MemoryLocation.getBeforeOrAfter base2)
                        _ ->
                          let loops = []
                              collectLoops s = case s of
                                SCEVAddRecExpr ar -> getLoop ar : loops
                                _ -> loops
                              loops1 = SCEVTraversal.visitAll collectLoops ptr1SCEV
                              loops2 = SCEVTraversal.visitAll collectLoops ptr2SCEV
                              allLoops = loops1 ++ loops2
                              compareLoops l1 l2 = l1 == l2 || contains l1 l2
                              sortedLoops = sortBy compareLoops allLoops
                          in if any (\(l1, l2) -> not (compareLoops l1 l2)) (zip sortedLoops (tail sortedLoops))
                             then True
                             else
                               let lt = isLessThan se ptr1SCEV ptr2SCEV
                                   gt = isLessThan se ptr2SCEV ptr1SCEV
                               in if not lt && not gt
                                  then True
                                  else
                                    let (ptr1SCEV', ptr2SCEV') = if gt then (ptr2SCEV, ptr1SCEV) else (ptr1SCEV, ptr2SCEV)
                                        ty = AST.elementType (AST.typeOf ptr1)
                                        as = AST.addressSpace (AST.typeOf ptr1)
                                        dl = AST.moduleDataLayout (AST.getGlobalParent f)
                                        indexWidth = AST.getIndexSizeInBits dl as
                                        size = C.Int indexWidth (toInteger $ AST.getTypeStoreSize dl ty)
                                    in isKnownPositive se (getMinusSCEV se (getAddExpr se [ptr1SCEV', getConstant se size]) ptr2SCEV')
                 _ -> True
     else True

data GlobalDependenceAnalysis = GlobalDependenceAnalysis
  { gdaAA :: AliasAnalysis
  , gdaSE :: ScalarEvolution
  , gdaLI :: LoopInfo
  , gdaLVI :: LazyValueInfo
  , gdaF :: AST.Function
  , gdaVPCtx :: VectorPackContext
  , gdaNoAlias :: Bool
  , gdaTransitiveClosure :: Map.Map AST.Instruction (Set.Set AST.Instruction)
  }

addDependences :: GlobalDependenceAnalysis -> AST.Instruction -> [AST.Instruction] -> GlobalDependenceAnalysis
addDependences gda@GlobalDependenceAnalysis{..} i deps =
  assert (isKnownValue gdaVPCtx i) $
    let depended = Set.fromList $ map (getScalarId gdaVPCtx) deps
        depended' = Set.unions $ depended : map (\d -> Map.findWithDefault Set.empty d gdaTransitiveClosure) deps
    in gda { gdaTransitiveClosure = Map.insert i depended' gdaTransitiveClosure }

addInstruction :: GlobalDependenceAnalysis -> AST.Instruction -> GlobalDependenceAnalysis
addInstruction gda@GlobalDependenceAnalysis{..} i =
  let deps = mapMaybe (\o -> case o of
                              AST.LocalReference _ _ -> Just o
                              _ -> Nothing) (AST.operands i)
  in addDependences gda i deps

initGlobalDependenceAnalysis :: AliasAnalysis -> ScalarEvolution -> LoopInfo -> LazyValueInfo -> AST.Function -> VectorPackContext -> Bool -> GlobalDependenceAnalysis
initGlobalDependenceAnalysis aa se li lvi f vpCtx noAlias =
  let gda = GlobalDependenceAnalysis aa se li lvi f vpCtx noAlias Map.empty
      memRefs = []
      dependences = Map.empty
      rpo = computeRPO f li
      processed = Set.empty
      gda' = foldl' (\gda' bb ->
        let l = getLoopFor li bb
            isHeader = isLoopHeader li bb
            assert (not isHeader || isJust l) $
              foldl' (\gda'' i ->
                let processed' = Set.insert i processed
                    deps = mapMaybe (\v -> case v of
                                            AST.LocalReference _ _ ->
                                              let opInst = v
                                                  isLoopCarriedDep = isHeader && AST.isPHI i && maybe False (\l -> contains l opInst) l
                                              in if not isLoopCarriedDep then Just opInst else Nothing
                                            _ -> Nothing) (AST.operands i)
                    deps' = case i of
                              AST.PHI _ _ | isHeader ->
                                maybe deps (\l ->
                                  let pnLoop = getLoopFor li (AST.getParent i)
                                  in maybe deps (\pnLoop ->
                                       if getHeader pnLoop == AST.getParent i && not (contains pnLoop i)
                                       then maybe deps (\latch ->
                                              maybe deps (\i2 -> i2:deps) (AST.getIncomingValueForBlock i latch)
                                            ) (getLoopLatch pnLoop)
                                       else deps
                                     ) pnLoop
                                ) l
                              _ -> deps
                    gda''' = if not (AST.isIntrinsic i AST.ExperimentalNoAliasScopeDecl ||
                                     AST.isIntrinsic i AST.LifetimeStart ||
                                     AST.isIntrinsic i AST.LifetimeEnd) &&
                                (AST.isReturn i || noAlias || AST.mayReadOrWriteMemory i)
                             then foldl' (\gda'''' prevRef ->
                                    if AST.isReturn prevRef || AST.mayWriteToMemory prevRef || AST.mayWriteToMemory i
                                    then if isAliased i prevRef aa se li lvi
                                         then addDependences gda'''' i [prevRef]
                                         else gda''''
                                    else gda''''
                                  ) gda'' memRefs
                             else gda''
                    memRefs' = if AST.mayReadOrWriteMemory i then i:memRefs else memRefs
                in addDependences gda''' i deps'
              ) gda' (AST.basicBlockInstructions bb)
      ) gda rpo
  in gda'