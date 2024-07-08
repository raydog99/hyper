{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Scalarizer where

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Instruction
import LLVM.AST.Operand
import LLVM.AST.Type
import LLVM.Context
import LLVM.Module
import LLVM.PassManager
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe (fromMaybe)

type ValueVector = [Operand]
type ScatterMap = Map.Map Operand ValueVector
type GatherList = [(Instruction, ValueVector)]

data LoopUnrollResult = Unmodified | PartiallyUnrolled | FullyUnrolled
  deriving (Show, Eq)

data UnrolledValue = UnrolledValue
  { uvIteration :: Int
  , uvOriginal :: Operand
  } deriving (Show, Eq)

data VectorLayout = VectorLayout
  { vlVecTy :: Type
  , vlElemTy :: Type
  , vlVecAlign :: Int
  , vlElemSize :: Integer
  } deriving (Show, Eq)

data ScalarizerState = ScalarizerState
  { ssScattered :: ScatterMap
  , ssGathered :: GatherList
  , ssPotentiallyDeadInstrs :: [Instruction]
  , ssParallelLoopAccessMDKind :: String
  }

-- Helper functions
skipPastPhiNodesAndDbg :: [Instruction] -> [Instruction]
skipPastPhiNodesAndDbg = dropWhile (\case
  Phi {} -> True
  Call { calleeOperand = (ConstantOperand (GlobalReference _ name)) }
    | name == "llvm.dbg.value" -> True
  _ -> False)

scatter :: IRB.IRBuilder m => Instruction -> Operand -> ScalarizerState -> m (ValueVector, ScalarizerState)
scatter point v state@ScalarizerState{..} = do
  let scatterHelper bb v = do
        let ty = typeOf v
            size = case ty of
              VectorType n _ -> n
              PointerType (VectorType n _) _ -> n
              _ -> error "Unexpected type in scatterHelper"
        cache <- fromMaybe (replicate size Nothing) $ Map.lookup v ssScattered
        (newCache, newState) <- foldM (\(acc, st) i -> case acc !! i of
          Just val -> return (acc, st)
          Nothing -> do
            let name = show v ++ ".i" ++ show i
            newVal <- case ty of
              PointerType _ _ ->
                if i == 0
                  then IRB.bitcast v (PointerType (elementType ty) (addrSpace ty)) name
                  else do
                    gep <- IRB.gep v [ConstantOperand $ Int 32 (fromIntegral i)]
                    IRB.load gep 0 name
              _ -> IRB.extractElement v (ConstantOperand $ Int 32 (fromIntegral i)) name
            let newAcc = take i acc ++ [Just newVal] ++ drop (i + 1) acc
            return (newAcc, st { ssScattered = Map.insert v newAcc (ssScattered st) })
          ) (cache, state) [0..size-1]
        return (map fromJust newCache, newState)
  case v of
    LocalReference _ _ -> do
      bb <- IRB.currentBlock
      scatterHelper bb v
    _ -> scatterHelper point v

gather :: IRB.IRBuilder m => Instruction -> ValueVector -> ScalarizerState -> m ScalarizerState
gather op cv state@ScalarizerState{..} = do
  let newState = state { ssGathered = (op, cv) : ssGathered }
  transferMetadataAndIRFlags op cv newState
  case Map.lookup op ssScattered of
    Just sv -> do
      let updateScattered i v = case (sv !! i, v) of
            (Just oldV, newV) | oldV /= newV -> do
              IRB.emitInstr (typeOf oldV) $ Do $ LLVM.IRBuilder.Instruction.InsertValue oldV newV []
              return $ state { ssPotentiallyDeadInstrs = oldV : ssPotentiallyDeadInstrs }
            _ -> return state
      foldM (\st (i, v) -> updateScattered i v) newState (zip [0..] cv)
    Nothing -> return newState { ssScattered = Map.insert op cv ssScattered }

canTransferMetadata :: String -> Bool
canTransferMetadata tag =
  tag `elem` ["tbaa", "fpmath", "tbaa.struct", "invariant.load", "alias.scope", "noalias", "llvm.mem.parallel_loop_access", "access_group"]

transferMetadataAndIRFlags :: IRB.IRBuilder m => Instruction -> ValueVector -> ScalarizerState -> m ()
transferMetadataAndIRFlags op cv ScalarizerState{..} = do
  let metadata = instructionMetadata op
  forM_ cv $ \v -> case v of
    LocalReference _ _ -> do
      forM_ metadata $ \(tag, md) ->
        when (canTransferMetadata tag) $
          IRB.setMetadata v tag md
    _ -> return ()

getVectorLayout :: Type -> Int -> Maybe VectorLayout
getVectorLayout ty alignment = case ty of
  VectorType n elemTy -> Just $ VectorLayout
    { vlVecTy = ty
    , vlElemTy = elemTy
    , vlVecAlign = alignment
    , vlElemSize = fromIntegral $ sizeOf elemTy
    }
  _ -> Nothing

visitSelectInst :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitSelectInst si state = case typeOf si of
  VectorType n _ -> do
    (vop1, state1) <- scatter si (operand si 1) state
    (vop2, state2) <- scatter si (operand si 2) state1
    res <- forM [0..n-1] $ \i -> do
      let name = show si ++ ".i" ++ show i
      case typeOf (operand si 0) of
        VectorType _ _ -> do
          (vop0, _) <- scatter si (operand si 0) state2
          IRB.select (vop0 !! i) (vop1 !! i) (vop2 !! i) name
        _ -> IRB.select (operand si 0) (vop1 !! i) (vop2 !! i) name
    newState <- gather si res state2
    return (True, newState)
  _ -> return (False, state)

visitCmpInst :: IRB.IRBuilder m => (Operand -> Operand -> String -> m Operand) -> Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitCmpInst cmpBuilder ci state = case typeOf ci of
  VectorType n _ -> do
    (vop0, state1) <- scatter ci (operand ci 0) state
    (vop1, state2) <- scatter ci (operand ci 1) state1
    res <- forM [0..n-1] $ \i -> do
      let name = show ci ++ ".i" ++ show i
      cmpBuilder (vop0 !! i) (vop1 !! i) name
    newState <- gather ci res state2
    return (True, newState)
  _ -> return (False, state)

visitBinaryInst :: IRB.IRBuilder m => (Operand -> Operand -> String -> m Operand) -> Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitBinaryInst binOpBuilder bi state = case typeOf bi of
  VectorType n _ -> do
    (vop0, state1) <- scatter bi (operand bi 0) state
    (vop1, state2) <- scatter bi (operand bi 1) state1
    res <- forM [0..n-1] $ \i -> do
      let name = show bi ++ ".i" ++ show i
      binOpBuilder (vop0 !! i) (vop1 !! i) name
    newState <- gather bi res state2
    return (True, newState)
  _ -> return (False, state)

visitGetElementPtrInst :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitGetElementPtrInst gepi state = case typeOf gepi of
  VectorType n _ -> do
    (vbase, state1) <- scatter gepi (operand gepi 0) state
    vindices <- forM [1..length (operands gepi) - 1] $ \i ->
      scatter gepi (operand gepi i) state1
    res <- forM [0..n-1] $ \i -> do
      let name = show gepi ++ ".i" ++ show i
          indices = map (!! i) vindices
      IRB.gep (vbase !! i) indices name
    newState <- gather gepi res state1
    return (True, newState)
  _ -> return (False, state)

visitCastInst :: IRB.IRBuilder m => (Operand -> Type -> String -> m Operand) -> Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitCastInst castBuilder ci state = case typeOf ci of
  VectorType n _ -> do
    (vop, state1) <- scatter ci (operand ci 0) state
    res <- forM [0..n-1] $ \i -> do
      let name = show ci ++ ".i" ++ show i
      castBuilder (vop !! i) (elementType $ typeOf ci) name
    newState <- gather ci res state1
    return (True, newState)
  _ -> return (False, state)

visitLoadInst :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitLoadInst li state@ScalarizerState{..} =
  if not scalarizeLoadStore || isVolatile li
  then return (False, state)
  else case getVectorLayout (typeOf li) (alignment li) of
    Just VectorLayout{..} -> do
      (vptr, state1) <- scatter li (operand li 0) state
      res <- forM [0..length vptr - 1] $ \i -> do
        let name = show li ++ ".i" ++ show i
        IRB.load (vptr !! i) (fromIntegral vlVecAlign) name
      newState <- gather li res state1
      return (True, newState)
    Nothing -> return (False, state)

visitStoreInst :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitStoreInst si state@ScalarizerState{..} =
  if not scalarizeLoadStore || isVolatile si
  then return (False, state)
  else case getVectorLayout (typeOf $ operand si 0) (alignment si) of
    Just VectorLayout{..} -> do
      (vptr, state1) <- scatter si (operand si 1) state
      (vval, state2) <- scatter si (operand si 0) state1
      forM_ [0..length vptr - 1] $ \i ->
        IRB.store (vptr !! i) (vval !! i) (fromIntegral vlVecAlign)
      return (True, state2)
    Nothing -> return (False, state)

visitCallInst :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitCallInst ci state = case typeOf ci of
  VectorType n _ -> do
    vargs <- forM [0..length (operands ci) - 2] $ \i ->
      scatter ci (operand ci i) state
    let callee = operand ci (length (operands ci) - 1)
    res <- forM [0..n-1] $ \i -> do
      let name = show ci ++ ".i" ++ show i
          args = map (!! i) vargs
      IRB.call callee args name
    newState <- gather ci res state
    return (True, newState)
  _ -> return (False, state)

visitPHINode :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitPHINode phi state = case typeOf phi of
  VectorType n _ -> do
    let incomingVals = zip (incomingValues phi) (incomingBlocks phi)
    res <- forM [0..n-1] $ \i -> do
      let name = show phi ++ ".i" ++ show i
      newPhi <- IRB.phi [] name
      forM_ incomingVals $ \(val, block) -> do
        (vop, _) <- scatter phi val state
        IRB.addIncoming newPhi [(vop !! i, block)]
      return newPhi
    newState <- gather phi res state
    return (True, newState)
  _ -> return (False, state)

visitInstruction :: IRB.IRBuilder m => Instruction -> ScalarizerState -> m (Bool, ScalarizerState)
visitInstruction instr state = case instr of
  Select {} -> visitSelectInst instr state
  ICmp {} -> visitCmpInst IRB.icmp instr state
  FCmp {} -> visitCmpInst IRB.fcmp instr state
  Add {} -> visitBinaryInst IRB.add instr state
  FAdd {} -> visitBinaryInst IRB.fadd instr state
  Sub {} -> visitBinaryInst IRB.sub instr state
  FSub {} -> visitBinaryInst IRB.fsub instr state
  Mul {} -> visitBinaryInst IRB.mul instr state
  FMul {} -> visitBinaryInst IRB.fmul instr state
  UDiv {} -> visitBinaryInst IRB.udiv instr state
  SDiv {} -> visitBinaryInst IRB.sdiv instr state
  FDiv {} -> visitBinaryInst IRB.fdiv instr state
  URem {} -> visitBinaryInst IRB.urem instr state
  SRem {} -> visitBinaryInst IRB.srem instr state
  FRem {} -> visitBinaryInst IRB.frem instr state
  GetElementPtr {} -> visitGetElementPtrInst instr state
  Trunc {} -> visitCastInst IRB.trunc instr state
  ZExt {} -> visitCastInst IRB.zext instr state
  SExt {} -> visitCastInst IRB.sext instr state
  FPToUI {} -> visitCastInst IRB.fptoui instr state
  FPToSI {} -> visitCastInst IRB.fptosi instr state
  UIToFP {} -> visitCastInst IRB.uitofp instr state
  SIToFP {} -> visitCastInst IRB.sitofp instr state
  FPTrunc {} -> visitCastInst IRB.fptrunc instr state
  FPExt {} -> visitCastInst IRB.fpext instr state
  PtrToInt {} -> visitCastInst IRB.ptrtoint instr state
  IntToPtr {} -> visitCastInst IRB.inttoptr instr state
  BitCast {} -> visitCastInst IRB.bitcast instr state
  AddrSpaceCast {} -> visitCastInst IRB.addrspacecast instr state
  Load {} -> visitLoadInst instr state
  Store {} -> visitStoreInst instr state
  Call {} -> visitCallInst instr state
  Phi {} -> visitPHINode instr state
  _ -> return (False, state)

visitBasicBlock :: IRB.IRBuilder m => BasicBlock -> ScalarizerState -> m (Bool, ScalarizerState)
visitBasicBlock bb state = do
  (changed, finalState) <- foldM (\(changed, st) instr -> do
    (instrChanged, newState) <- visitInstruction instr st
    return (changed || instrChanged, newState)
  ) (False, state) (bbInstructions bb)
  return (changed, finalState)

visitFunction :: IRB.IRBuilder m => Function -> ScalarizerState -> m (Bool, ScalarizerState)
visitFunction func state = do
  (changed, finalState) <- foldM (\(changed, st) bb -> do
    (bbChanged, newState) <- visitBasicBlock bb st
    return (changed || bbChanged, newState)
  ) (False, state) (functionBlocks func)
  let finalChanged = changed || not (null (ssGathered finalState)) || not (Map.null (ssScattered finalState))
  if finalChanged
    then finish finalState
    else return (finalChanged, finalState)

finish :: IRB.IRBuilder m => ScalarizerState -> m (Bool, ScalarizerState)
finish state@ScalarizerState{..} = do
  forM_ ssGathered $ \(op, cv) -> do
    unless (isUndef op) $ do
      let ty = typeOf op
      res <- case ty of
        VectorType n _ -> do
          let bb = instructionParent op
          builder <- IRB.saveIRBuilderState
          case op of
            Phi {} -> IRB.positionAtEnd bb
            _ -> return ()
          newVal <- foldM (\acc (i, v) -> do
            let name = show op ++ ".upto" ++ show i
            IRB.insertElement acc v (ConstantOperand $ Int 32 (fromIntegral i)) name
          ) (UndefValue ty) (zip [0..] cv)
          IRB.loadIRBuilderState builder
          return newVal
        _ -> do
          assert (length cv == 1 && ty == typeOf (head cv)) $ return ()
          return (head cv)
      when (op /= res) $ do
        IRB.emitInstr (typeOf op) $ Do $ LLVM.IRBuilder.Instruction.InsertValue op res []
        modifySTRef (ssPotentiallyDeadInstrs) (op :)

  forM_ ssPotentiallyDeadInstrs $ \instr ->
    when (isConstant instr || null (instructionUses instr)) $
      IRB.emitInstr VoidType $ Do $ LLVM.IRBuilder.Instruction.Delete instr

  return (True, state { ssGathered = [], ssScattered = Map.empty, ssPotentiallyDeadInstrs = [] })

runOnFunction :: IRB.IRBuilder m => Function -> m Bool
runOnFunction func = do
  let parallelLoopAccessMDKind = "llvm.mem.parallel_loop_access"
      initialState = ScalarizerState
        { ssScattered = Map.empty
        , ssGathered = []
        , ssPotentiallyDeadInstrs = []
        , ssParallelLoopAccessMDKind = parallelLoopAccessMDKind
        }
  (changed, _) <- visitFunction func initialState
  return changed

runOnModule :: IRB.IRBuilder Module => Module -> m Bool
runOnModule mod = do
  changes <- mapM runOnFunction (moduleDefinitions mod)
  return (or changes)

scalarizer :: Pass
scalarizer = Pass
  { passName = "Scalarizer"
  , passDescription = "Scalarize vector operations"
  , runOnModule = runOnModule
  }