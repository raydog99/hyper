{-# LANGUAGE RecordWildCards #-}

module BlockBuilder where

import qualified LLVM.AST as AST
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified LLVM.IRBuilder.Constant as IRB
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

data ControlCondition
data ConditionAnd
data ConditionOr

data BlockBuilder = BlockBuilder
  { bbContext :: AST.Context
  , bbFunction :: AST.Operand
  , bbEmitCondition :: AST.Operand -> IRB.IRBuilderT ModuleBuilder AST.Operand
  , bbActiveConds :: Map.Map (Maybe ControlCondition) AST.Name
  , bbSemiActiveConds :: Map.Map ControlCondition [ControlCondition]
  , bbDummyCounter :: Int
  }

createBlockBuilder :: AST.Name -> (AST.Operand -> IRB.IRBuilderT ModuleBuilder AST.Operand) -> IRB.ModuleBuilder BlockBuilder
createBlockBuilder entryBB emitCondition = do
  ctx <- IRB.liftIRState IRB.getContext
  f <- IRB.liftIRState $ IRB.getFunction entryBB
  return BlockBuilder
    { bbContext = ctx
    , bbFunction = f
    , bbEmitCondition = emitCondition
    , bbActiveConds = Map.singleton Nothing entryBB
    , bbSemiActiveConds = Map.empty
    , bbDummyCounter = 0
    }

createBlock :: BlockBuilder -> IRB.IRBuilderT ModuleBuilder AST.Name
createBlock BlockBuilder{..} = IRB.freshName "block"

data ConditionEmitter = ConditionEmitter
  { ceBuilder :: IRB.IRBuilderT ModuleBuilder
  , ceCommon :: Maybe ControlCondition
  , ceEmitCondition :: AST.Operand -> IRB.IRBuilderT ModuleBuilder AST.Operand
  , ceEmitted :: Map.Map ControlCondition AST.Operand
  }

createConditionEmitter :: AST.Name -> Maybe ControlCondition -> (AST.Operand -> IRB.IRBuilderT ModuleBuilder AST.Operand) -> ConditionEmitter
createConditionEmitter bb common emitCondition =
  ConditionEmitter
    { ceBuilder = IRB.block bb
    , ceCommon = common
    , ceEmitCondition = emitCondition
    , ceEmitted = Map.empty
    }

emit :: ConditionEmitter -> Maybe ControlCondition -> IRB.IRBuilderT ModuleBuilder AST.Operand
emit ce Nothing = IRB.bit 1
emit ce (Just c)
  | Just c == ceCommon ce = IRB.bit 1
  | otherwise = case c of
      ConditionAnd{} -> emitAnd ce c
      ConditionOr{} -> emitOr ce c

emitAnd :: ConditionEmitter -> ConditionAnd -> IRB.IRBuilderT ModuleBuilder AST.Operand
emitAnd ce and = do
  case Map.lookup and (ceEmitted ce) of
    Just v -> return v
    Nothing -> do
      parent <- emit ce (andParent and)
      cond <- ceEmitCondition ce (andCond and)
      v <- if andIsTrue and
           then IRB.and parent cond
           else IRB.and parent =<< IRB.not cond
      return $ Map.insert and v (ceEmitted ce)

emitOr :: ConditionEmitter -> ConditionOr -> IRB.IRBuilderT ModuleBuilder AST.Operand
emitOr ce or = do
  case Map.lookup or (ceEmitted ce) of
    Just v -> return v
    Nothing -> do
      v <- emitDisjunction ce (orConds or)
      return $ Map.insert or v (ceEmitted ce)

emitDisjunction :: ConditionEmitter -> [ControlCondition] -> IRB.IRBuilderT ModuleBuilder AST.Operand
emitDisjunction ce conds = do
  values <- mapM (emit ce . Just) conds
  IRB.or values

getBlockFor :: BlockBuilder -> ControlCondition -> IRB.IRBuilderT ModuleBuilder AST.Name
getBlockFor bb@BlockBuilder{..} c = do
  case Map.lookup (Just c) bbActiveConds of
    Just block -> return block
    Nothing -> do
      if c `Map.member` bbSemiActiveConds
        then do
          conds <- getActiveConds bb c
          newBB <- createBlock bb
          mapM_ (branchToNewBlock newBB) conds
          return newBB
        else case c of
          ConditionAnd{} -> handleConditionAnd bb c
          ConditionOr{} -> handleConditionOr bb c
  where
    branchToNewBlock newBB cond = do
      oldBB <- fromMaybe (error "Active condition not found") $ Map.lookup (Just cond) bbActiveConds
      IRB.br newBB

handleConditionAnd :: BlockBuilder -> ConditionAnd -> IRB.IRBuilderT ModuleBuilder AST.Name
handleConditionAnd bb@BlockBuilder{..} and = do
  ifTrue <- createBlock bb
  ifFalse <- createBlock bb
  parentBB <- getBlockFor bb (andParent and)
  cond <- bbEmitCondition (andCond and)
  IRB.condBr cond ifTrue ifFalse
  let resultBB = if andIsTrue and then ifTrue else ifFalse
  return resultBB

handleConditionOr :: BlockBuilder -> ConditionOr -> IRB.IRBuilderT ModuleBuilder AST.Name
handleConditionOr bb@BlockBuilder{..} or = do
  let commonC = orGreatestCommonCond or
  conds <- if commonC `Map.notMember` bbSemiActiveConds
           then do
             _ <- getBlockFor bb commonC
             return [commonC]
           else getActiveConds bb commonC
  newBB <- createBlock bb
  auxBB <- createBlock bb
  drainBB <- createBlock bb
  if null (orConds or)
    then IRB.br drainBB
    else do
      let ce = createConditionEmitter auxBB (Just commonC) bbEmitCondition
      cond <- emitDisjunction ce (orConds or)
      IRB.condBr cond newBB drainBB
  return newBB

getActiveConds :: BlockBuilder -> ControlCondition -> IRB.IRBuilderT ModuleBuilder [ControlCondition]
getActiveConds BlockBuilder{..} c = do
  return $ maybe [] id $ Map.lookup c bbSemiActiveConds

getDummyCondition :: BlockBuilder -> ControlCondition
getDummyCondition bb = 
  return ()

setBlockForCondition :: BlockBuilder -> AST.Name -> ControlCondition -> IRB.IRBuilderT ModuleBuilder ()
setBlockForCondition BlockBuilder{..} block c = do
  return ()