{-# LANGUAGE RecordWildCards #-}

module ControlReifier where

import qualified Data.Map as Map
import qualified LLVM.AST as AST
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB

import ControlDependence
import DependenceAnalysis
import VLoop

data ControlReifier = ControlReifier
  { crCtx :: AST.Context
  , crReifiedValues :: Map.Map (ControlCondition, VLoop) AST.Operand
  , crInsertedInsts :: [AST.Instruction]
  }

reify :: ControlReifier -> Maybe ControlCondition -> VLoop -> IRB.IRBuilderT AST.Module AST.Operand
reify ControlReifier{..} Nothing vl = return $ AST.ConstantOperand $ AST.Constant.Int 1 1
reify cr@ControlReifier{..} (Just c) vl = do
  case Map.lookup (c, vl) crReifiedValues of
    Just v -> return v
    Nothing -> do
      reified <- case c of
        ConditionAnd And{..} -> do
          _ <- reify cr parent vl
          cond <- if isTrue
                  then return cond
                  else do
                    notInst <- IRB.not cond
                    VLoop.addInstruction vl notInst parent
                    return notInst
          VLoop.createOneHotPhi vl parent cond (AST.ConstantOperand $ AST.Constant.Int 1 0) "reified.onehot"
        ConditionOr Or{..} -> do
          foldl (\acc c2 -> do
            tmp <- IRB.or acc =<< reify cr c2 vl
            VLoop.addInstruction vl tmp Nothing
            return tmp
          ) (reify cr (head conds) vl) (tail conds)
      let cr' = cr { crReifiedValues = Map.insert (c, vl) reified crReifiedValues }
      case c of
        ConditionAnd And{..} -> void $ reify cr' complement vl
        _ -> return ()
      return reified

hasValue :: ControlReifier -> Maybe ControlCondition -> VLoop -> Bool
hasValue _ Nothing _ = True
hasValue ControlReifier{..} (Just c) vl = Map.member (c, vl) crReifiedValues

getValue :: ControlReifier -> Maybe ControlCondition -> VLoop -> AST.Operand
getValue cr@ControlReifier{..} c vl = 
  assert (hasValue cr c vl) $
  case c of
    Nothing -> AST.ConstantOperand $ AST.Constant.Int 1 1
    Just c' -> crReifiedValues Map.! (c', vl)