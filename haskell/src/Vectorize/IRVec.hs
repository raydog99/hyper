{-# LANGUAGE RecordWildCards #-}

module IRVec where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import qualified LLVM.Analysis.TargetTransformInfo as TTI

isFloat :: AST.InstructionMetadata -> Bool
isFloat op = op `elem` [AST.FAdd, AST.FSub, AST.FMul, AST.FDiv, AST.FRem]

data BinaryIROperation = BinaryIROperation
  { opcode :: AST.InstructionMetadata
  , bitwidth :: Int
  }

matchBinaryIROperation :: BinaryIROperation -> AST.Operand -> [Match] -> Maybe [Match]
matchBinaryIROperation BinaryIROperation{..} v matches =
  case v of
    AST.LocalReference _ (AST.Name name) ->
      case AST.getInstructionMetadata name of
        Just op | op == opcode && hasBitWidth v bitwidth ->
          let operands = [AST.getOperand v 0, AST.getOperand v 1]
              newMatch = Match False operands v
          in Just (newMatch : matches)
        _ -> Nothing
    _ -> Nothing

getMaximumVF :: BinaryIROperation -> TTI.TargetTransformInfo -> Int
getMaximumVF BinaryIROperation{..} tti =
  TTI.getLoadStoreVecRegBitWidth tti 0 `div` bitwidth

getBinaryIROperationName :: BinaryIROperation -> String
getBinaryIROperationName BinaryIROperation{..} =
  show opcode ++ "-i" ++ show bitwidth

data UnaryIROperation = UnaryIROperation
  { uopcode :: AST.InstructionMetadata
  , ubitwidth :: Int
  }

matchUnaryIROperation :: UnaryIROperation -> AST.Operand -> [Match] -> Maybe [Match]
matchUnaryIROperation UnaryIROperation{..} v matches =
  case v of
    AST.LocalReference _ (AST.Name name) ->
      case AST.getInstructionMetadata name of
        Just op | op == uopcode && hasBitWidth v ubitwidth ->
          let operands = [AST.getOperand v 0]
              newMatch = Match False operands v
          in Just (newMatch : matches)
        _ -> Nothing
    _ -> Nothing

getUnaryMaximumVF :: UnaryIROperation -> TTI.TargetTransformInfo -> Int
getUnaryMaximumVF UnaryIROperation{..} tti =
  TTI.getLoadStoreVecRegBitWidth tti 0 `div` ubitwidth

getUnaryIROperationName :: UnaryIROperation -> String
getUnaryIROperationName UnaryIROperation{..} =
  show uopcode ++ "-" ++ show ubitwidth

data IRVectorBinding = IRVectorBinding
  { bop :: BinaryIROperation
  , bname :: String
  , bsig :: InstSignature
  , blaneOps :: [BoundOperation]
  }

getIRVectorBindingCost :: IRVectorBinding -> TTI.TargetTransformInfo -> AST.Context -> Float
getIRVectorBindingCost IRVectorBinding{..} tti ctx =
  let scalarTy = if isFloat (opcode bop)
                 then if bitwidth bop == 32
                      then AST.FloatType
                      else AST.DoubleType
                 else AST.IntegerType (bitwidth bop)
      numElems = length blaneOps
      vecTy = AST.VectorType numElems scalarTy
  in TTI.getArithmeticInstrCost tti (opcode bop) vecTy

createIRVectorBinding :: BinaryIROperation -> Int -> IRVectorBinding
createIRVectorBinding op vectorWidth =
  let sig = InstSignature
        { inputWidths = [vectorWidth, vectorWidth]
        , outputWidths = [vectorWidth]
        , hasImm8 = False
        }
      elemWidth = bitwidth op
      numLanes = vectorWidth `div` elemWidth
      laneOps = [ BoundOperation op [(0, lo, hi), (1, lo, hi)]
                | i <- [0..numLanes-1]
                , let lo = i * elemWidth
                      hi = lo + elemWidth
                ]
  in IRVectorBinding
     { bop = op
     , bname = getBinaryIROperationName op
     , bsig = sig
     , blaneOps = laneOps
     }

emitIRVectorBinding :: IRVectorBinding -> [AST.Operand] -> IRB.IRBuilder AST.Operand
emitIRVectorBinding IRVectorBinding{..} operands =
  case operands of
    [op1, op2] -> IRB.emitInstr (AST.typeOf op1) $ AST.BinaryOperator (opcode bop) op1 op2 []
    _ -> error "Invalid number of operands"

isIRVectorBindingSupported :: IRVectorBinding -> TTI.TargetTransformInfo -> Bool
isIRVectorBindingSupported IRVectorBinding{..} tti =
  length blaneOps <= getMaximumVF bop tti