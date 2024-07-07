module IRVec

import Data.Vect
import Data.So

%default total

data Opcode = FAdd | FSub | FMul | FDiv | FRem | Add | Sub | Mul | UDiv | SDiv | URem | SRem | Shl | LShr | AShr | And | Or | Xor

isFloat : Opcode -> Bool
isFloat FAdd = True
isFloat FSub = True
isFloat FMul = True
isFloat FDiv = True
isFloat FRem = True
isFloat _ = False

record BinaryIROperation where
  constructor MkBinaryIROperation
  opcode : Opcode
  bitwidth : Nat

record Match where
  constructor MkMatch
  liveIn : Bool
  operands : List Value
  result : Value

matchBinaryIROperation : BinaryIROperation -> Value -> List Match -> Maybe (List Match)
matchBinaryIROperation op v matches =
  case v of
    BinOp binOp x y =>
      if binOp == op.opcode && hasBitWidth v op.bitwidth
      then Just $ MkMatch False [x, y] v :: matches
      else Nothing
    _ => Nothing

getMaximumVF : BinaryIROperation -> TargetTransformInfo -> Nat
getMaximumVF op tti = divNat (getLoadStoreVecRegBitWidth tti 0) op.bitwidth

getBinaryIROperationName : BinaryIROperation -> String
getBinaryIROperationName op = show op.opcode ++ "-i" ++ show op.bitwidth

record UnaryIROperation where
  constructor MkUnaryIROperation
  opcode : Opcode
  bitwidth : Nat

matchUnaryIROperation : UnaryIROperation -> Value -> List Match -> Maybe (List Match)
matchUnaryIROperation op v matches =
  case v of
    UnOp unOp x =>
      if unOp == op.opcode && hasBitWidth v op.bitwidth
      then Just $ MkMatch False [x] v :: matches
      else Nothing
    _ => Nothing

getUnaryMaximumVF : UnaryIROperation -> TargetTransformInfo -> Nat
getUnaryMaximumVF op tti = divNat (getLoadStoreVecRegBitWidth tti 0) op.bitwidth

getUnaryIROperationName : UnaryIROperation -> String
getUnaryIROperationName op = show op.opcode ++ "-" ++ show op.bitwidth

record IRVectorBinding where
  constructor MkIRVectorBinding
  op : BinaryIROperation
  name : String
  sig : InstSignature
  laneOps : List BoundOperation

getIRVectorBindingCost : IRVectorBinding -> TargetTransformInfo -> Context -> Double
getIRVectorBindingCost binding tti ctx =
  let scalarTy = if isFloat binding.op.opcode
                 then if binding.op.bitwidth == 32
                      then FloatType
                      else DoubleType
                 else IntegerType binding.op.bitwidth
      numElems = length binding.laneOps
      vecTy = VectorType numElems scalarTy
  in getArithmeticInstrCost tti binding.op.opcode vecTy

createIRVectorBinding : BinaryIROperation -> Nat -> IRVectorBinding
createIRVectorBinding op vectorWidth =
  let sig = MkInstSignature [vectorWidth, vectorWidth] [vectorWidth] False
      elemWidth = op.bitwidth
      numLanes = divNat vectorWidth elemWidth
      laneOps = [ MkBoundOperation op [(0, lo, hi), (1, lo, hi)]
                | i <- [0..numLanes-1]
                , let lo = i * elemWidth
                      hi = lo + elemWidth
                ]
  in MkIRVectorBinding op (getBinaryIROperationName op) sig laneOps

emitIRVectorBinding : IRVectorBinding -> Vect 2 Value -> IRBuilder Value
emitIRVectorBinding binding [op1, op2] =
  emitInstr (typeOf op1) $ BinaryOperator (opcode binding.op) op1 op2 []
emitIRVectorBinding _ _ = fail "Invalid number of operands"

isIRVectorBindingSupported : IRVectorBinding -> TargetTransformInfo -> Bool
isIRVectorBindingSupported binding tti =
  length binding.laneOps <= getMaximumVF binding.op tti

record IRInstTable where
  constructor MkIRInstTable
  vectorizableOps : List BinaryIROperation
  unaryOps : List UnaryIROperation
  floatToIntOps : List FloatToInt
  intToFloatOps : List IntToFloat
  vectorFloatToInts : List VectorFloatToInt
  vectorIntToFloats : List VectorIntToFloat
  vectorInsts : List IRVectorBinding
  unaryVectorInsts : List UnaryIRVectorBinding
  truncOps : List Truncate
  extOps : List Extension
  selectOps : List Select
  unaryMathOps : List UnaryMath
  binaryMathOps : List BinaryMath
  vectorTruncs : List VectorTruncate
  vectorExtensions : List VectorExtension
  vectorSelects : List VectorSelect
  vectorUnaryMathFuncs : List VectorUnaryMath
  vectorBinaryMathFuncs : List VectorBinaryMath

initIRInstTable : IRInstTable
initIRInstTable = MkIRInstTable [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []