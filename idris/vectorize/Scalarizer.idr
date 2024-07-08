module Scalarizer

import Data.Vect
import Data.SortedMap
import Control.Monad.State

%default total

data LLVMType = IntType Nat
              | FloatType
              | PointerType LLVMType
              | VectorType Nat LLVMType
              | VoidType

data LLVMValue : LLVMType -> Type where
  ConstInt : (n : Nat) -> LLVMValue (IntType bits)
  ConstFloat : Double -> LLVMValue FloatType
  ConstVector : Vect n (LLVMValue t) -> LLVMValue (VectorType n t)
  Instruction : (ty : LLVMType) -> String -> LLVMValue ty

data Instruction : Type where
  SelectInst : LLVMValue (IntType 1) -> LLVMValue a -> LLVMValue a -> Instruction
  ICmpInst : (pred : String) -> LLVMValue (IntType n) -> LLVMValue (IntType n) -> Instruction
  FCmpInst : (pred : String) -> LLVMValue FloatType -> LLVMValue FloatType -> Instruction
  BinaryInst : (op : String) -> LLVMValue a -> LLVMValue a -> Instruction
  CastInst : (op : String) -> LLVMValue a -> LLVMType -> Instruction
  LoadInst : LLVMValue (PointerType a) -> Instruction
  StoreInst : LLVMValue a -> LLVMValue (PointerType a) -> Instruction
  CallInst : (fname : String) -> Vect n (LLVMValue a) -> Instruction
  PHIInst : Vect n (LLVMValue a, String) -> Instruction

record BasicBlock where
  constructor MkBasicBlock
  name : String
  instructions : List Instruction

record Function where
  constructor MkFunction
  name : String
  basicBlocks : List BasicBlock

record Module where
  constructor MkModule
  name : String
  functions : List Function

record ScalarizerState where
  constructor MkScalarizerState
  scattered : SortedMap String (Vect n (Maybe (LLVMValue a)))
  gathered : List (Instruction, Vect n (LLVMValue a))
  potentiallyDeadInstrs : List Instruction

Scalarizer : Type -> Type
Scalarizer = State ScalarizerState

scatter : {n : Nat} -> {a : LLVMType} -> 
          LLVMValue (VectorType n a) -> 
          Scalarizer (Vect n (LLVMValue a))
scatter v = do
  state <- get
  case lookup (show v) state.scattered of
    Just vec => pure $ map (fromMaybe (Instruction a "dummy")) vec
    Nothing => do
      let newVec = replicate n (Instruction a "scattered")
      put $ record { scattered $= insert (show v) newVec } state
      pure newVec

gather : Instruction -> Vect n (LLVMValue a) -> Scalarizer ()
gather inst vec = modify $ \s => record { gathered $= ((inst, vec) ::) } s

visitSelectInst : {n : Nat} -> {a : LLVMType} ->
                  LLVMValue (IntType 1) -> 
                  LLVMValue (VectorType n a) -> 
                  LLVMValue (VectorType n a) -> 
                  Scalarizer Bool
visitSelectInst cond trueVal falseVal = do
  vop1 <- scatter trueVal
  vop2 <- scatter falseVal
  let res = zipWith (\t, f => Instruction a "select") vop1 vop2
  gather (SelectInst cond trueVal falseVal) res
  pure True

visitCmpInst : {n : Nat} -> {a : LLVMType} -> 
               (isFCmp : Bool) -> 
               (pred : String) -> 
               LLVMValue (VectorType n a) -> 
               LLVMValue (VectorType n a) -> 
               Scalarizer Bool
visitCmpInst isFCmp pred op1 op2 = do
  vop1 <- scatter op1
  vop2 <- scatter op2
  let res = zipWith (\o1, o2 => Instruction (IntType 1) "cmp") vop1 vop2
  gather (if isFCmp then FCmpInst pred op1 op2 else ICmpInst pred op1 op2) res
  pure True

visitBinaryInst : {n : Nat} -> {a : LLVMType} ->
                  (op : String) -> 
                  LLVMValue (VectorType n a) -> 
                  LLVMValue (VectorType n a) -> 
                  Scalarizer Bool
visitBinaryInst op lhs rhs = do
  vop1 <- scatter lhs
  vop2 <- scatter rhs
  let res = zipWith (\l, r => Instruction a "binop") vop1 vop2
  gather (BinaryInst op lhs rhs) res
  pure True

visitCastInst : {n : Nat} -> {a, b : LLVMType} ->
                (op : String) -> 
                LLVMValue (VectorType n a) -> 
                LLVMType -> 
                Scalarizer Bool
visitCastInst op val destTy = do
  vop <- scatter val
  let res = map (\v => Instruction destTy "cast") vop
  gather (CastInst op val destTy) res
  pure True

visitLoadInst : {n : Nat} -> {a : LLVMType} ->
                LLVMValue (VectorType n (PointerType a)) -> 
                Scalarizer Bool
visitLoadInst ptr = do
  vptr <- scatter ptr
  let res = map (\p => Instruction a "load") vptr
  gather (LoadInst ptr) res
  pure True

visitStoreInst : {n : Nat} -> {a : LLVMType} ->
                 LLVMValue (VectorType n a) -> 
                 LLVMValue (VectorType n (PointerType a)) -> 
                 Scalarizer Bool
visitStoreInst val ptr = do
  vval <- scatter val
  vptr <- scatter ptr
  let res = zipWith (\v, p => Instruction VoidType "store") vval vptr
  gather (StoreInst val ptr) res
  pure True

visitCallInst : {n, m : Nat} -> {a : LLVMType} ->
                (fname : String) -> 
                Vect m (LLVMValue (VectorType n a)) -> 
                Scalarizer Bool
visitCallInst fname args = do
  vargs <- traverse scatter args
  let res = transpose vargs
  let calls = map (\argSet => Instruction a "call") res
  gather (CallInst fname (concat vargs)) calls
  pure True

visitPHIInst : {n, m : Nat} -> {a : LLVMType} ->
               Vect m (LLVMValue (VectorType n a), String) -> 
               Scalarizer Bool
visitPHIInst incomings = do
  vincs <- traverse (\(val, label) => do
                        vval <- scatter val
                        pure $ map (\v => (v, label)) vval) incomings
  let res = transpose vincs
  let phis = map (\incSet => Instruction a "phi") res
  gather (PHIInst (concat vincs)) phis
  pure True

visitInstruction : Instruction -> Scalarizer Bool
visitInstruction (SelectInst cond t f) = visitSelectInst cond t f
visitInstruction (ICmpInst pred a b) = visitCmpInst False pred a b
visitInstruction (FCmpInst pred a b) = visitCmpInst True pred a b
visitInstruction (BinaryInst op a b) = visitBinaryInst op a b
visitInstruction (CastInst op val ty) = visitCastInst op val ty
visitInstruction (LoadInst ptr) = visitLoadInst ptr
visitInstruction (StoreInst val ptr) = visitStoreInst val ptr
visitInstruction (CallInst fname args) = visitCallInst fname args
visitInstruction (PHIInst incs) = visitPHIInst incs

visitBasicBlock : BasicBlock -> Scalarizer Bool
visitBasicBlock bb = do
  results <- traverse visitInstruction bb.instructions
  pure $ any id results

visitFunction : Function -> Scalarizer Bool
visitFunction f = do
  results <- traverse visitBasicBlock f.basicBlocks
  changed <- pure $ any id results
  finishChanged <- finish
  pure $ changed || finishChanged

finish : Scalarizer Bool
finish = do
  state <- get
  if null state.gathered && isEmpty state.scattered
    then pure False
    else do
      put $ MkScalarizerState empty [] []
      pure True

runOnModule : Module -> (Bool, ScalarizerState)
runOnModule m = 
  let initialState = MkScalarizerState empty [] []
      (changed, finalState) = runState (traverse visitFunction m.functions) initialState
  in (any id changed, finalState)