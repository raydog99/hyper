module IntrinsicBuilder

import Data.SortedMap
import LLVM.AST
import LLVM.IRBuilder

%default total

record IntrinsicBuilder where
  constructor MkIntrinsicBuilder
  instWrappers : Module
  builder : IRBuilder ()

create : IntrinsicBuilder -> String -> List Operand -> Bits8 -> IRBuilder Operand
create (MkIntrinsicBuilder instWrappers builder) name operands imm8 = do
  let wrapperName = "intrinsic_wrapper_" ++ name ++ "_" ++ show imm8
  f <- case lookup wrapperName (moduleDefinitions instWrappers) of
         Just (GlobalDefinition f@(Function {})) => pure f
         _ => error "Intrinsic wrapper undefined."

  let [bb] = basicBlocks f
  let numArgs = length $ parameters f
  assert_total $ length operands == numArgs

  vMap <- foldl (\m, (arg, op) => do
    assert_total $ castIsValid BitCast op (typeOf arg)
    operand <- bitcast op (typeOf arg)
    pure $ insert arg operand m
  ) empty (zip (parameters f) operands)

  retVal <- foldl (\r, i => case i of
    RetInst _ val _ => pure $ Just val
    _ => do
      newI <- cloneInstruction i
      emitInstr (typeOf newI) newI
      let vMap' = insert i newI vMap
      remapInstructionInPlace newI vMap' [RF_NoModuleLevelChanges, RF_IgnoreMissingLocals]
      case newI of
        CallInst _ _ (Right callee) _ _ _ => do
          assert_total $ isIntrinsic callee
          m <- getModule
          intrinsicDecl <- emitDefn $ GlobalDefinition $
            Function (linkage callee) (visibility callee) (dllStorageClass callee)
                     (callingConvention callee) (returnAttributes callee) (returnType callee)
                     (name callee) (parameters callee) (functionAttributes callee)
                     (section callee) (comdat callee) (alignment callee)
                     (garbageCollectorName callee) (prefix callee) (basicBlocks callee)
                     (personalityFunction callee) (metadata callee)
          modifyInstruction newI $ \case
            CallInst t s _ args attrs md => CallInst t s (Right intrinsicDecl) args attrs md
            other => other
        _ => pure ()
      pure r
  ) Nothing (basicBlockInstructions bb)

  case retVal of
    Just val => case lookup val vMap of
      Just output => pure output
      Nothing => error "Return value not found in value map"
    Nothing => error "Wrapper not returning explicitly"