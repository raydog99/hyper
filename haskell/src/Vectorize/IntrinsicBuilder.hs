{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module IntrinsicBuilder where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import Data.Text.Format (format)
import Data.Text (Text)
import qualified Data.Map as Map
import Control.Monad.State

data IntrinsicBuilder = IntrinsicBuilder
  { instWrappers :: AST.Module
  , builder :: IRB.IRBuilder ()
  }

create :: IntrinsicBuilder -> Text -> [AST.Operand] -> Word8 -> IRB.IRBuilder AST.Operand
create IntrinsicBuilder{..} name operands imm8 = do
  let wrapperName = format "intrinsic_wrapper_{}_{}" (name, imm8)
  f <- case Map.lookup wrapperName (AST.moduleDefinitions instWrappers) of
         Just (AST.GlobalDefinition f@AST.Function{}) -> return f
         _ -> error "Intrinsic wrapper undefined."
  
  let [bb] = AST.basicBlocks f
  let numArgs = length $ AST.parameters f
  assert (length operands == numArgs) "Incorrect number of operands"

  vMap <- foldM (\m (arg, op) -> do
    assert (IRB.castIsValid IRB.BitCast op (AST.typeOf arg)) "Invalid input type"
    operand <- IRB.bitcast op (AST.typeOf arg)
    return $ Map.insert arg operand m
  ) Map.empty (zip (AST.parameters f) operands)

  retVal <- foldM (\r i -> case i of
    AST.RetInst _ val _ -> return $ Just val
    _ -> do
      newI <- IRB.cloneInstruction i
      IRB.emitInstr (AST.typeOf newI) newI
      let vMap' = Map.insert i newI vMap
      IRB.remapInstructionInPlace newI vMap' [IRB.RF_NoModuleLevelChanges, IRB.RF_IgnoreMissingLocals]
      case newI of
        AST.CallInst _ _ (Right callee) _ _ _ -> do
          assert (AST.isIntrinsic callee) "Called function must be an intrinsic"
          let m = AST.moduleDefinitions $ IRB.getModule
          intrinsicDecl <- IRB.emitDefn $ AST.GlobalDefinition $
            AST.Function (AST.linkage callee) (AST.visibility callee) (AST.dllStorageClass callee)
                         (AST.callingConvention callee) (AST.returnAttributes callee) (AST.returnType callee)
                         (AST.name callee) (AST.parameters callee) (AST.functionAttributes callee)
                         (AST.section callee) (AST.comdat callee) (AST.alignment callee)
                         (AST.garbageCollectorName callee) (AST.prefix callee) (AST.basicBlocks callee)
                         (AST.personalityFunction callee) (AST.metadata callee)
          IRB.modifyInstruction newI $ \case
            AST.CallInst t s _ args attrs md -> AST.CallInst t s (Right intrinsicDecl) args attrs md
            other -> other
        _ -> return ()
      return r
  ) Nothing (AST.basicBlockInstructions bb)

  case retVal of
    Just val -> case Map.lookup val vMap of
      Just output -> return output
      Nothing -> error "Return value not found in value map"
    Nothing -> error "Wrapper not returning explicitly"

assert :: Bool -> String -> IRB.IRBuilder ()
assert cond msg = unless cond $ error msg