{-# LANGUAGE RecordWildCards #-}

module VectorPack where

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as IRB
import qualified LLVM.IRBuilder.Instruction as IRB
import Data.List (sort)
import Data.Maybe (catMaybes)

import InstBinding
import OperandPack
import ConditionPack
import ReductionInfo

data VectorPack = VectorPack
  { vpKind :: PackKind
  , vpProducer :: Maybe InstBinding
  , vpMatches :: [Maybe Operation.Match]
  , vpLoads :: [AST.Instruction]
  , vpStores :: [AST.Instruction]
  , vpPHIs :: [AST.Instruction]
  , vpRdx :: Maybe ReductionInfo
  , vpRdxLen :: Int
  , vpGEPs :: [AST.Instruction]
  , vpGammas :: [Gamma]
  , vpCmps :: [AST.Instruction]
  , vpCP :: Maybe ConditionPack
  , vpIsGatherScatter :: Bool
  , vpOperandPacks :: [OperandPack]
  , vpOrderedValues :: [AST.Operand]
  , vpCost :: Double
  , vpProducingCost :: Double
  , vpCtx :: VectorPackContext
  }

computeOperandPacksForGeneral :: VectorPack -> [OperandPack]
computeOperandPacksForGeneral VectorPack{..} = 
  let sig = getSignature $ fromJust vpProducer
      numInputs = numInputs sig
      laneOps = getLaneOps $ fromJust vpProducer
      numLanes = length laneOps
      
      computeOperandPack :: Int -> OperandPack
      computeOperandPack i =
        let inputValues = sort $ concatMap (getInputValuesForInput i) $ zip [0..] laneOps
            elementSize = inputSliceSize $ slice $ head inputValues
            
            fillOperandPack :: Int -> [BoundInput] -> [AST.Operand]
            fillOperandPack curOffset [] 
              | curOffset >= inputSize = []
              | otherwise = AST.ConstantOperand C.UndefVector : fillOperandPack (curOffset + elementSize) []
            fillOperandPack curOffset (bv:rest)
              | curOffset < inputSliceLo (slice bv) = 
                  AST.ConstantOperand C.UndefVector : fillOperandPack (curOffset + elementSize) (bv:rest)
              | otherwise = v bv : fillOperandPack (curOffset + elementSize) rest
            
            inputSize = getInputBitwidth sig i
            op = fillOperandPack 0 inputValues
        in if null op || head op /= AST.ConstantOperand C.UndefVector || not (all (== head op) op)
           then OperandPack op Nothing
           else OperandPack op $ Just $ 
             AST.VectorType (fromIntegral $ length op) (AST.IntegerType $ fromIntegral elementSize)
      
      getInputValuesForInput :: Int -> (Int, LaneOp) -> [BoundInput]
      getInputValuesForInput i (j, laneOp) = 
        [ BoundInput bs v
        | (k, bs) <- zip [0..] $ getBoundSlices laneOp
        , inputSliceInputId bs == i
        , let v = case vpMatches !! j of
                    Just m  -> inputs m !! k
                    Nothing -> AST.ConstantOperand C.UndefVector
        ]
  
  in map computeOperandPack [0..numInputs-1]