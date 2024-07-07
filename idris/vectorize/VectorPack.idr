module VectorPack

import Data.Vect
import Data.SortedMap
import Data.SortedSet

%default total

record VectorPack where
  constructor MkVectorPack
  kind : PackKind
  producer : Maybe InstBinding
  matches : Vect n (Maybe Operation.Match)
  loads : Vect m LLVMValue
  stores : Vect p LLVMValue
  phis : Vect q LLVMValue
  rdx : Maybe ReductionInfo
  rdxLen : Nat
  geps : Vect r LLVMValue
  gammas : Vect s Gamma
  cmps : Vect t LLVMValue
  cp : Maybe ConditionPack
  isGatherScatter : Bool
  operandPacks : List OperandPack
  orderedValues : List LLVMValue
  cost : Double
  producingCost : Double
  vpCtx : VectorPackContext

computeOperandPacksForGeneral : VectorPack -> List OperandPack
computeOperandPacksForGeneral vp =
  let sig = getSignature $ fromMaybe (believe_me ()) vp.producer
      numInputs = numInputs sig
      laneOps = getLaneOps $ fromMaybe (believe_me ()) vp.producer
      numLanes = length laneOps
      
      computeOperandPack : Nat -> OperandPack
      computeOperandPack i =
        let inputValues = sort $ concat $ map (getInputValuesForInput i) $ zip [0..numLanes-1] laneOps
            elementSize = maybe 0 sliceSize (head' inputValues)
            
            fillOperandPack : Nat -> List BoundInput -> List LLVMValue
            fillOperandPack curOffset [] = 
              if curOffset >= inputSize then []
              else LLVMUndef :: fillOperandPack (curOffset + elementSize) []
            fillOperandPack curOffset (bv::rest) =
              if curOffset < sliceLo bv.slice
              then LLVMUndef :: fillOperandPack (curOffset + elementSize) (bv::rest)
              else bv.v :: fillOperandPack (curOffset + elementSize) rest
            
            inputSize = getInputBitwidth sig i
            op = fillOperandPack 0 inputValues
        in if isNil op || (head op /= LLVMUndef && not (all (== head op) op))
           then MkOperandPack op Nothing
           else MkOperandPack op $ Just $ 
             LLVMVectorType (length op) (LLVMIntegerType elementSize)
      
      getInputValuesForInput : Nat -> (Nat, LaneOp) -> List BoundInput
      getInputValuesForInput i (j, laneOp) = 
        [ MkBoundInput bs v
        | (k, bs) <- zip [0..] $ getBoundSlices laneOp
        , inputSliceInputId bs == i
        , let v = maybe LLVMUndef (\m => index k m.inputs) (index j vp.matches)
        ]
  
  in map computeOperandPack [0..numInputs-1]