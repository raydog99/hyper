package hyper

import (
  "sort"

  "github.com/llir/llvm/ir"
  "github.com/llir/llvm/ir/constant"
  "github.com/llir/llvm/ir/types"
  "github.com/llir/llvm/ir/value"
)

type VectorPack struct {
  Kind             PackKind
  Producer         *InstBinding
  Matches          []*Operation.Match
  Loads            []*ir.InstLoad
  Stores           []*ir.InstStore
  PHIs             []*ir.InstPhi
  Rdx              *ReductionInfo
  RdxLen           int
  GEPs             []*ir.InstGetElementPtr
  Gammas           []*Gamma
  Cmps             []*ir.InstICmp
  CP               *ConditionPack
  IsGatherScatter  bool
  OperandPacks     []*OperandPack
  OrderedValues    []value.Value
  Cost             float64
  ProducingCost    float64
  VPCtx            *VectorPackContext
}

func (vp *VectorPack) ComputeOperandPacksForGeneral() []*OperandPack {
  sig := vp.Producer.GetSignature()
  numInputs := sig.NumInputs()
  laneOps := vp.Producer.GetLaneOps()
  numLanes := len(laneOps)
  operandPacks := make([]*OperandPack, numInputs)

  for i := 0; i < numInputs; i++ {
    var inputValues []BoundInput
    var elementSize int
    for j := 0; j < numLanes; j++ {
      boundSlices := laneOps[j].GetBoundSlices()
      for k, bs := range boundSlices {
        if bs.InputId == i {
          elementSize = bs.Size()
          var v value.Value
          if vp.Matches[j] != nil {
            v = vp.Matches[j].Inputs[k]
          }
          inputValues = append(inputValues, BoundInput{Slice: bs, V: v})
        }
      }
    }
    if elementSize == 0 {
      panic("elementSize cannot be 0")
    }

    sort.Slice(inputValues, func(i, j int) bool {
      return inputValues[i].Slice.Lo < inputValues[j].Slice.Lo
    })

    curOffset := 0
    stride := inputValues[0].Slice.Size()
    op := NewOperandPack()
    for _, bv := range inputValues {
      for curOffset < bv.Slice.Lo {
        op.Push(nil)
        curOffset += stride
      }
      if curOffset != bv.Slice.Lo {
        panic("curOffset must equal bv.Slice.Lo")
      }
      op.Push(bv.V)
      curOffset += stride
    }
    inputSize := sig.InputBitwidths[i]
    for curOffset < inputSize {
      op.Push(nil)
      curOffset += stride
    }
    if op.Size()*stride != inputSize {
      panic("op.Size()*stride must equal inputSize")
    }

    if op.Front() == nil && op.IsSplat() {
      elementType := types.NewInt(elementSize)
      op.Ty = types.NewVector(elementType, op.Size())
    }
    operandPacks[i] = op
  }

  return operandPacks
}