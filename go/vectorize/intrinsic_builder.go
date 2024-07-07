package hyper

import (
  "fmt"
  "github.com/llir/llvm/ir"
  "github.com/llir/llvm/ir/constant"
  "github.com/llir/llvm/ir/types"
  "github.com/llir/llvm/ir/value"
)

type IntrinsicBuilder struct {
  InstWrappers *ir.Module
  Builder      *ir.Builder
}

func (ib *IntrinsicBuilder) Create(name string, operands []value.Value, imm8 uint8) (value.Value, error) {
  wrapperName := fmt.Sprintf("intrinsic_wrapper_%s_%d", name, imm8)
  f := ib.InstWrappers.Func(wrapperName)
  if f == nil {
    return nil, fmt.Errorf("intrinsic wrapper undefined")
  }
  if len(f.Blocks) != 1 {
    return nil, fmt.Errorf("intrinsic wrapper should have a single basic block")
  }
  bb := f.Blocks[0]
  if len(operands) != len(f.Params) {
    return nil, fmt.Errorf("incorrect number of operands")
  }

  vMap := make(map[value.Value]value.Value)
  for i, arg := range f.Params {
    if !constant.IsValidCast(constant.BitCast, operands[i], arg.Type()) {
      return nil, fmt.Errorf("invalid input type")
    }
    operand := ib.Builder.CreateBitCast(operands[i], arg.Type(), "")
    vMap[arg] = operand
  }

  var retVal value.Value
  for _, inst := range bb.Insts {
    if ret, ok := inst.(*ir.InstRet); ok {
      retVal = ret.X
      break
    }
    newI := inst.Clone()
    ib.Builder.Insert(newI)
    vMap[inst] = newI
    remapInstruction(newI, vMap)
    if call, ok := newI.(*ir.InstCall); ok {
      callee := call.Callee
      if f, ok := callee.(*ir.Func); ok && f.IsIntrinsic() {
        m := ib.Builder.GetInsertBlock().Parent.Module
        intrinsicDecl := m.NewFunc(f.Name(), f.Sig)
        call.Callee = intrinsicDecl
      }
    }
  }

  if retVal == nil {
    return nil, fmt.Errorf("wrapper not returning explicitly")
  }
  output, ok := vMap[retVal]
  if !ok {
    return nil, fmt.Errorf("return value not found in value map")
  }
  return output, nil
}

func remapInstruction(inst ir.Instruction, vMap map[value.Value]value.Value) {}