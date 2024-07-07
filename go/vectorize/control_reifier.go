package hyper

import (
  "github.com/llir/llvm/ir"
  "github.com/llir/llvm/ir/constant"
  "github.com/llir/llvm/ir/types"
  "github.com/llir/llvm/ir/value"
)

type ControlReifier struct {
  Ctx            *types.Context
  ReifiedValues  map[ControlConditionVLoopPair]value.Value
  InsertedInsts  []*ir.Instruction
}

type ControlConditionVLoopPair struct {
  C  *ControlCondition
  VL *VLoop
}

func NewControlReifier(ctx *types.Context) *ControlReifier {
  return &ControlReifier{
    Ctx:           ctx,
    ReifiedValues: make(map[ControlConditionVLoopPair]value.Value),
    InsertedInsts: []*ir.Instruction{},
  }
}

func (cr *ControlReifier) Reify(c *ControlCondition, vl *VLoop) value.Value {
  if c == nil {
    return constant.NewInt(types.I1, 1)
  }
  
  pair := ControlConditionVLoopPair{c, vl}
  if v, ok := cr.ReifiedValues[pair]; ok {
    return v
  }
  
  var reified value.Value
  
  switch cond := c.(type) {
  case *ConditionAnd:
    cr.Reify(cond.Parent, vl)
    condVal := cond.Cond
    if !cond.IsTrue {
      notInst := ir.NewNot(condVal)
      cr.InsertedInsts = append(cr.InsertedInsts, notInst)
      vl.AddInstruction(notInst, cond.Parent)
      condVal = notInst
    }
    reified = vl.CreateOneHotPhi(cond.Parent, condVal, constant.NewInt(types.I1, 0), "reified.onehot")
  
  case *ConditionOr:
    reified = cr.Reify(cond.Conds[0], vl)
    for _, c2 := range cond.Conds[1:] {
      tmp := ir.NewOr(reified, cr.Reify(c2, vl))
      cr.InsertedInsts = append(cr.InsertedInsts, tmp)
      vl.AddInstruction(tmp, nil)
      reified = tmp
    }
  }
  
  cr.ReifiedValues[pair] = reified
  
  if and, ok := c.(*ConditionAnd); ok {
    cr.Reify(and.Complement, vl)
  }
  
  return reified
}

func (cr *ControlReifier) HasValue(c *ControlCondition, vl *VLoop) bool {
  if c == nil {
    return true
  }
  _, ok := cr.ReifiedValues[ControlConditionVLoopPair{c, vl}]
  return ok
}

func (cr *ControlReifier) GetValue(c *ControlCondition, vl *VLoop) value.Value {
  if !cr.HasValue(c, vl) {
    panic("Value not found")
  }
  if c == nil {
    return constant.NewInt(types.I1, 1)
  }
  return cr.ReifiedValues[ControlConditionVLoopPair{c, vl}]
}