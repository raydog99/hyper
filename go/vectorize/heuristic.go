package hyper

import (
  "math"

  "github.com/llvm/llvm-project/llvm"
)

const (
  cSplat   = 1.0
  cPerm    = 2.0
  cInsert  = 2.0
  cShuffle = 2.0
  cExtract = 1.0
)

var (
  allowDeinterleave = false
  allowTranspose    = false
)

type Solution struct {
  Cost  float64
  Packs []*vectorpack.VectorPack
}

type Heuristic struct {
  solutions    map[*vectorpack.OperandPack]Solution
  scalarCosts  map[llvm.Value]float64
  pkr          *packer.Packer
}

func NewHeuristic(pkr *packer.Packer) *Heuristic {
  return &Heuristic{
    solutions:    make(map[*vectorpack.OperandPack]Solution),
    scalarCosts:  make(map[llvm.Value]float64),
    pkr:          pkr,
  }
}

func (h *Heuristic) GetCost(vp *vectorpack.VectorPack) float64 {
  cost := vp.GetProducingCost()
  for _, op := range vp.GetOperandPacks() {
    if allCmpInst(op) {
      continue
    }
    cost += h.GetCost(op)
  }
  return cost
}

func allCmpInst(op *vectorpack.OperandPack) bool {
  for _, v := range op.ToSlice() {
    if _, ok := v.(*llvm.CmpInst); !ok {
      return false
    }
  }
  return true
}

func deinterleave(vpCtx *vectorpack.VectorPackContext, op *vectorpack.OperandPack, stride uint) []*vectorpack.OperandPack {
  return nil
}

func transpose(vpCtx *vectorpack.VectorPackContext, op *vectorpack.OperandPack, n uint) *vectorpack.OperandPack {
  if op.Size()%n != 0 {
    return nil
  }
  m := op.Size() / n
  t := make([]llvm.Value, op.Size())
  for i := uint(0); i < m; i++ {
    for j := uint(0); j < n; j++ {
      t[i*n+j] = op.Get(j*m + i)
    }
  }
  return vpCtx.GetCanonicalOperandPack(t)
}

func (h *Heuristic) Solve(op *vectorpack.OperandPack) Solution {
  if sol, ok := h.solutions[op]; ok {
    return sol
  }

  h.solutions[op] = Solution{Cost: 0}

  cost := 0.0
  inserted := make(map[llvm.Value]bool)
  for i, v := range op.ToSlice() {
    if _, ok := v.(*llvm.Constant); !ok && !inserted[v] {
      inserted[v] = true
      cost += h.GetCostValue(v) + cInsert
    }
  }

  sol := Solution{Cost: cost}
  if cost == 0 {
    h.solutions[op] = sol
    return sol
  }

  broadcastCost := h.GetCostValue(op.Front()) + cSplat
  if op.IsSplat() && cost > broadcastCost {
    return Solution{Cost: broadcastCost}
  }

  vpCtx := h.pkr.GetContext()
  deduped := vpCtx.Dedup(op)
  extraCost := 0.0
  if deduped != op {
    extraCost = cShuffle
  }

  opi := h.pkr.GetProducerInfo(deduped)
  for _, vp := range opi.GetProducers() {
    newSol := Solution{Cost: h.GetCost(vp) + extraCost, Packs: []*vectorpack.VectorPack{vp}}
    if newSol.Cost < sol.Cost {
      sol = newSol
    }
  }

  if allowTranspose {
    for _, n := range []uint{2, 4, 8} {
      if t := transpose(vpCtx, op, n); t != nil {
        opi := h.pkr.GetProducerInfo(t)
        for _, vp := range opi.GetProducers() {
          newSol := Solution{Cost: h.GetCost(vp) + cPerm, Packs: []*vectorpack.VectorPack{vp}}
          if newSol.Cost < sol.Cost {
            sol = newSol
          }
        }
      }
    }
  }

  if allowDeinterleave {
    for _, stride := range []uint{2, 4, 8} {
      if deduped.Size()%stride == 0 {
        ops := deinterleave(vpCtx, deduped, stride)
        cost := cShuffle * float64(len(ops))
        var packs []*vectorpack.VectorPack
        for _, op2 := range ops {
          sol2 := h.Solve(op2)
          packs = append(packs, sol2.Packs...)
          cost += sol2.Cost
          if cost > sol.Cost {
            break
          }
        }
        if cost < sol.Cost {
          sol = Solution{Cost: cost, Packs: packs}
        }
      }
    }
  }

  h.solutions[op] = sol
  return sol
}

func (h *Heuristic) GetCostValue(v llvm.Value) float64 {
  if i, ok := v.(*llvm.Instruction); !ok {
    return 0
  } else {
    if cost, ok := h.scalarCosts[i]; ok {
      return cost
    }
    h.scalarCosts[i] = 0
    cost := h.pkr.GetScalarCost(i)
    for _, op := range i.Operands() {
      cost += h.GetCostValue(op)
    }
    h.scalarCosts[i] = cost
    return cost
  }
}