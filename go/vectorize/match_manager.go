package hyper

import (
  "sort"

  "github.com/llir/llvm/ir"
  "github.com/llir/llvm/ir/value"
)

type MatchManager struct {
  OpMatches map[*Operation][]Operation.Match
}

func sortByOutput(a, b Operation.Match) bool {
  return a.Output.Ident() < b.Output.Ident()
}

func (mm *MatchManager) matchValue(v value.Value) {
  for op, matches := range mm.OpMatches {
    op.Match(v, &matches)
    mm.OpMatches[op] = matches
  }
}

func Create(insts []*InstBinding, f *ir.Func) *MatchManager {
  mm := &MatchManager{OpMatches: make(map[*Operation][]Operation.Match)}
  for _, inst := range insts {
    for _, laneOp := range inst.GetLaneOps() {
      op := laneOp.GetOperation()
      if _, exists := mm.OpMatches[op]; !exists {
        mm.OpMatches[op] = []Operation.Match{}
      }
    }
  }
  for _, bb := range f.Blocks {
    for _, inst := range bb.Insts {
      mm.matchValue(inst)
    }
  }
  for _, matches := range mm.OpMatches {
    sort.Slice(matches, func(i, j int) bool {
      return sortByOutput(matches[i], matches[j])
    })
  }
  return mm
}

func CreateWithInstructions(insts []*InstBinding, toMatch []ir.Instruction) *MatchManager {
  mm := &MatchManager{OpMatches: make(map[*Operation][]Operation.Match)}
  for _, inst := range insts {
    for _, laneOp := range inst.GetLaneOps() {
      op := laneOp.GetOperation()
      if _, exists := mm.OpMatches[op]; !exists {
        mm.OpMatches[op] = []Operation.Match{}
      }
    }
  }
  for _, inst := range toMatch {
    mm.matchValue(inst)
  }
  for _, matches := range mm.OpMatches {
    sort.Slice(matches, func(i, j int) bool {
      return sortByOutput(matches[i], matches[j])
    })
  }
  return mm
}

func (mm *MatchManager) GetMatches(op *Operation) []Operation.Match {
  return mm.OpMatches[op]
}

func (mm *MatchManager) GetMatchesForOutput(op *Operation, output value.Value) []Operation.Match {
  matches := mm.GetMatches(op)
  dummyMatch := Operation.Match{LiveIn: false, Operands: nil, Output: output}
  lowerBound := sort.Search(len(matches), func(i int) bool {
    return !sortByOutput(matches[i], dummyMatch)
  })
  upperBound := sort.Search(len(matches), func(i int) bool {
    return sortByOutput(dummyMatch, matches[i])
  })
  return matches[lowerBound:upperBound]
}