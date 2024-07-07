package hyper

import (
  "llvm.org/llvm/bindings/go/llvm"
  "math/rand"
)

func getLoopNest(li llvm.LoopInfo, v llvm.Value) []llvm.Loop {
  if !v.IsAInstruction().IsNil() {
    i := v.IsAInstruction()
    var loopNest []llvm.Loop
    for l := li.GetLoopFor(i.InstructionParent()); !l.IsNil(); l = l.GetParentLoop() {
      loopNest = append(loopNest, l)
    }
    for i, j := 0, len(loopNest)-1; i < j; i, j = i+1, j-1 {
      loopNest[i], loopNest[j] = loopNest[j], loopNest[i]
    }
    return loopNest
  }
  return nil
}

func getAddressSpaceOperand(i llvm.Value) int {
  if !i.IsALoadInst().IsNil() {
    return i.IsALoadInst().GetPointerAddressSpace()
  }
  if !i.IsAStoreInst().IsNil() {
    return i.IsAStoreInst().GetPointerAddressSpace()
  }
  return -1
}

type AddRecLoopRewriter struct {
  SE      llvm.ScalarEvolution
  Loops   map[llvm.Loop]llvm.Loop
  Success bool
}

func (r *AddRecLoopRewriter) Rewrite(expr llvm.SCEV) llvm.SCEV {
  if !expr.IsAScevAddRecExpr().IsNil() {
    ar := expr.IsAScevAddRecExpr()
    oldLoop := ar.GetLoop()
    newLoop, ok := r.Loops[oldLoop]
    if !ok {
      newLoop = oldLoop
    }
    var operands []llvm.SCEV
    for i := 0; i < ar.GetNumOperands(); i++ {
      operands = append(operands, r.Rewrite(ar.OperandAt(i)))
    }
    for _, op := range operands {
      if !r.SE.IsAvailableAtLoopEntry(op, newLoop) {
        r.Success = false
        return expr
      }
    }
    return r.SE.GetAddRecExpr(operands, newLoop, ar.GetNoWrapFlags())
  }
  return expr
}

func isEquivalent(ptrA, ptrB llvm.Value, se llvm.ScalarEvolution, li llvm.LoopInfo) bool {
  numEquivChecks++
  if se.GetSCEV(ptrA).Eq(se.GetSCEV(ptrB)) {
    return true
  }
  if ptrA.IsAInstruction().IsNil() || ptrB.IsAInstruction().IsNil() {
    return false
  }
  if ptrA.Type() != ptrB.Type() {
    return false
  }
  loopNest1 := getLoopNest(li, ptrA)
  loopNest2 := getLoopNest(li, ptrB)
  if len(loopNest1) != len(loopNest2) {
    return false
  }
  loops := make(map[llvm.Loop]llvm.Loop)
  for i := range loopNest1 {
    if !haveIdenticalTripCounts(loopNest1[i], loopNest2[i], se) {
      return false
    }
    loops[loopNest2[i]] = loopNest1[i]
  }
  ptrSCEVA := se.GetSCEV(ptrA)
  rewriter := &AddRecLoopRewriter{SE: se, Loops: loops, Success: true}
  ptrSCEVB := rewriter.Rewrite(se.GetSCEV(ptrB))
  return ptrSCEVA.Eq(ptrSCEVB)
}

func isConsecutive(a, b llvm.Value, dl llvm.TargetData, se llvm.ScalarEvolution, li llvm.LoopInfo) bool {
  numConsecChecks++
  ptrA := getLoadStorePointerOperand(a)
  ptrB := getLoadStorePointerOperand(b)
  if ptrA.IsNil() || ptrB.IsNil() {
    return false
  }
  asA := getAddressSpaceOperand(a)
  asB := getAddressSpaceOperand(b)
  loopNest1 := getLoopNest(li, ptrA)
  loopNest2 := getLoopNest(li, ptrB)
  if len(loopNest1) != len(loopNest2) {
    return false
  }
  loops := make(map[llvm.Loop]llvm.Loop)
  for i := range loopNest1 {
    if loopNest1[i] != loopNest2[i] {
      if !haveIdenticalTripCounts(loopNest1[i], loopNest2[i], se) {
        return false
      }
      loops[loopNest2[i]] = loopNest1[i]
    }
  }
  if asA != asB || ptrA.Type() != ptrB.Type() || ptrA == ptrB {
    return false
  }
  idxWidth := dl.IndexSizeInBits(asA)
  ty := ptrA.Type().ElementType()
  offsetA, strippedPtrA := stripAndAccumulateInBoundsConstantOffsets(dl, ptrA)
  offsetB, strippedPtrB := stripAndAccumulateInBoundsConstantOffsets(dl, ptrB)
  asA = strippedPtrA.Type().PointerAddressSpace()
  asB = strippedPtrB.Type().PointerAddressSpace()
  if asA != asB {
    return false
  }
  idxWidth = dl.IndexSizeInBits(asA)
  offsetA = llvm.ConstIntSExtOrTrunc(offsetA, llvm.IntType(idxWidth))
  offsetB = llvm.ConstIntSExtOrTrunc(offsetB, llvm.IntType(idxWidth))
  size := llvm.ConstInt(llvm.IntType(idxWidth), uint64(dl.TypeStoreSize(ty)), false)
  offsetSCEVA := se.GetConstant(offsetA)
  offsetSCEVB := se.GetConstant(offsetB)
  offsetDeltaSCEV := se.GetMinusSCEV(offsetSCEVB, offsetSCEVA)
  offsetDelta := offsetDeltaSCEV.SCEVConstant().ZExtValue()
  if strippedPtrA == strippedPtrB {
    return offsetDelta == size.ZExtValue()
  }
  sizeSCEV := se.GetConstant(size)
  baseDelta := se.GetMinusSCEV(sizeSCEV, offsetDeltaSCEV)
  ptrSCEVA := se.GetSCEV(strippedPtrA)
  ptrSCEVB := se.GetSCEV(strippedPtrB)
  if len(loops) > 0 {
    rewriter := &AddRecLoopRewriter{SE: se, Loops: loops, Success: true}
    ptrSCEVB = rewriter.Rewrite(ptrSCEVB)
  }
  x := se.GetAddExpr([]llvm.SCEV{ptrSCEVA, baseDelta})
  return x.Eq(ptrSCEVB)
}

func findConsecutiveAccesses(se llvm.ScalarEvolution, dl llvm.TargetData, li llvm.LoopInfo, accesses []llvm.Value, equivalentAccesses *EquivalenceClasses, numFingerprints int) [][2]llvm.Value {
  if len(accesses) == 0 {
    return nil
  }
  fingerprintsToAccesses := make(map[int64][]llvm.Value)
  accessToFingerprints := make(map[llvm.Value][]int64)
  ptr := getLoadStorePointerOperand(accesses[0])
  ty := ptr.Type().ElementType()
  size := dl.TypeStoreSize(ty)
  var consecutiveAccesses [][2]llvm.Value
  sg := NewSizeGenerator()
  igs := make([]*IterationGenerator, numFingerprints)
  for i := range igs {
    igs[i] = NewIterationGenerator(int64(i))
  }
  for _, i := range accesses {
    ptr := getLoadStorePointerOperand(i)
    ptrSCEV := se.GetSCEV(ptr)
    fingerprints := fingerprintSCEV(se, ptrSCEV, sg, igs, numFingerprints)
    left := fingerprints[0] - int64(size)
    for _, leftI := range fingerprintsToAccesses[left] {
      leftFingerprints := accessToFingerprints[leftI]
      allMatch := true
      for j := range leftFingerprints {
        if leftFingerprints[j]+int64(size) != fingerprints[j] {
          allMatch = false
          break
        }
      }
      if allMatch && isConsecutive(leftI, i, dl, se, li) {
        consecutiveAccesses = append(consecutiveAccesses, [2]llvm.Value{leftI, i})
      }
    }
    for _, i2 := range fingerprintsToAccesses[fingerprints[0]] {
      fingerprints2 := accessToFingerprints[i2]
      allMatch := true
      for j := range fingerprints2 {
        if fingerprints2[j] != fingerprints[j] {
          allMatch = false
          break
        }
      }
      if allMatch && isEquivalent(getLoadStorePointerOperand(i), getLoadStorePointerOperand(i2), se, li) {
        equivalentAccesses.UnionSets(i, i2)
        break
      }
    }
    right := fingerprints[0] + int64(size)
    for _, rightI := range fingerprintsToAccesses[right] {
      rightFingerprints := accessToFingerprints[rightI]
      allMatch := true
      for j := range rightFingerprints {
        if rightFingerprints[j] != fingerprints[j]+int64(size) {
          allMatch = false
          break
        }
      }
      if allMatch && isConsecutive(i, rightI, dl, se, li) {
        consecutiveAccesses = append(consecutiveAccesses, [2]llvm.Value{i, rightI})
      }
    }
    fingerprintsToAccesses[fingerprints[0]] = append(fingerprintsToAccesses[fingerprints[0]], i)
    accessToFingerprints[i] = fingerprints
  }
  return consecutiveAccesses
}

type SizeGenerator struct {
  unknownToSize map[llvm.SCEV]int64
}

func NewSizeGenerator() *SizeGenerator {
  return &SizeGenerator{unknownToSize: make(map[llvm.SCEV]int64)}
}

func (sg *SizeGenerator) GetSize(expr llvm.SCEV) int64 {
  i := int64(len(sg.unknownToSize))
  size, exists := sg.unknownToSize[expr]
  if !exists {
    size = (2 << i) + i
    sg.unknownToSize[expr] = size
  }
  return size
}

type IterationGenerator struct {
  iterations map[llvm.Loop]int64
  offset     int64
}

func NewIterationGenerator(offset int64) *IterationGenerator {
  return &IterationGenerator{
    iterations: make(map[llvm.Loop]int64),
    offset:     offset,
  }
}

func (ig *IterationGenerator) GetIteration(l llvm.Loop) int64 {
  i := int64(len(ig.iterations))
  iteration, exists := ig.iterations[l]
  if !exists {
    iteration = rand.Int63n(32)
    ig.iterations[l] = iteration
  }
  return iteration
}

func fingerprintSCEV(se llvm.ScalarEvolution, expr llvm.SCEV, sg *SizeGenerator, igs []*IterationGenerator, n int) []int64 {
  fingerprints := make([]int64, n)
  for i := 0; i < n; i++ {
    fingerprinter := &SCEVFingerprinter{SE: se, SG: sg, IG: igs[i]}
    result := fingerprinter.Visit(expr)
    fingerprints[i] = result.SCEVConstant().SExtValue()
  }
  return fingerprints
}

type SCEVFingerprinter struct {
  SE llvm.ScalarEvolution
  SG *SizeGenerator
  IG *IterationGenerator
}

func (fp *SCEVFingerprinter) Visit(expr llvm.SCEV) llvm.SCEV {
  switch {
  case !expr.IsAScevAddRecExpr().IsNil():
    return fp.VisitAddRecExpr(expr.IsAScevAddRecExpr())
  case !expr.IsAScevUnknown().IsNil():
    return fp.VisitUnknown(expr.IsAScevUnknown())
  default:
    return expr
  }
}

func (fp *SCEVFingerprinter) VisitAddRecExpr(expr llvm.ScevAddRecExpr) llvm.SCEV {
  var operands []llvm.SCEV
  for i := 0; i < expr.GetNumOperands(); i++ {
    operands = append(operands, fp.Visit(expr.OperandAt(i)))
  }
  l := expr.GetLoop()
  x := fp.SE.GetAddRecExpr(operands, l, expr.GetNoWrapFlags())
  rec, ok := x.IsAScevAddRecExpr()
  if !ok {
    return x
  }
  return rec.EvaluateAtIteration(llvm.ConstInt(llvm.Int64Type(), uint64(fp.IG.GetIteration(l)), false), fp.SE)
}

func (fp *SCEVFingerprinter) VisitUnknown(expr llvm.ScevUnknown) llvm.SCEV {
  return llvm.ConstInt(expr.Type(), uint64(fp.SG.GetSize(expr)), false)
}