package hyper

import (
	"flag"
	"fmt"
	"log"

	"github.com/llir/llvm"
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type Loop struct {
	Header *ir.Block
	Latch  *ir.Block
	Blocks []*ir.Block
	Exits  []*ir.Block
}

type UnrollLoopOptions struct {
	Count                    int
	TripCount                int
	TripMultiple             int
	PeelCount                int
	AllowRuntime             bool
	AllowExpensiveTripCount  bool
	Force                    bool
	UnrollRemainder          bool
	ForgetAllSCEV            bool
}

type LoopUnrollResult int

const (
	Unmodified LoopUnrollResult = iota
	PartiallyUnrolled
	FullyUnrolled
)

type UnrolledValue struct {
	Iteration int
	Original  value.Value
}

var unrollRuntimeEpilog bool

func needToInsertPhisForLCSSA(l *Loop, blocks []*ir.Block, li *LoopInfo) bool {
	for _, bb := range blocks {
		if li.GetLoopFor(bb) == l {
			continue
		}
		for _, inst := range bb.Insts {
			for _, op := range inst.Operands() {
				if defInst, ok := op.(*ir.Instruction); ok {
					defLoop := li.GetLoopFor(defInst.Parent)
					if defLoop != nil && defLoop.Contains(l) {
						return true
					}
				}
			}
		}
	}
	return false
}

func simplifyLoopAfterUnroll2(l *Loop, simplifyIVs bool, li *LoopInfo, se *ScalarEvolution, dt *DominatorTree, ac *AssumptionCache, tti *TargetTransformInfo) {
	if se != nil && simplifyIVs {
		deadInsts := make([]*ir.Instruction, 0)
		simplifyLoopIVs(l, se, dt, li, tti, &deadInsts)
		for _, inst := range deadInsts {
			inst.Parent.Insts.Remove(inst)
		}
	}

	dl := l.Header.Parent.Module.DataLayout
	for _, bb := range l.Blocks {
		for i := 0; i < len(bb.Insts); i++ {
			inst := bb.Insts[i]
			if v := simplifyInstruction(inst, dl, nil, dt, ac); v != nil {
				if li.ReplacementPreservesLCSSAForm(inst, v) {
					inst.ReplaceAllUsesWith(v)
				}
			}
			if isInstructionTriviallyDead(inst) {
				bb.Insts.Remove(inst)
				i--
			}
		}
	}
}

func isEpilogProfitable(l *Loop) bool {
	preheader := l.Header.Parent.Blocks[len(l.Header.Parent.Blocks)-1]
	for _, phi := range l.Header.Insts {
		if p, ok := phi.(*ir.InstPhi); ok {
			for _, inc := range p.Incs {
				if inc.Pred == preheader {
					if _, ok := inc.X.(*constant.Int); ok {
						return true
					}
				}
			}
		}
	}
	return false
}

func unrollLoopWithVMap(l *Loop, ulo UnrollLoopOptions, li *LoopInfo, se *ScalarEvolution, dt *DominatorTree, ac *AssumptionCache, tti *TargetTransformInfo, preserveLCSSA bool, unrollToOrigMap map[value.Value]UnrolledValue, remainderLoop **Loop) LoopUnrollResult {
	if l.GetLoopPreheader() == nil {
		return Unmodified
	}

	if l.GetLoopLatch() == nil {
		return Unmodified
	}

	if !l.IsSafeToClone() {
		return Unmodified
	}

	if l.Header.HasAddressTaken() {
		return Unmodified
	}

	if ulo.TripCount != 0 && ulo.Count > ulo.TripCount {
		ulo.Count = ulo.TripCount
	}

	if ulo.TripCount == 0 && ulo.Count < 2 && ulo.PeelCount == 0 {
		return Unmodified
	}

	completelyUnroll := ulo.Count == ulo.TripCount
	runtimeTripCount := ulo.TripCount == 0 && ulo.Count > 0 && ulo.AllowRuntime

	peeled := false
	if ulo.PeelCount > 0 {
		peeled = peelLoop(l, ulo.PeelCount, li, se, dt, ac, preserveLCSSA)
		if peeled {
			exitingBlock := l.GetLoopLatch()
			ulo.TripCount = se.GetSmallConstantTripCount(l, exitingBlock)
			ulo.TripMultiple = se.GetSmallConstantTripMultiple(l, exitingBlock)
		}
	}

	origLoopBlocks := l.Blocks

	if se != nil {
		if ulo.ForgetAllSCEV {
			se.ForgetAllLoops()
		} else {
			se.ForgetTopmostLoop(l)
		}
	}

	var breakoutTrip int
	if ulo.TripCount != 0 {
		breakoutTrip = ulo.TripCount % ulo.Count
		ulo.TripMultiple = 0
	} else {
		breakoutTrip = ulo.TripMultiple
		ulo.TripMultiple = gcd(ulo.Count, ulo.TripMultiple)
	}

	lastValueMap := make(map[value.Value]value.Value)
	var origPHINode []*ir.InstPhi
	for _, inst := range l.Header.Insts {
		if phi, ok := inst.(*ir.InstPhi); ok {
			origPHINode = append(origPHINode, phi)
		}
	}

	var headers, latches []*ir.Block
	headers = append(headers, l.Header)
	latches = append(latches, l.GetLoopLatch())

	unrolledLoopBlocks := l.Blocks

	loopsToSimplify := make(map[*Loop]struct{})
	for _, subLoop := range l.SubLoops {
		loopsToSimplify[subLoop] = struct{}{}
	}

	for it := 1; it != ulo.Count; it++ {
		newBlocks := make([]*ir.Block, 0)
		newLoops := make(map[*Loop]*Loop)
		newLoops[l] = l

		for _, bb := range origLoopBlocks {
			newBB := cloneBasicBlock(bb, fmt.Sprintf(".%d", it))
			l.Header.Parent.Blocks = append(l.Header.Parent.Blocks, newBB)

			unrollToOrigMap[newBB] = UnrolledValue{Iteration: it, Original: bb}

			oldLoop := addClonedBlockToLoopInfo(bb, newBB, li, newLoops)
			if oldLoop != nil {
				loopsToSimplify[newLoops[oldLoop]] = struct{}{}
			}

			if bb == l.Header {
				for _, origPHI := range origPHINode {
					newPHI := newBB.Insts[0].(*ir.InstPhi)
					inVal := newPHI.Incs[len(newPHI.Incs)-1].X
					if inValI, ok := inVal.(*ir.Instruction); ok {
						if it > 1 && l.Contains(inValI.Parent) {
							inVal = lastValueMap[inValI]
						}
					}
					lastValueMap[origPHI] = inVal
					newBB.Insts = newBB.Insts[1:]
				}
			}

			for i, inst := range bb.Insts {
				lastValueMap[inst] = newBB.Insts[i]
				unrollToOrigMap[newBB.Insts[i]] = UnrolledValue{Iteration: it, Original: inst}
			}

			for _, succ := range bb.Term.Succs() {
				if l.Contains(succ) {
					continue
				}
				for _, phi := range succ.Insts {
					if p, ok := phi.(*ir.InstPhi); ok {
						incoming := p.Incoming(bb)
						if newIncoming, exists := lastValueMap[incoming]; exists {
							incoming = newIncoming
						}
						p.Incs = append(p.Incs, &ir.Incoming{Pred: newBB, X: incoming})
					}
				}
			}

			if bb == l.Header {
				headers = append(headers, newBB)
			}
			if bb == l.GetLoopLatch() {
				latches = append(latches, newBB)
			}

			newBlocks = append(newBlocks, newBB)
			unrolledLoopBlocks = append(unrolledLoopBlocks, newBB)

			if dt != nil {
				if bb == l.Header {
					dt.AddNewBlock(newBB, latches[it-1])
				} else {
					bbDomNode := dt.GetNode(bb)
					bbIDom := bbDomNode.IDom()
					originalBBIDom := bbIDom.Block()
					dt.AddNewBlock(newBB, lastValueMap[originalBBIDom].(*ir.Block))
				}
			}
		}

		remapInstructions(newBlocks, lastValueMap)
		for _, newBlock := range newBlocks {
			for _, inst := range newBlock.Insts {
				if ii, ok := inst.(*ir.InstCall); ok {
					if ii.Callee == l.Header.Parent.Module.NamedFuncs["llvm.assume"] {
						ac.RegisterAssumption(ii)
					}
				}
			}
		}
	}

	for _, pn := range origPHINode {
		if completelyUnroll {
			pn.ReplaceAllUsesWith(pn.Incoming(l.GetLoopPreheader()))
			l.Header.Insts.Remove(pn)
		} else if ulo.Count > 1 {
			inVal := pn.RemoveIncoming(l.GetLoopLatch())
			if inValI, ok := inVal.(*ir.Instruction); ok {
				if l.Contains(inValI.Parent) {
					inVal = lastValueMap[inVal]
				}
			}
			pn.Incs = append(pn.Incs, &ir.Incoming{Pred: latches[len(latches)-1], X: inVal})
		}
	}

	for i, latch := range latches {
		dest := headers[(i+1)%len(headers)]
		term := latch.Term.(*ir.TermBr)
		if term.Cond != nil {
			term.SuccTrue = dest
		} else {
			newBr := ir.NewBr(dest)
			latch.Term.ReplaceAllUsesWith(newBr)
			latch.Term = newBr
		}
	}

	simplifyLoopAfterUnroll2(l, !completelyUnroll && (ulo.Count > 1 || peeled), li, se, dt, ac, tti)

	outerL := l.Parent
	if completelyUnroll {
		li.Erase(l)
	}

	if preserveLCSSA && outerL != nil && completelyUnroll {
		needToFixLCSSA := needToInsertPhisForLCSSA(outerL, unrolledLoopBlocks, li)
		if needToFixLCSSA {
			latchLoop := li.GetLoopFor(latches[len(latches)-1])
			fixLCSSALoop := outerL
			for fixLCSSALoop.Parent != latchLoop {
				fixLCSSALoop = fixLCSSALoop.Parent
			}
			formLCSSARecursively(fixLCSSALoop, dt, li, se)
		}
	}

	if dt != nil {
		if outerL != nil {
			simplifyLoop(outerL, dt, li, se, ac, nil, preserveLCSSA)
		} else {
			for loop := range loopsToSimplify {
				simplifyLoop(loop, dt, li, se, ac, nil, preserveLCSSA)
			}
		}
	}

	if completelyUnroll {
		return FullyUnrolled
	}
	return PartiallyUnrolled
}