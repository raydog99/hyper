package hyper

import (
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/value"
	"github.com/llvm-mirror/llvm/bindings/go/llvm"
)

type LoopInfo interface {
	GetLoopFor(bb *ir.Block) *Loop
	Contains(l *Loop, bb *ir.Block) bool
	GetHeader(l *Loop) *ir.Block
	IsOutermostLoop(l *Loop) bool
	HasNestedLoops(l *Loop) bool
	GetTopLevelLoops() []*Loop
}

type Loop struct {
	Header llvm.BasicBlock
}

func (l *Loop) GetHeader() llvm.BasicBlock {
	return l.Header
}

func ComputeRPO(f *ir.Function, li LoopInfo, rpo *[]*ir.Block) {
	worklist := []*ir.Block{f.Blocks[0]}
	visited := make(map[*ir.Block]bool)

	for len(worklist) > 0 {
		bb := worklist[len(worklist)-1]
		worklist = worklist[:len(worklist)-1]

		if !visited[bb] {
			visited[bb] = true
			for _, succ := range bb.Succs {
				if li.GetLoopFor(succ) != nil {
					worklist = append(worklist, succ)
				} else {
					worklist = append(worklist, succ)
				}
			}
			*rpo = append([]*ir.Block{bb}, *rpo...)
		}
	}
}

func ComputeLoopRPO(li LoopInfo, l *Loop, rpo *[]*ir.Block) {
	header := li.GetHeader(l)
	worklist := []*ir.Block{header}
	visited := make(map[*ir.Block]bool)

	for len(worklist) > 0 {
		bb := worklist[len(worklist)-1]
		worklist = worklist[:len(worklist)-1]

		if !visited[bb] {
			visited[bb] = true
			for _, succ := range bb.Succs {
				if li.Contains(l, succ) {
					if succ != header {
						worklist = append(worklist, succ)
					}
				} else {
					worklist = append(worklist, succ)
				}
			}
			*rpo = append([]*ir.Block{bb}, *rpo...)
		}
	}
}

func ComputeNestedLoopRPO(li LoopInfo, l *Loop, rpo *[]*ir.Block) {
	var subLoops []*Loop
	header := li.GetHeader(l)
	worklist := []*ir.Block{header}
	visited := make(map[*ir.Block]bool)

	for len(worklist) > 0 {
		bb := worklist[len(worklist)-1]
		worklist = worklist[:len(worklist)-1]

		if !visited[bb] {
			visited[bb] = true
			for _, succ := range bb.Succs {
				if li.Contains(l, succ) {
					succLoop := li.GetLoopFor(succ)
					if succLoop != nil && li.GetLoopFor(li.GetHeader(succLoop)) == l {
						subLoops = append(subLoops, succLoop)
					} else if succ != header {
						worklist = append(worklist, succ)
					}
				} else {
					worklist = append(worklist, succ)
				}
			}
			*rpo = append([]*ir.Block{bb}, *rpo...)
		}
	}

	for _, subLoop := range subLoops {
		ComputeLoopRPO(li, subLoop, rpo)
	}
}

func LARPORecurse(li LoopInfo, l *Loop, rpo *[]*ir.Block) {
	if li.IsOutermostLoop(l) {
		ComputeRPO(li.GetHeader(l).Parent(), li, rpo)
	} else if li.HasNestedLoops(l) {
		ComputeNestedLoopRPO(li, l, rpo)
	} else {
		ComputeLoopRPO(li, l, rpo)
	}
}

func ComputeLARPO(f *ir.Function, li LoopInfo) []*ir.Block {
	var rpo []*ir.Block
	ComputeRPO(f, li, &rpo)
	for _, l := range li.GetTopLevelLoops() {
		LARPORecurse(li, l, &rpo)
	}
	return rpo
}