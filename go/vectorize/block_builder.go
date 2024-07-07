package hyper

import (
	"fmt"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

type ControlCondition interface{}
type ConditionAnd struct {
	Parent   ControlCondition
	Cond     value.Value
	IsTrue   bool
	Complement ControlCondition
}
type ConditionOr struct {
	Conds               []ControlCondition
	GreatestCommonCond  ControlCondition
}

type BlockBuilder struct {
	ctx           *types.Context
	f             *ir.Func
	emitCondition func(value.Value) value.Value
	activeConds   map[ControlCondition]*ir.Block
	semiActiveConds map[ControlCondition][]ControlCondition
	dummyCounter  int
}

func NewBlockBuilder(entryBB *ir.Block, emitCondition func(value.Value) value.Value) *BlockBuilder {
	return &BlockBuilder{
		ctx:           entryBB.Parent.Parent.TypeDefs,
		f:             entryBB.Parent,
		emitCondition: emitCondition,
		activeConds:   map[ControlCondition]*ir.Block{nil: entryBB},
		semiActiveConds: make(map[ControlCondition][]ControlCondition),
		dummyCounter:  0,
	}
}

func (bb *BlockBuilder) CreateBlock() *ir.Block {
	return bb.f.NewBlock("")
}

type conditionEmitter struct {
	builder       *ir.Block
	common        ControlCondition
	emitCondition func(value.Value) value.Value
	emitted       map[ControlCondition]value.Value
}

func newConditionEmitter(bb *ir.Block, common ControlCondition, emitCondition func(value.Value) value.Value) *conditionEmitter {
	return &conditionEmitter{
		builder:       bb,
		common:        common,
		emitCondition: emitCondition,
		emitted:       make(map[ControlCondition]value.Value),
	}
}

func (ce *conditionEmitter) emit(c ControlCondition) value.Value {
	if c == nil || c == ce.common {
		return constant.NewInt(types.I1, 1)
	}
	switch cond := c.(type) {
	case *ConditionAnd:
		return ce.emitAnd(cond)
	case *ConditionOr:
		return ce.emitOr(cond)
	default:
		panic("Unknown condition type")
	}
}

func (ce *conditionEmitter) emitAnd(and *ConditionAnd) value.Value {
	if v, ok := ce.emitted[and]; ok {
		return v
	}
	parent := ce.emit(and.Parent)
	cond := ce.emitCondition(and.Cond)
	var v value.Value
	if and.IsTrue {
		v = ce.builder.NewAnd(parent, cond)
	} else {
		notCond := ce.builder.NewNot(cond)
		v = ce.builder.NewAnd(parent, notCond)
	}
	ce.emitted[and] = v
	return v
}

func (ce *conditionEmitter) emitOr(or *ConditionOr) value.Value {
	if v, ok := ce.emitted[or]; ok {
		return v
	}
	v := ce.emitDisjunction(or.Conds)
	ce.emitted[or] = v
	return v
}

func (ce *conditionEmitter) emitDisjunction(conds []ControlCondition) value.Value {
	values := make([]value.Value, len(conds))
	for i, c := range conds {
		values[i] = ce.emit(c)
	}
	return ce.builder.NewOr(values...)
}

func (bb *BlockBuilder) GetBlockFor(c ControlCondition) *ir.Block {
	if block, ok := bb.activeConds[c]; ok {
		return block
	}

	if _, ok := bb.semiActiveConds[c]; ok {
		conds := bb.getActiveConds(c)
		newBB := bb.CreateBlock()
		for _, c2 := range conds {
			oldBB := bb.activeConds[c2]
			oldBB.NewBr(newBB)
			delete(bb.activeConds, c2)
		}
		bb.activeConds[c] = newBB
		return newBB
	}

	switch cond := c.(type) {
	case *ConditionAnd:
		return bb.handleConditionAnd(cond)
	case *ConditionOr:
		return bb.handleConditionOr(cond)
	default:
		panic("Unknown condition type")
	}
}

func (bb *BlockBuilder) handleConditionAnd(and *ConditionAnd) *ir.Block {
	ifTrue := bb.CreateBlock()
	ifFalse := bb.CreateBlock()
	parentBB := bb.GetBlockFor(and.Parent)
	cond := bb.emitCondition(and.Cond)
	parentBB.NewCondBr(cond, ifTrue, ifFalse)
	resultBB := ifTrue
	if !and.IsTrue {
		resultBB = ifFalse
	}
	delete(bb.activeConds, and.Parent)
	bb.semiActiveConds[and.Parent] = []ControlCondition{and, and.Complement}
	bb.activeConds[and] = resultBB
	bb.activeConds[and.Complement] = ifFalse
	if and.IsTrue {
		bb.activeConds[and.Complement] = ifTrue
	}
	return resultBB
}

func (bb *BlockBuilder) handleConditionOr(or *ConditionOr) *ir.Block {
	commonC := or.GreatestCommonCond
	var conds []ControlCondition
	if _, ok := bb.semiActiveConds[commonC]; !ok {
		bb.GetBlockFor(commonC)
		conds = []ControlCondition{commonC}
	} else {
		conds = bb.getActiveConds(commonC)
	}

	newBB := bb.CreateBlock()
	auxBB := bb.CreateBlock()
	condsToJoin := make(map[ControlCondition]bool)
	for _, c := range or.Conds {
		condsToJoin[c] = true
	}
	joined := make(map[ControlCondition]bool)

	for _, c2 := range conds {
		oldBB := bb.activeConds[c2]
		if condsToJoin[c2] {
			oldBB.NewBr(newBB)
			joined[c2] = true
		} else {
			oldBB.NewBr(auxBB)
		}
		delete(bb.activeConds, c2)
	}

	var unjoinedConds []ControlCondition
	for _, c := range or.Conds {
		if !joined[c] {
			unjoinedConds = append(unjoinedConds, c)
		}
	}

	drainBB := bb.CreateBlock()
	if len(unjoinedConds) == 0 {
		auxBB.NewBr(drainBB)
	} else {
		ce := newConditionEmitter(auxBB, commonC, bb.emitCondition)
		cond := ce.emitDisjunction(unjoinedConds)
		auxBB.NewCondBr(cond, newBB, drainBB)
	}

	dummyC := bb.GetDummyCondition()
	bb.activeConds[or] = newBB
	bb.activeConds[dummyC] = drainBB
	bb.semiActiveConds[commonC] = []ControlCondition{or, dummyC}
	delete(bb.activeConds, commonC)
	return newBB
}

func (bb *BlockBuilder) getActiveConds(c ControlCondition) []ControlCondition {
	visited := make(map[ControlCondition]bool)
	var conds []ControlCondition

	var dfs func(ControlCondition)
	dfs = func(c2 ControlCondition) {
		if visited[c2] {
			return
		}
		visited[c2] = true
		if _, ok := bb.activeConds[c2]; ok {
			conds = append(conds, c2)
			return
		}
		if children, ok := bb.semiActiveConds[c2]; ok {
			for _, child := range children {
				dfs(child)
			}
		}
	}

	dfs(c)
	return conds
}

func (bb *BlockBuilder) GetDummyCondition() ControlCondition {
	bb.dummyCounter++
	return fmt.Sprintf("dummy_%d", bb.dummyCounter)
}

func (bb *BlockBuilder) SetBlockForCondition(block *ir.Block, c ControlCondition) {
	if _, ok := bb.activeConds[c]; !ok {
		panic("Can only set block for active condition")
	}
	bb.activeConds[c] = block
}