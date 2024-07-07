package hyper

import (
	"fmt"
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

func isFloat(opcode enum.BinaryOp) bool {
	switch opcode {
	case enum.FAdd, enum.FSub, enum.FMul, enum.FDiv, enum.FRem:
		return true
	default:
		return false
	}
}

type BinaryIROperation struct {
	Opcode   enum.BinaryOp
	Bitwidth int
}

func (op *BinaryIROperation) Match(v value.Value, matches *[]Match) bool {
	if binOp, ok := v.(*ir.InstBinOp); ok {
		if binOp.Op == op.Opcode && hasBitWidth(v, op.Bitwidth) {
			*matches = append(*matches, Match{
				LiveIn:   false,
				Operands: []value.Value{binOp.X, binOp.Y},
				Result:   v,
			})
			return true
		}
	}
	return false
}

func (op *BinaryIROperation) GetMaximumVF(tti *TargetTransformInfo) int {
	return tti.GetLoadStoreVecRegBitWidth(0) / op.Bitwidth
}

func (op *BinaryIROperation) GetName() string {
	return fmt.Sprintf("%s-i%d", op.Opcode, op.Bitwidth)
}

type UnaryIROperation struct {
	Opcode   enum.UnaryOp
	Bitwidth int
}

func (op *UnaryIROperation) Match(v value.Value, matches *[]Match) bool {
	if unaryOp, ok := v.(*ir.InstUnaryOp); ok {
		if unaryOp.Op == op.Opcode && hasBitWidth(v, op.Bitwidth) {
			*matches = append(*matches, Match{
				LiveIn:   false,
				Operands: []value.Value{unaryOp.X},
				Result:   v,
			})
			return true
		}
	}
	return false
}

func (op *UnaryIROperation) GetMaximumVF(tti *TargetTransformInfo) int {
	return tti.GetLoadStoreVecRegBitWidth(0) / op.Bitwidth
}

func (op *UnaryIROperation) GetName() string {
	return fmt.Sprintf("%s-%d", op.Opcode, op.Bitwidth)
}

type IRVectorBinding struct {
	Op      *BinaryIROperation
	Name    string
	Sig     InstSignature
	LaneOps []BoundOperation
}

func (b *IRVectorBinding) GetCost(tti *TargetTransformInfo, ctx *types.Context) float64 {
	var scalarTy types.Type
	if isFloat(b.Op.Opcode) {
		if b.Op.Bitwidth == 32 {
			scalarTy = types.Float
		} else {
			scalarTy = types.Double
		}
	} else {
		scalarTy = types.NewInt(b.Op.Bitwidth)
	}
	numElems := len(b.LaneOps)
	vecTy := types.NewVector(scalarTy, int64(numElems))
	return tti.GetArithmeticInstrCost(b.Op.Opcode, vecTy)
}

func CreateIRVectorBinding(op *BinaryIROperation, vectorWidth int) *IRVectorBinding {
	sig := InstSignature{
		InputWidths:  []int{vectorWidth, vectorWidth},
		OutputWidths: []int{vectorWidth},
		HasImm8:      false,
	}
	elemWidth := op.Bitwidth
	numLanes := vectorWidth / elemWidth
	laneOps := make([]BoundOperation, numLanes)
	for i := 0; i < numLanes; i++ {
		lo := i * elemWidth
		hi := lo + elemWidth
		laneOps[i] = BoundOperation{
			Op: op,
			InputBindings: []InputBinding{
				{0, lo, hi},
				{1, lo, hi},
			},
		}
	}
	return &IRVectorBinding{
		Op:      op,
		Name:    op.GetName(),
		Sig:     sig,
		LaneOps: laneOps,
	}
}

func (b *IRVectorBinding) Emit(operands []value.Value, builder *ir.Builder) (value.Value, error) {
	if len(operands) != 2 {
		return nil, fmt.Errorf("invalid number of operands")
	}
	return builder.NewBinOp(b.Op.Opcode, operands[0], operands[1], "")
}

func (b *IRVectorBinding) IsSupported(tti *TargetTransformInfo) bool {
	return len(b.LaneOps) <= b.Op.GetMaximumVF(tti)
}

type UnaryIRVectorBinding struct {
	Op      *UnaryIROperation
	Name    string
	Sig     InstSignature
	LaneOps []BoundOperation
}

func (b *UnaryIRVectorBinding) GetCost(tti *TargetTransformInfo, ctx *types.Context) float64 {
	elemWidth := b.Op.Bitwidth
	numElems := len(b.LaneOps)
	if b.Op.Opcode == enum.FNeg {
		var ty types.Type
		if elemWidth == 32 {
			ty = types.Float
		} else {
			ty = types.Double
		}
		vecTy := types.NewVector(ty, int64(numElems))
		return tti.GetArithmeticInstrCost(enum.FNeg, vecTy)
	}
	return 1.0
}

func CreateUnaryIRVectorBinding(op *UnaryIROperation, vectorWidth int) *UnaryIRVectorBinding {
	sig := InstSignature{
		InputWidths:  []int{vectorWidth},
		OutputWidths: []int{vectorWidth},
		HasImm8:      false,
	}
	elemWidth := op.Bitwidth
	numLanes := vectorWidth / elemWidth
	laneOps := make([]BoundOperation, numLanes)
	for i := 0; i < numLanes; i++ {
		lo := i * elemWidth
		hi := lo + elemWidth
		laneOps[i] = BoundOperation{
			Op: op,
			InputBindings: []InputBinding{
				{0, lo, hi},
			},
		}
	}
	return &UnaryIRVectorBinding{
		Op:      op,
		Name:    op.GetName(),
		Sig:     sig,
		LaneOps: laneOps,
	}
}

func (b *UnaryIRVectorBinding) Emit(operands []value.Value, builder *ir.Builder) (value.Value, error) {
	if len(operands) != 1 {
		return nil, fmt.Errorf("invalid number of operands")
	}
	ctx := builder.GetInsertBlock().Parent.Parent.Context()
	elemWidth := b.Op.Bitwidth
	numElems := len(b.LaneOps)
	floatTy := types.Float
	if elemWidth == 64 {
		floatTy = types.Double
	}
	intTy := types.NewInt(elemWidth)
	vecFloatTy := types.NewVector(floatTy, int64(numElems))
	vecIntTy := types.NewVector(intTy, int64(numElems))
	v := operands[0]
	switch b.Op.Opcode {
	case enum.SIToFP:
		return builder.NewSIToFP(tryCast(builder, v, vecIntTy), vecFloatTy)
	case enum.FPToSI:
		return builder.NewFPToSI(tryCast(builder, v, vecFloatTy), vecIntTy)
	case enum.FNeg:
		return builder.NewFNeg(tryCast(builder, v, vecFloatTy))
	default:
		return nil, fmt.Errorf("unsupported unary opcode")
	}
}

func (b *UnaryIRVectorBinding) IsSupported(tti *TargetTransformInfo) bool {
	return len(b.LaneOps) <= b.Op.GetMaximumVF(tti)
}

func tryCast(builder *ir.Builder, v value.Value, ty types.Type) value.Value {
	if v.Type() == ty {
		return v
	}
	return builder.NewBitCast(v, ty)
}