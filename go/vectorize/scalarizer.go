package hyper

import (
    "fmt"
    "log"

    "github.com/llir/llvm/ir"
    "github.com/llir/llvm/ir/constant"
    "github.com/llir/llvm/ir/enum"
    "github.com/llir/llvm/ir/types"
    "github.com/llir/llvm/ir/value"
)

var (
    scalarizeVariableInsertExtract = true
    scalarizeLoadStore             = false
)

type valueVector []value.Value
type scatterMap map[value.Value]valueVector
type gatherList []struct {
    instr value.Value
    vec   valueVector
}

type loopUnrollResult int

const (
    unmodified loopUnrollResult = iota
    partiallyUnrolled
    fullyUnrolled
)

type unrolledValue struct {
    iteration int
    original  value.Value
}

type vectorLayout struct {
    vecTy    types.Type
    elemTy   types.Type
    vecAlign int64
    elemSize int64
}

type scalarizer struct {
    scattered              scatterMap
    gathered               gatherList
    potentiallyDeadInstrs  []value.Value
    parallelLoopAccessMDKind string
    context                *ir.Context
    mod                    *ir.Module
    currentFunc            *ir.Func
    currentBlock           *ir.Block
}

func newScalarizer(mod *ir.Module) *scalarizer {
    return &scalarizer{
        scattered:              make(scatterMap),
        gathered:               make(gatherList, 0),
        potentiallyDeadInstrs:  make([]value.Value, 0),
        parallelLoopAccessMDKind: "llvm.mem.parallel_loop_access",
        context:                mod.Context(),
        mod:                    mod,
    }
}

func (s *scalarizer) skipPastPhiNodesAndDbg(iter *ir.Inst) *ir.Inst {
    for iter != nil {
        switch inst := iter.(type) {
        case *ir.InstPhi:
            iter = inst.Parent().Insts[inst.Index()+1]
        case *ir.InstCall:
            if callee, ok := inst.Callee().(*ir.Func); ok && callee.Name() == "llvm.dbg.value" {
                iter = inst.Parent().Insts[inst.Index()+1]
            } else {
                return iter
            }
        default:
            return iter
        }
    }
    return nil
}

func (s *scalarizer) scatter(point *ir.Block, v value.Value) valueVector {
    scatterHelper := func(bb *ir.Block, v value.Value) valueVector {
        ty := v.Type()
        var size int
        switch t := ty.(type) {
        case *types.VectorType:
            size = t.Len
        case *types.PointerType:
            if vt, ok := t.ElemType.(*types.VectorType); ok {
                size = vt.Len
            } else {
                log.Fatal("Unexpected type in scatter_helper")
            }
        default:
            log.Fatal("Unexpected type in scatter_helper")
        }

        cache, exists := s.scattered[v]
        if !exists {
            cache = make(valueVector, size)
        }

        for i := 0; i < size; i++ {
            if cache[i] == nil {
                var newVal value.Value
                switch t := ty.(type) {
                case *types.PointerType:
                    if i == 0 {
                        newPtrTy := types.NewPointer(t.ElemType.(*types.VectorType).ElemType)
                        newVal = s.context.NewBitCast(v, newPtrTy, fmt.Sprintf("%s.i0", v.Name()))
                    } else {
                        gep := s.context.NewGetElementPtr(t.ElemType, v, constant.NewInt(types.I32, int64(i)))
                        newVal = s.context.NewLoad(t.ElemType.(*types.VectorType).ElemType, gep)
                    }
                default:
                    newVal = s.context.NewExtractElement(v, constant.NewInt(types.I32, int64(i)))
                }
                cache[i] = newVal
            }
        }
        s.scattered[v] = cache
        return cache
    }

    switch v := v.(type) {
    case *ir.Arg:
        return scatterHelper(s.currentFunc.Blocks[0], v)
    case value.Named:
        if inst, ok := v.(ir.Instruction); ok {
            bb := inst.Parent()
            iter := s.skipPastPhiNodesAndDbg(bb.Insts[inst.Index()+1])
            return scatterHelper(bb, v)
        }
    }
    return scatterHelper(point, v)
}

func (s *scalarizer) gather(op value.Value, cv valueVector) {
    s.transferMetadataAndIRFlags(op, cv)

    if sv, exists := s.scattered[op]; exists {
        for i, v := range cv {
            if sv[i] != nil && sv[i] != v {
                sv[i].ReplaceAllUsesWith(v)
                s.potentiallyDeadInstrs = append(s.potentiallyDeadInstrs, sv[i])
            }
        }
    }
    s.scattered[op] = cv
    s.gathered = append(s.gathered, struct {
        instr value.Value
        vec   valueVector
    }{op, cv})
}

func (s *scalarizer) canTransferMetadata(tag string) bool {
    return tag == "tbaa" ||
        tag == "fpmath" ||
        tag == "tbaa.struct" ||
        tag == "invariant.load" ||
        tag == "alias.scope" ||
        tag == "noalias" ||
        tag == s.parallelLoopAccessMDKind ||
        tag == "access_group"
}

func (s *scalarizer) transferMetadataAndIRFlags(op value.Value, cv valueVector) {
    if inst, ok := op.(ir.Instruction); ok {
        for _, v := range cv {
            if newInst, ok := v.(ir.Instruction); ok {
                for _, md := range inst.Metadata() {
                    if s.canTransferMetadata(md.Name()) {
                        newInst.SetMetadata(md.Name(), md.Value())
                    }
                }
                // Note: Go LLVM bindings might not have direct equivalents for copyFastMathFlags
                // You may need to implement these separately or use lower-level LLVM API calls
                if inst.DebugLoc() != nil && newInst.DebugLoc() == nil {
                    newInst.SetDebugLoc(inst.DebugLoc())
                }
            }
        }
    }
}

func (s *scalarizer) getVectorLayout(ty types.Type, alignment int64) *vectorLayout {
    if vecTy, ok := ty.(*types.VectorType); ok {
        elemTy := vecTy.ElemType
        if s.mod.DataLayout().TypeAllocSize(elemTy) == s.mod.DataLayout().TypeStoreSize(elemTy) {
            return &vectorLayout{
                vecTy:    ty,
                elemTy:   elemTy,
                vecAlign: alignment,
                elemSize: s.mod.DataLayout().TypeStoreSize(elemTy),
            }
        }
    }
    return nil
}

func (s *scalarizer) visitSelectInst(si *ir.InstSelect) bool {
    if vecTy, ok := si.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        vop1 := s.scatter(si.Parent(), si.X())
        vop2 := s.scatter(si.Parent(), si.Y())
        res := make(valueVector, numElems)

        if _, ok := si.Cond().Type().(*types.VectorType); ok {
            vop0 := s.scatter(si.Parent(), si.Cond())
            for i := 0; i < numElems; i++ {
                res[i] = s.context.NewSelect(vop0[i], vop1[i], vop2[i])
            }
        } else {
            for i := 0; i < numElems; i++ {
                res[i] = s.context.NewSelect(si.Cond(), vop1[i], vop2[i])
            }
        }
        s.gather(si, res)
        return true
    }
    return false
}

func (s *scalarizer) visitCmpInst(ci value.Value) bool {
    var pred enum.IPred
    var fpred enum.FPred
    var isFloatCmp bool

    switch cmp := ci.(type) {
    case *ir.InstICmp:
        pred = cmp.Pred()
    case *ir.InstFCmp:
        fpred = cmp.Pred()
        isFloatCmp = true
    default:
        return false
    }

    if vecTy, ok := ci.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        vop0 := s.scatter(ci.Parent().(*ir.Block), ci.Operands()[0])
        vop1 := s.scatter(ci.Parent().(*ir.Block), ci.Operands()[1])
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            if isFloatCmp {
                res[i] = s.context.NewFCmp(fpred, vop0[i], vop1[i])
            } else {
                res[i] = s.context.NewICmp(pred, vop0[i], vop1[i])
            }
        }
        s.gather(ci, res)
        return true
    }
    return false
}

func (s *scalarizer) visitBinaryOperator(bo value.Value) bool {
    var opcode enum.BinaryOp
    switch inst := bo.(type) {
    case *ir.InstAdd:
        opcode = enum.BinaryOpAdd
    case *ir.InstFAdd:
        opcode = enum.BinaryOpFAdd
    case *ir.InstSub:
        opcode = enum.BinaryOpSub
    case *ir.InstFSub:
        opcode = enum.BinaryOpFSub
    case *ir.InstMul:
        opcode = enum.BinaryOpMul
    case *ir.InstFMul:
        opcode = enum.BinaryOpFMul
    case *ir.InstUDiv:
        opcode = enum.BinaryOpUDiv
    case *ir.InstSDiv:
        opcode = enum.BinaryOpSDiv
    case *ir.InstFDiv:
        opcode = enum.BinaryOpFDiv
    case *ir.InstURem:
        opcode = enum.BinaryOpURem
    case *ir.InstSRem:
        opcode = enum.BinaryOpSRem
    case *ir.InstFRem:
        opcode = enum.BinaryOpFRem
    default:
        return false
    }

    if vecTy, ok := bo.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        vop0 := s.scatter(bo.Parent().(*ir.Block), bo.Operands()[0])
        vop1 := s.scatter(bo.Parent().(*ir.Block), bo.Operands()[1])
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            res[i] = s.context.NewBinOp(opcode, vop0[i], vop1[i])
        }
        s.gather(bo, res)
        return true
    }
    return false
}

func (s *scalarizer) visitGetElementPtrInst(gepi *ir.InstGetElementPtr) bool {
    if vecTy, ok := gepi.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        vbase := s.scatter(gepi.Parent(), gepi.Src())
        vindices := make([]valueVector, len(gepi.Indices()))
        for i, idx := range gepi.Indices() {
            vindices[i] = s.scatter(gepi.Parent(), idx)
        }
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            indices := make([]value.Value, len(vindices))
            for j, vindex := range vindices {
                indices[j] = vindex[i]
            }
            res[i] = s.context.NewGetElementPtr(gepi.ElemType, vbase[i], indices...)
        }
        s.gather(gepi, res)
        return true
    }
    return false
}

func (s *scalarizer) visitCastInst(ci value.Value) bool {
    var opcode enum.InstOpcode
    switch inst := ci.(type) {
    case *ir.InstTrunc:
        opcode = enum.InstOpcodeTrunc
    case *ir.InstZExt:
        opcode = enum.InstOpcodeZExt
    case *ir.InstSExt:
        opcode = enum.InstOpcodeSExt
    case *ir.InstFPToUI:
        opcode = enum.InstOpcodeFPToUI
    case *ir.InstFPToSI:
        opcode = enum.InstOpcodeFPToSI
    case *ir.InstUIToFP:
        opcode = enum.InstOpcodeUIToFP
    case *ir.InstSIToFP:
        opcode = enum.InstOpcodeSIToFP
    case *ir.InstFPTrunc:
        opcode = enum.InstOpcodeFPTrunc
    case *ir.InstFPExt:
        opcode = enum.InstOpcodeFPExt
    case *ir.InstPtrToInt:
        opcode = enum.InstOpcodePtrToInt
    case *ir.InstIntToPtr:
        opcode = enum.InstOpcodeIntToPtr
    case *ir.InstBitCast:
        opcode = enum.InstOpcodeBitCast
    case *ir.InstAddrSpaceCast:
        opcode = enum.InstOpcodeAddrSpaceCast
    default:
        return false
    }

    if vecTy, ok := ci.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        vop := s.scatter(ci.Parent().(*ir.Block), ci.Operands()[0])
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            res[i] = s.context.NewCast(opcode, vop[i], vecTy.ElemType)
        }
        s.gather(ci, res)
        return true
    }
    return false
}

func (s *scalarizer) visitLoadInst(li *ir.InstLoad) bool {
    if !scalarizeLoadStore || li.Volatile {
        return false
    }

    if layout := s.getVectorLayout(li.Type(), li.Align); layout != nil {
        numElems := layout.vecTy.(*types.VectorType).Len
        vptr := s.scatter(li.Parent(), li.Src())
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            res[i] = s.context.NewLoad(layout.elemTy, vptr[i])
            if load, ok := res[i].(*ir.InstLoad); ok {
                load.Align = li.Align
            }
        }
        s.gather(li, res)
        return true
    }
    return false
}

func (s *scalarizer) visitStoreInst(si *ir.InstStore) bool {
    if !scalarizeLoadStore || si.Volatile {
        return false
    }

    if layout := s.getVectorLayout(si.Src().Type(), si.Align); layout != nil {
        numElems := layout.vecTy.(*types.VectorType).Len
        vptr := s.scatter(si.Parent(), si.Dst())
        vval := s.scatter(si.Parent(), si.Src())

        for i := 0; i < numElems; i++ {
            store := s.context.NewStore(vval[i], vptr[i])
            store.Align = si.Align
            s.currentBlock.AppendInst(store)
        }
        return true
    }
    return false
}

func (s *scalarizer) visitCallInst(ci *ir.InstCall) bool {
    if vecTy, ok := ci.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        vargs := make([]valueVector, len(ci.Args()))
        for i, arg := range ci.Args() {
            vargs[i] = s.scatter(ci.Parent(), arg)
        }
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            args := make([]value.Value, len(vargs))
            for j, varg := range vargs {
                args[j] = varg[i]
            }
            res[i] = s.context.NewCall(ci.Callee(), args...)
        }
        s.gather(ci, res)
        return true
    }
    return false
}

func (s *scalarizer) visitPHINode(phi *ir.InstPhi) bool {
    if vecTy, ok := phi.Type().(*types.VectorType); ok {
        numElems := vecTy.Len
        res := make(valueVector, numElems)

        for i := 0; i < numElems; i++ {
            newPhi := s.context.NewPhi(vecTy.ElemType)
            for _, inc := range phi.Incs {
                vop := s.scatter(inc.Pred, inc.X)
                newPhi.Incs = append(newPhi.Incs, &ir.Incoming{
                    X:    vop[i],
                    Pred: inc.Pred,
                })
            }
            res[i] = newPhi
        }
        s.gather(phi, res)
        return true
    }
    return false
}

func (s *scalarizer) visitInstruction(inst ir.Instruction) bool {
    switch inst := inst.(type) {
    case *ir.InstSelect:
        return s.visitSelectInst(inst)
    case *ir.InstICmp, *ir.InstFCmp:
        return s.visitCmpInst(inst)
    case *ir.InstAdd, *ir.InstFAdd, *ir.InstSub, *ir.InstFSub, *ir.InstMul, *ir.InstFMul,
         *ir.InstUDiv, *ir.InstSDiv, *ir.InstFDiv, *ir.InstURem, *ir.InstSRem, *ir.InstFRem:
        return s.visitBinaryOperator(inst)
    case *ir.InstGetElementPtr:
        return s.visitGetElementPtrInst(inst)
    case *ir.InstTrunc, *ir.InstZExt, *ir.InstSExt, *ir.InstFPToUI, *ir.InstFPToSI,
         *ir.InstUIToFP, *ir.InstSIToFP, *ir.InstFPTrunc, *ir.InstFPExt,
         *ir.InstPtrToInt, *ir.InstIntToPtr, *ir.InstBitCast, *ir.InstAddrSpaceCast:
        return s.visitCastInst(inst)
    case *ir.InstLoad:
        return s.visitLoadInst(inst)
    case *ir.InstStore:
        return s.visitStoreInst(inst)
    case *ir.InstCall:
        return s.visitCallInst(inst)
    case *ir.InstPhi:
        return s.visitPHINode(inst)
    }
    return false
}

func (s *scalarizer) visitBasicBlock(bb *ir.Block) bool {
    changed := false
    s.currentBlock = bb
    for _, inst := range bb.Insts {
        if s.visitInstruction(inst) {
            changed = true
        }
    }
    return changed
}

func (s *scalarizer) visitFunction(f *ir.Func) bool {
    changed := false
    s.currentFunc = f
    for _, bb := range f.Blocks {
        if s.visitBasicBlock(bb) {
            changed = true
        }
    }
    return changed || s.finish()
}

func (s *scalarizer) finish() bool {
    if len(s.gathered) == 0 && len(s.scattered) == 0 {
        return false
    }

    for _, gatherItem := range s.gathered {
        op := gatherItem.instr
        cv := gatherItem.vec

        if !isUndef(op) {
            ty := op.Type()
            var res value.Value

            switch ty := ty.(type) {
            case *types.VectorType:
                count := ty.Len
                res = constant.NewUndef(ty)
                for i := 0; i < count; i++ {
                    res = s.context.NewInsertElement(res, cv[i], constant.NewInt(types.I32, int64(i)))
                }
            default:
                if len(cv) != 1 || ty != cv[0].Type() {
                    log.Fatal("Unexpected gathered result")
                }
                res = cv[0]
            }

            if op != res {
                op.ReplaceAllUsesWith(res)
                s.potentiallyDeadInstrs = append(s.potentiallyDeadInstrs, op)
            }
        }
    }

    for _, inst := range s.potentiallyDeadInstrs {
        if isConstant(inst) || len(inst.(*ir.Instruction).Uses()) == 0 {
            inst.(*ir.Instruction).Parent().RemoveInst(inst.(*ir.Instruction))
        }
    }

    s.gathered = nil
    s.scattered = make(scatterMap)
    s.potentiallyDeadInstrs = nil

    return true
}

func (s *scalarizer) run() bool {
    changed := false
    for _, f := range s.mod.Funcs {
        if s.visitFunction(f) {
            changed = true
        }
    }
    return changed
}