package hyper

import (
	"llvm.org/llvm/bindings/go/llvm"
	"sort"
)

func isLessThan(se llvm.ScalarEvolution, a, b llvm.SCEV) bool {
	return se.IsKnownNegative(se.GetMinusSCEV(a, b))
}

func refineWithRange(se llvm.ScalarEvolution, expr llvm.SCEV, cr llvm.ConstantRange) llvm.SCEV {
	smin := llvm.APIntGetSignedMinValue(cr.GetBitWidth())
	umin := llvm.APIntGetMinValue(cr.GetBitWidth())
	smax := llvm.APIntGetSignedMaxValue(cr.GetBitWidth())
	umax := llvm.APIntGetMaxValue(cr.GetBitWidth())

	if !cr.GetSignedMin().Eq(smax) && llvm.ConstantRange{cr.GetSignedMin(), smax}.Contains(cr) {
		expr = se.GetSMaxExpr(expr, se.GetConstant(cr.GetSignedMin()))
	}
	if !cr.GetUnsignedMin().Eq(umax) && llvm.ConstantRange{cr.GetUnsignedMin(), umax}.Contains(cr) {
		expr = se.GetUMaxExpr(expr, se.GetConstant(cr.GetUnsignedMin()))
	}
	if !cr.GetUpper().Eq(smin) && llvm.ConstantRange{smin, cr.GetUpper()}.Contains(cr) {
		expr = se.GetSMinExpr(expr, se.GetConstant(cr.GetSignedMax()))
	}
	if !umin.Eq(cr.GetUpper()) && llvm.ConstantRange{umin, cr.GetUpper()}.Contains(cr) {
		expr = se.GetUMinExpr(expr, se.GetConstant(cr.GetUnsignedMax()))
	}
	return expr
}

func refineWithRanges(se llvm.ScalarEvolution, expr llvm.SCEV, ranges map[llvm.Value]llvm.ConstantRange) llvm.SCEV {
	valueToSCEV := make(map[llvm.Value]llvm.SCEV)
	for v, r := range ranges {
		valueToSCEV[v] = refineWithRange(se, se.GetSCEV(v), r)
	}
	return llvm.SCEVParameterRewriter_Rewrite(expr, se, valueToSCEV)
}

type UnknownSCEVCollector struct {
	SE     llvm.ScalarEvolution
	Values map[llvm.Value]struct{}
}

func (c *UnknownSCEVCollector) VisitUnknown(expr llvm.SCEVUnknown) llvm.SCEV {
	if expr.GetType().IsIntegerTy() {
		c.Values[expr.GetValue()] = struct{}{}
	}
	return expr
}

func getLocation(i llvm.Value) llvm.MemoryLocation {
	switch i.InstructionOpcode() {
	case llvm.Store:
		return llvm.GetStore(i)
	case llvm.Load:
		return llvm.GetLoad(i)
	default:
		return llvm.MemoryLocation{}
	}
}

func isSimple(i llvm.Value) bool {
	switch i.InstructionOpcode() {
	case llvm.Load:
		return llvm.GetLoad(i).IsSimple()
	case llvm.Store:
		return llvm.GetStore(i).IsSimple()
	case llvm.Call:
		return !llvm.GetCallSiteAttributes(i).HasAttribute(llvm.AttributeVolatile)
	default:
		return true
	}
}

func getLoopForPointer(li llvm.LoopInfo, ptr llvm.Value) llvm.Loop {
	if i, ok := ptr.(*llvm.Instruction); ok {
		return li.GetLoopFor(i.Parent())
	}
	return llvm.Loop{}
}

func getBaseValue(s llvm.SCEV) llvm.Value {
	switch s.SCEVType() {
	case llvm.SCEVAddRecExpr:
		return getBaseValue(s.SCEVAddRecExprStart())
	case llvm.SCEVAddExpr:
		last := s.SCEVAddExprGetOperand(s.SCEVAddExprGetNumOperands() - 1)
		if last.GetType().IsPointerTy() {
			return getBaseValue(last)
		}
	case llvm.SCEVUnknown:
		return s.SCEVUnknownGetValue()
	}
	return llvm.Value{}
}

func isAliased(i1, i2 llvm.Value, aa llvm.AliasAnalysis, se llvm.ScalarEvolution, li llvm.LoopInfo, lvi llvm.LazyValueInfo) bool {
	loc1 := getLocation(i1)
	loc2 := getLocation(i2)
	f := i1.InstructionParent().Parent()

	if loc1.Ptr.IsNil() || loc2.Ptr.IsNil() || !isSimple(i1) || !isSimple(i2) {
		return true
	}

	result := aa.Alias(loc1, loc2)
	if result != llvm.MayAlias {
		return result == llvm.MustAlias
	}

	ptr1 := llvm.GetLoadStorePointerOperand(i1)
	ptr2 := llvm.GetLoadStorePointerOperand(i2)
	if ptr1.IsNil() || ptr2.IsNil() {
		return true
	}

	ptr1SCEV := se.GetSCEV(ptr1)
	ptr2SCEV := se.GetSCEV(ptr2)

	base1 := getBaseValue(ptr1SCEV)
	base2 := getBaseValue(ptr2SCEV)
	if !base1.IsNil() && !base2.IsNil() && base1 != base2 {
		return aa.Alias(llvm.MemoryLocationGetBeforeOrAfter(base1), llvm.MemoryLocationGetBeforeOrAfter(base2)) == llvm.MustAlias
	}

	var loops []llvm.Loop
	collectLoops := func(s llvm.SCEV) bool {
		if s.SCEVType() == llvm.SCEVAddRecExpr {
			loops = append(loops, s.SCEVAddRecExprGetLoop())
		}
		return true
	}
	llvm.SCEVTraversal_VisitAll(collectLoops, ptr1SCEV)
	llvm.SCEVTraversal_VisitAll(collectLoops, ptr2SCEV)

	compareLoops := func(l1, l2 llvm.Loop) bool {
		return l1 == l2 || l1.Contains(l2)
	}
	sort.Slice(loops, func(i, j int) bool {
		return compareLoops(loops[i], loops[j])
	})
	for i := 0; i < len(loops)-1; i++ {
		if !compareLoops(loops[i], loops[i+1]) {
			return true
		}
	}

	lt := isLessThan(se, ptr1SCEV, ptr2SCEV)
	gt := isLessThan(se, ptr2SCEV, ptr1SCEV)
	if !lt && !gt {
		return true
	}
	if gt {
		ptr1SCEV, ptr2SCEV = ptr2SCEV, ptr1SCEV
	}

	ty := ptr1.Type().ElementType()
	as := ptr1.Type().PointerAddressSpace()
	dl := f.GlobalParent().DataLayout()
	indexWidth := dl.IndexSizeInBits(as)
	size := llvm.ConstInt(llvm.IntType(indexWidth), uint64(dl.TypeStoreSize(ty)), false)
	return se.IsKnownPositive(se.GetMinusSCEV(se.GetAddExpr([]llvm.SCEV{ptr1SCEV, se.GetConstant(size)}), ptr2SCEV))
}

type GlobalDependenceAnalysis struct {
	AA              llvm.AliasAnalysis
	SE              llvm.ScalarEvolution
	LI              llvm.LoopInfo
	LVI             llvm.LazyValueInfo
	F               llvm.Function
	VPCtx           *VectorPackContext
	NoAlias         bool
	TransitiveClosure map[llvm.Value]llvm.BitVector
}

func NewGlobalDependenceAnalysis(aa llvm.AliasAnalysis, se llvm.ScalarEvolution, li llvm.LoopInfo, lvi llvm.LazyValueInfo, f llvm.Function, vpCtx *VectorPackContext, noAlias bool) *GlobalDependenceAnalysis {
	gda := &GlobalDependenceAnalysis{
		AA:      aa,
		SE:      se,
		LI:      li,
		LVI:     lvi,
		F:       f,
		VPCtx:   vpCtx,
		NoAlias: noAlias,
		TransitiveClosure: make(map[llvm.Value]llvm.BitVector),
	}

	memRefs := []llvm.Value{}
	dependences := make(map[llvm.Value][]llvm.Value)
	rpo := ComputeRPO(f, li)

	processed := make(map[llvm.Value]struct{})
	for _, bb := range rpo {
		l := li.GetLoopFor(bb)
		isHeader := li.IsLoopHeader(bb)
		if isHeader && l.IsNil() {
			panic("Header block without a loop")
		}

		for i := bb.FirstInstruction(); !i.IsNil(); i = i.NextInstruction() {
			processed[i] = struct{}{}

			for _, v := range i.Operands() {
				if opInst, ok := v.(*llvm.Instruction); ok {
					isLoopCarriedDep := isHeader && i.IsAPhiNode() && !l.IsNil() && l.Contains(opInst)
					if !isLoopCarriedDep {
						dependences[i] = append(dependences[i], opInst)
					}
				}
			}

			if i.IsAPhiNode() && isHeader && !l.IsNil() {
				pnLoop := li.GetLoopFor(i.InstructionParent())
				if !pnLoop.IsNil() && pnLoop.Header() == i.InstructionParent() && !pnLoop.Contains(i) {
					if latch := pnLoop.LoopLatch(); !latch.IsNil() {
						if i2 := i.IncomingValueForBlock(latch); !i2.IsNil() {
							dependences[i] = append(dependences[i], i2)
						}
					}
				}
			}

			if !i.IsAnIntrinsic(llvm.IntrinsicExperimentalNoAliasScopeDecl) &&
				!i.IsAnIntrinsic(llvm.IntrinsicLifetimeStart) &&
				!i.IsAnIntrinsic(llvm.IntrinsicLifetimeEnd) &&
				(i.IsAReturnInst() || noAlias || i.MayReadOrWriteMemory()) {
				for _, prevRef := range memRefs {
					if prevRef.IsAReturnInst() || prevRef.MayWriteToMemory() || i.MayWriteToMemory() {
						if isAliased(i, prevRef, aa, se, li, lvi) {
							dependences[i] = append(dependences[i], prevRef)
						}
					}
				}
				memRefs = append(memRefs, i)
			}
		}
	}

	for _, bb := range rpo {
		for i := bb.FirstInstruction(); !i.IsNil(); i = i.NextInstruction() {
			gda.addDependences(i, dependences[i])
		}
	}

	return gda
}

func (gda *GlobalDependenceAnalysis) addDependences(i llvm.Value, deps []llvm.Value) {
	if !gda.VPCtx.IsKnownValue(i) {
		panic("Unknown value")
	}
	depended := llvm.NewBitVector(gda.VPCtx.GetNumValues())
	for _, dep := range deps {
		depended.Set(gda.VPCtx.GetScalarId(dep))
		if existingDeps, ok := gda.TransitiveClosure[dep]; ok {
			depended.Or(existingDeps)
		}
	}
	gda.TransitiveClosure[i] = depended
}

func (gda *GlobalDependenceAnalysis) AddInstruction(i llvm.Value) {
	deps := []llvm.Value{}
	for _, o := range i.Operands() {
		if oi, ok := o.(*llvm.Instruction); ok {
			deps = append(deps, oi)
		}
	}
	gda.addDependences(i, deps)
}