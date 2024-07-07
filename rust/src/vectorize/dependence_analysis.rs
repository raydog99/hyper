use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::analysis::*;
use std::collections::{HashMap, HashSet};
use std::ffi::CStr;

unsafe fn is_less_than(se: LLVMScalarEvolutionRef, a: LLVMValueRef, b: LLVMValueRef) -> bool {
    LLVMIsKnownNegative(se, LLVMGetMinusSCEV(se, a, b)) != 0
}

unsafe fn refine_with_range(se: LLVMScalarEvolutionRef, expr: LLVMValueRef, cr: LLVMConstantRangeRef) -> LLVMValueRef {
    let smin = LLVMAPIntGetSignedMinValue(LLVMGetBitWidth(cr));
    let umin = LLVMAPIntGetMinValue(LLVMGetBitWidth(cr));
    let smax = LLVMAPIntGetSignedMaxValue(LLVMGetBitWidth(cr));
    let umax = LLVMAPIntGetMaxValue(LLVMGetBitWidth(cr));

    let mut result = expr;

    if LLVMConstantRangeGetSignedMin(cr) != smax && LLVMConstantRangeContains(LLVMConstantRangeCreate(LLVMConstantRangeGetSignedMin(cr), smax), cr) != 0 {
        result = LLVMGetSMaxExpr(se, result, LLVMGetConstant(se, LLVMConstantRangeGetSignedMin(cr)));
    }
    if LLVMConstantRangeGetUnsignedMin(cr) != umax && LLVMConstantRangeContains(LLVMConstantRangeCreate(LLVMConstantRangeGetUnsignedMin(cr), umax), cr) != 0 {
        result = LLVMGetUMaxExpr(se, result, LLVMGetConstant(se, LLVMConstantRangeGetUnsignedMin(cr)));
    }
    if LLVMConstantRangeGetUpper(cr) != smin && LLVMConstantRangeContains(LLVMConstantRangeCreate(smin, LLVMConstantRangeGetUpper(cr)), cr) != 0 {
        result = LLVMGetSMinExpr(se, result, LLVMGetConstant(se, LLVMConstantRangeGetSignedMax(cr)));
    }
    if umin != LLVMConstantRangeGetUpper(cr) && LLVMConstantRangeContains(LLVMConstantRangeCreate(umin, LLVMConstantRangeGetUpper(cr)), cr) != 0 {
        result = LLVMGetUMinExpr(se, result, LLVMGetConstant(se, LLVMConstantRangeGetUnsignedMax(cr)));
    }

    result
}

unsafe fn refine_with_ranges(se: LLVMScalarEvolutionRef, expr: LLVMValueRef, ranges: &HashMap<LLVMValueRef, LLVMConstantRangeRef>) -> LLVMValueRef {
    let mut value_to_scev = HashMap::new();
    for (v, r) in ranges {
        value_to_scev.insert(*v, refine_with_range(se, LLVMGetSCEV(se, *v), *r));
    }
    LLVMSCEVParameterRewriterRewrite(expr, se, &value_to_scev)
}

struct UnknownSCEVCollector {
    values: HashSet<LLVMValueRef>,
}

impl UnknownSCEVCollector {
    fn new() -> Self {
        UnknownSCEVCollector {
            values: HashSet::new(),
        }
    }

    unsafe fn visit_unknown(&mut self, expr: LLVMValueRef) -> LLVMValueRef {
        if LLVMTypeOf(expr).is_integer_ty() {
            self.values.insert(LLVMSCEVUnknownGetValue(expr));
        }
        expr
    }
}

unsafe fn get_location(i: LLVMValueRef) -> LLVMMemoryLocationRef {
    match LLVMGetInstructionOpcode(i) {
        LLVMStore => LLVMGetStore(i),
        LLVMLoad => LLVMGetLoad(i),
        _ => std::ptr::null_mut(),
    }
}

unsafe fn is_simple(i: LLVMValueRef) -> bool {
    match LLVMGetInstructionOpcode(i) {
        LLVMLoad => LLVMIsSimpleLoad(i) != 0,
        LLVMStore => LLVMIsSimpleStore(i) != 0,
        LLVMCall => !LLVMGetCallSiteAttributes(i).has_attribute(LLVMAttributeVolatile),
        _ => true,
    }
}

unsafe fn get_loop_for_pointer(li: LLVMLoopInfoRef, ptr: LLVMValueRef) -> LLVMLoopRef {
    if let Some(i) = ptr.as_instruction() {
        LLVMGetLoopFor(li, LLVMGetInstructionParent(i))
    } else {
        std::ptr::null_mut()
    }
}

unsafe fn get_base_value(s: LLVMValueRef) -> Option<LLVMValueRef> {
    match LLVMSCEVType(s) {
        LLVMSCEVAddRecExpr => get_base_value(LLVMSCEVAddRecExprGetStart(s)),
        LLVMSCEVAddExpr => {
            let last = LLVMSCEVAddExprGetOperand(s, LLVMSCEVAddExprGetNumOperands(s) - 1);
            if LLVMTypeOf(last).is_pointer_ty() {
                get_base_value(last)
            } else {
                None
            }
        }
        LLVMSCEVUnknown => Some(LLVMSCEVUnknownGetValue(s)),
        _ => None,
    }
}

unsafe fn is_aliased(i1: LLVMValueRef, i2: LLVMValueRef, aa: LLVMAliasAnalysisRef, se: LLVMScalarEvolutionRef, li: LLVMLoopInfoRef, lvi: LLVMLazyValueInfoRef) -> bool {
    let loc1 = get_location(i1);
    let loc2 = get_location(i2);
    let f = LLVMGetGlobalParent(LLVMGetInstructionParent(i1));

    if loc1.is_null() || loc2.is_null() || !is_simple(i1) || !is_simple(i2) {
        return true;
    }

    let result = LLVMAliasAnalysisAlias(aa, loc1, loc2);
    if result != LLVMMayAlias {
        return result == LLVMMustAlias;
    }

    let ptr1 = LLVMGetLoadStorePointerOperand(i1);
    let ptr2 = LLVMGetLoadStorePointerOperand(i2);
    if ptr1.is_null() || ptr2.is_null() {
        return true;
    }

    let ptr1_scev = LLVMGetSCEV(se, ptr1);
    let ptr2_scev = LLVMGetSCEV(se, ptr2);

    let base1 = get_base_value(ptr1_scev);
    let base2 = get_base_value(ptr2_scev);
    if let (Some(base1), Some(base2)) = (base1, base2) {
        if base1 != base2 {
            return LLVMAliasAnalysisAlias(aa, LLVMMemoryLocationGetBeforeOrAfter(base1), LLVMMemoryLocationGetBeforeOrAfter(base2)) == LLVMMustAlias;
        }
    }

    let mut loops = Vec::new();
    let collect_loops = |s: LLVMValueRef| {
        if LLVMSCEVType(s) == LLVMSCEVAddRecExpr {
            loops.push(LLVMSCEVAddRecExprGetLoop(s));
        }
        true
    };
    LLVMSCEVTraversalVisitAll(collect_loops, ptr1_scev);
    LLVMSCEVTraversalVisitAll(collect_loops, ptr2_scev);

    loops.sort_by(|a, b| {
        if *a == *b || LLVMLoopContains(*a, *b) != 0 {
            std::cmp::Ordering::Equal
        } else {
            std::cmp::Ordering::Greater
        }
    });

    for i in 0..loops.len() - 1 {
        if loops[i] != loops[i + 1] && LLVMLoopContains(loops[i], loops[i + 1]) == 0 {
            return true;
        }
    }

    let lt = is_less_than(se, ptr1_scev, ptr2_scev);
    let gt = is_less_than(se, ptr2_scev, ptr1_scev);
    if !lt && !gt {
        return true;
    }

    let (ptr1_scev, ptr2_scev) = if gt {
        (ptr2_scev, ptr1_scev)
    } else {
        (ptr1_scev, ptr2_scev)
    };

    let ty = LLVMGetElementType(LLVMTypeOf(ptr1));
    let as_ = LLVMGetPointerAddressSpace(LLVMTypeOf(ptr1));
    let dl = LLVMGetModuleDataLayout(LLVMGetGlobalParent(f));
    let index_width = LLVMGetIndexTypeSizeInBits(dl, as_);
    let size = LLVMConstInt(LLVMIntTypeInContext(LLVMGetGlobalContext(), index_width), LLVMStoreSizeOfType(dl, ty) as u64, 0);

    LLVMIsKnownPositive(se, LLVMGetMinusSCEV(se, LLVMGetAddExpr(se, &[ptr1_scev, LLVMGetConstant(se, size)], 2), ptr2_scev)) != 0
}

struct GlobalDependenceAnalysis {
    aa: LLVMAliasAnalysisRef,
    se: LLVMScalarEvolutionRef,
    li: LLVMLoopInfoRef,
    lvi: LLVMLazyValueInfoRef,
    f: LLVMValueRef,
    vp_ctx: *mut VectorPackContext,
    no_alias: bool,
    transitive_closure: HashMap<LLVMValueRef, LLVMBitVectorRef>,
}

impl GlobalDependenceAnalysis {
    unsafe fn new(aa: LLVMAliasAnalysisRef, se: LLVMScalarEvolutionRef, li: LLVMLoopInfoRef, lvi: LLVMLazyValueInfoRef, f: LLVMValueRef, vp_ctx: *mut VectorPackContext, no_alias: bool) -> Self {
        let mut gda = GlobalDependenceAnalysis {
            aa,
            se,
            li,
            lvi,
            f,
            vp_ctx,
            no_alias,
            transitive_closure: HashMap::new(),
        };

        let mut mem_refs = Vec::new();
        let mut dependences = HashMap::new();
        let mut rpo = Vec::new();
        compute_rpo(f, li, &mut rpo);

        let mut processed = HashSet::new();
        for bb in rpo.iter() {
            let l = LLVMGetLoopFor(li, *bb);
            let is_header = LLVMIsLoopHeader(li, *bb) != 0;
            assert!(!is_header || !l.is_null());

            let mut inst = LLVMGetFirstInstruction(*bb);
            while !inst.is_null() {
                processed.insert(inst);

                for op in (0..LLVMGetNumOperands(inst)).map(|i| LLVMGetOperand(inst, i)) {
                    if let Some(op_inst) = op.as_instruction() {
                        let is_loop_carried_dep = is_header && LLVMIsAPHINode(inst) != 0 && !l.is_null() && LLVMLoopContains(l, op_inst) != 0;
                        if !is_loop_carried_dep {
                            dependences.entry(inst).or_insert_with(Vec::new).push(op_inst);
                        }
                    }
                }

                if LLVMIsAPHINode(inst) != 0 && is_header && !l.is_null() {
                    let pn_loop = LLVMGetLoopFor(li, LLVMGetInstructionParent(inst));
                    if !pn_loop.is_null() && LLVMGetHeader(pn_loop) == LLVMGetInstructionParent(inst) && LLVMLoopContains(pn_loop, inst) == 0 {
                        if let Some(latch) = LLVMGetLoopLatch(pn_loop).as_ref() {
                            if let Some(i2) = LLVMGetIncomingValueForBlock(inst, *latch).as_instruction() {
                                dependences.entry(inst).or_insert_with(Vec::new).push(i2);
                            }
                        }
                    }
                }

                if LLVMIsAIntrinsicInst(inst).is_null() &&
                   (LLVMIsAReturnInst(inst).is_null() || no_alias || LLVMMayReadOrWriteMemory(inst) != 0) {
                    for prev_ref in &mem_refs {
                        if LLVMIsAReturnInst(*prev_ref).is_null() || LLVMMayWriteToMemory(*prev_ref) != 0 || LLVMMayWriteToMemory(inst) != 0 {
                            if is_aliased(inst, *prev_ref, aa, se, li, lvi) {
                                dependences.entry(inst).or_insert_with(Vec::new).push(*prev_ref);
                            }
                        }
                    }
                    mem_refs.push(inst);
                }

                inst = LLVMGetNextInstruction(inst);
            }
        }

        for bb in rpo.iter() {
            let mut inst = LLVMGetFirstInstruction(*bb);
            while !inst.is_null() {
                if let Some(deps) = dependences.get(&inst) {
                    gda.add_dependences(inst, deps);
                }
                inst = LLVMGetNextInstruction(inst);
            }
        }

        gda
    }

    unsafe fn add_dependences(&mut self, i: LLVMValueRef, deps: &[LLVMValueRef]) {
        assert!((*self.vp_ctx).is_known_value(i) != 0);
        let mut depended = LLVMBitVectorCreate((*self.vp_ctx).get_num_values());
        for dep in deps {
            LLVMBitVectorSet(depended, (*self.vp_ctx).get_scalar_id(*dep));
            if let Some(existing_deps) = self.transitive_closure.get(dep) {
                LLVMBitVectorOr(depended, *existing_deps);
            }
        }
        self.transitive_closure.insert(i, depended);
    }

    unsafe fn add_instruction(&mut self, i: LLVMValueRef) {
        let mut deps = Vec::new();
        for op in (0..LLVMGetNumOperands(i)).map(|idx| LLVMGetOperand(i, idx)) {
            if let Some(op_inst) = op.as_instruction() {
                deps.push(op_inst);
            }
        }
        self.add_dependences(i, &deps);
    }
}