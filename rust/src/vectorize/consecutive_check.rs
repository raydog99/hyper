use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::analysis::*;
use std::collections::HashMap;
use std::ffi::CStr;

unsafe fn get_loop_nest(li: LLVMLoopInfoRef, v: LLVMValueRef) -> Vec<LLVMLoopRef> {
    let mut loop_nest = Vec::new();
    let mut l = LLVMGetLoopFor(li, LLVMGetInstructionParent(v));
    while !l.is_null() {
        loop_nest.push(l);
        l = LLVMGetLoopParent(l);
    }
    loop_nest.reverse();
    loop_nest
}

unsafe fn get_address_space_operand(i: LLVMValueRef) -> u32 {
    if LLVMIsALoadInst(i) != std::ptr::null_mut() {
        LLVMGetPointerAddressSpace(LLVMGetOperand(i, 0))
    } else if LLVMIsAStoreInst(i) != std::ptr::null_mut() {
        LLVMGetPointerAddressSpace(LLVMGetOperand(i, 1))
    } else {
        std::u32::MAX
    }
}

struct AddRecLoopRewriter<'a> {
    se: &'a LLVMScalarEvolutionRef,
    loops: HashMap<LLVMLoopRef, LLVMLoopRef>,
    success: bool,
}

impl<'a> AddRecLoopRewriter<'a> {
    fn new(se: &'a LLVMScalarEvolutionRef, loops: HashMap<LLVMLoopRef, LLVMLoopRef>) -> Self {
        AddRecLoopRewriter {
            se,
            loops,
            success: true,
        }
    }

    unsafe fn rewrite(&mut self, expr: LLVMValueRef) -> LLVMValueRef {
        if LLVMSCEVAddRecExpr == LLVMGetSCEVType(expr) {
            let old_loop = LLVMGetLoopForAddRecExpr(*self.se, expr);
            let new_loop = *self.loops.get(&old_loop).unwrap_or(&old_loop);
            let num_operands = LLVMGetNumOperands(expr);
            let mut operands = Vec::with_capacity(num_operands as usize);
            for i in 0..num_operands {
                operands.push(self.rewrite(LLVMGetOperand(expr, i)));
            }
            if operands.iter().all(|&op| LLVMIsSCEVAvailableAtLoop(*self.se, op, new_loop) != 0) {
                LLVMGetAddRecExpr(*self.se, operands.as_ptr(), operands.len() as u32, new_loop)
            } else {
                self.success = false;
                expr
            }
        } else {
            expr
        }
    }
}

unsafe fn is_equivalent(ptr_a: LLVMValueRef, ptr_b: LLVMValueRef, se: LLVMScalarEvolutionRef, li: LLVMLoopInfoRef) -> bool {
    NUM_EQUIV_CHECKS += 1;
    if LLVMIsSCEVEqual(LLVMGetSCEV(se, ptr_a), LLVMGetSCEV(se, ptr_b)) != 0 {
        return true;
    }
    if LLVMIsAInstruction(ptr_a) == std::ptr::null_mut() || LLVMIsAInstruction(ptr_b) == std::ptr::null_mut() {
        return false;
    }
    if LLVMTypeOf(ptr_a) != LLVMTypeOf(ptr_b) {
        return false;
    }
    let loop_nest1 = get_loop_nest(li, ptr_a);
    let loop_nest2 = get_loop_nest(li, ptr_b);
    if loop_nest1.len() != loop_nest2.len() {
        return false;
    }
    let mut loops = HashMap::new();
    for (&l1, &l2) in loop_nest1.iter().zip(loop_nest2.iter()) {
        if have_identical_trip_counts(l1, l2, se) == 0 {
            return false;
        }
        loops.insert(l2, l1);
    }
    let ptr_scev_a = LLVMGetSCEV(se, ptr_a);
    let mut rewriter = AddRecLoopRewriter::new(&se, loops);
    let ptr_scev_b = rewriter.rewrite(LLVMGetSCEV(se, ptr_b));
    LLVMIsSCEVEqual(ptr_scev_a, ptr_scev_b) != 0
}

unsafe fn is_consecutive(a: LLVMValueRef, b: LLVMValueRef, dl: LLVMTargetDataRef, se: LLVMScalarEvolutionRef, li: LLVMLoopInfoRef) -> bool {
    NUM_CONSEC_CHECKS += 1;
    let ptr_a = LLVMGetLoadStorePointerOperand(a);
    let ptr_b = LLVMGetLoadStorePointerOperand(b);
    if ptr_a.is_null() || ptr_b.is_null() {
        return false;
    }
    let as_a = get_address_space_operand(a);
    let as_b = get_address_space_operand(b);
    let loop_nest1 = get_loop_nest(li, ptr_a);
    let loop_nest2 = get_loop_nest(li, ptr_b);
    if loop_nest1.len() != loop_nest2.len() {
        return false;
    }
    let mut loops = HashMap::new();
    for (&l1, &l2) in loop_nest1.iter().zip(loop_nest2.iter()) {
        if l1 != l2 && have_identical_trip_counts(l1, l2, se) == 0 {
            return false;
        }
        if l1 != l2 {
            loops.insert(l2, l1);
        }
    }
    if as_a != as_b || LLVMTypeOf(ptr_a) != LLVMTypeOf(ptr_b) || ptr_a == ptr_b {
        return false;
    }
    let idx_width = LLVMGetIndexTypeSizeInBits(dl, as_a);
    let ty = LLVMGetElementType(LLVMTypeOf(ptr_a));
    let (offset_a, stripped_ptr_a) = strip_and_accumulate_inbounds_constant_offsets(dl, ptr_a);
    let (offset_b, stripped_ptr_b) = strip_and_accumulate_inbounds_constant_offsets(dl, ptr_b);
    let as_a = LLVMGetPointerAddressSpace(LLVMTypeOf(stripped_ptr_a));
    let as_b = LLVMGetPointerAddressSpace(LLVMTypeOf(stripped_ptr_b));
    if as_a != as_b {
        return false;
    }
    let idx_width = LLVMGetIndexTypeSizeInBits(dl, as_a);
    let offset_a = LLVMConstIntGetSExtValue(LLVMConstIntCast(offset_a, LLVMIntTypeInContext(LLVMGetGlobalContext(), idx_width), 1));
    let offset_b = LLVMConstIntGetSExtValue(LLVMConstIntCast(offset_b, LLVMIntTypeInContext(LLVMGetGlobalContext(), idx_width), 1));
    let size = LLVMConstIntGetSExtValue(LLVMConstInt(LLVMIntTypeInContext(LLVMGetGlobalContext(), idx_width), LLVMStoreSizeOfType(dl, ty), 0));
    let offset_scev_a = LLVMGetConstant(se, offset_a);
    let offset_scev_b = LLVMGetConstant(se, offset_b);
    let offset_delta_scev = LLVMGetMinusSCEV(se, offset_scev_b, offset_scev_a);
    let offset_delta = LLVMConstIntGetSExtValue(LLVMSCEVConstant(offset_delta_scev));
    if stripped_ptr_a == stripped_ptr_b {
        return offset_delta == size;
    }
    let size_scev = LLVMGetConstant(se, size);
    let base_delta = LLVMGetMinusSCEV(se, size_scev, offset_delta_scev);
    let ptr_scev_a = LLVMGetSCEV(se, stripped_ptr_a);
    let mut ptr_scev_b = LLVMGetSCEV(se, stripped_ptr_b);
    if !loops.is_empty() {
        let mut rewriter = AddRecLoopRewriter::new(&se, loops);
        ptr_scev_b = rewriter.rewrite(ptr_scev_b);
    }
    let x = LLVMGetAddExpr(se, &[ptr_scev_a, base_delta], 2);
    LLVMIsSCEVEqual(x, ptr_scev_b) != 0
}

struct SizeGenerator {
    unknown_to_size: HashMap<LLVMValueRef, i64>,
}

impl SizeGenerator {
    fn new() -> Self {
        SizeGenerator {
            unknown_to_size: HashMap::new(),
        }
    }

    fn get_size(&mut self, expr: LLVMValueRef) -> i64 {
        let i = self.unknown_to_size.len() as i64;
        *self.unknown_to_size.entry(expr).or_insert((2 << i) + i)
    }
}

struct IterationGenerator {
    iterations: HashMap<LLVMLoopRef, i64>,
    offset: i64,
}

impl IterationGenerator {
    fn new(offset: i64) -> Self {
        IterationGenerator {
            iterations: HashMap::new(),
            offset,
        }
    }

    fn get_iteration(&mut self, l: LLVMLoopRef) -> i64 {
        let i = self.iterations.len() as i64;
        *self.iterations.entry(l).or_insert(rand::random::<i64>() % 32)
    }
}

unsafe fn fingerprint_scev(se: LLVMScalarEvolutionRef, expr: LLVMValueRef, sg: &mut SizeGenerator, igs: &mut [IterationGenerator], n: usize) -> Vec<i64> {
    let mut fingerprints = vec![0; n];
    for i in 0..n {
        let mut fingerprinter = SCEVFingerprinter { se, sg, ig: &mut igs[i] };
        let result = fingerprinter.visit(expr);
        fingerprints[i] = LLVMConstIntGetSExtValue(LLVMSCEVConstant(result));
    }
    fingerprints
}

struct SCEVFingerprinter<'a> {
    se: LLVMScalarEvolutionRef,
    sg: &'a mut SizeGenerator,
    ig: &'a mut IterationGenerator,
}

impl<'a> SCEVFingerprinter<'a> {
    unsafe fn visit(&mut self, expr: LLVMValueRef) -> LLVMValueRef {
        match LLVMGetSCEVType(expr) {
            LLVMSCEVAddRecExpr => self.visit_add_rec_expr(expr),
            LLVMSCEVUnknown => self.visit_unknown(expr),
            _ => expr,
        }
    }

    unsafe fn visit_add_rec_expr(&mut self, expr: LLVMValueRef) -> LLVMValueRef {
        let num_operands = LLVMGetNumOperands(expr);
        let mut operands = Vec::with_capacity(num_operands as usize);
        for i in 0..num_operands {operands.push(self.visit(LLVMGetOperand(expr, i)));
        }
        let l = LLVMGetLoopForAddRecExpr(self.se, expr);
        let x = LLVMGetAddRecExpr(self.se, operands.as_ptr(), operands.len() as u32, l);
        if LLVMGetSCEVType(x) == LLVMSCEVAddRecExpr {
            LLVMEvaluateAddRecExprAtIteration(self.se, x, LLVMConstInt(LLVMInt64TypeInContext(LLVMGetGlobalContext()), self.ig.get_iteration(l) as u64, 0))
        } else {
            x
        }
    }

    unsafe fn visit_unknown(&mut self, expr: LLVMValueRef) -> LLVMValueRef {
        LLVMConstInt(LLVMTypeOf(expr), self.sg.get_size(expr) as u64, 0)
    }
}

unsafe fn find_consecutive_accesses(se: LLVMScalarEvolutionRef, dl: LLVMTargetDataRef, li: LLVMLoopInfoRef, accesses: &[LLVMValueRef], equivalent_accesses: &mut EquivalenceClasses<LLVMValueRef>, num_fingerprints: usize) -> Vec<(LLVMValueRef, LLVMValueRef)> {
    if accesses.is_empty() {
        return vec![];
    }

    let mut fingerprints_to_accesses: HashMap<i64, Vec<LLVMValueRef>> = HashMap::new();
    let mut access_to_fingerprints: HashMap<LLVMValueRef, Vec<i64>> = HashMap::new();

    let ptr = LLVMGetLoadStorePointerOperand(accesses[0]);
    let ty = LLVMGetElementType(LLVMTypeOf(ptr));
    let size = LLVMStoreSizeOfType(dl, ty) as i64;

    let mut consecutive_accesses = vec![];
    let mut sg = SizeGenerator::new();
    let mut igs: Vec<_> = (0..num_fingerprints).map(|i| IterationGenerator::new(i as i64)).collect();

    for &i in accesses {
        let ptr = LLVMGetLoadStorePointerOperand(i);
        let ptr_scev = LLVMGetSCEV(se, ptr);
        let fingerprints = fingerprint_scev(se, ptr_scev, &mut sg, &mut igs, num_fingerprints);

        let left = fingerprints[0] - size;
        if let Some(left_accesses) = fingerprints_to_accesses.get(&left) {
            for &left_i in left_accesses {
                let left_fingerprints = access_to_fingerprints.get(&left_i).unwrap();
                if left_fingerprints.iter().zip(&fingerprints).all(|(&a, &b)| a + size == b) && is_consecutive(left_i, i, dl, se, li) {
                    consecutive_accesses.push((left_i, i));
                }
            }
        }

        if let Some(equiv_accesses) = fingerprints_to_accesses.get(&fingerprints[0]) {
            for &i2 in equiv_accesses {
                let fingerprints2 = access_to_fingerprints.get(&i2).unwrap();
                if fingerprints2 == &fingerprints && is_equivalent(LLVMGetLoadStorePointerOperand(i), LLVMGetLoadStorePointerOperand(i2), se, li) {
                    equivalent_accesses.union_sets(i, i2);
                    break;
                }
            }
        }

        let right = fingerprints[0] + size;
        if let Some(right_accesses) = fingerprints_to_accesses.get(&right) {
            for &right_i in right_accesses {
                let right_fingerprints = access_to_fingerprints.get(&right_i).unwrap();
                if right_fingerprints.iter().zip(&fingerprints).all(|(&a, &b)| a == b + size) && is_consecutive(i, right_i, dl, se, li) {
                    consecutive_accesses.push((i, right_i));
                }
            }
        }

        fingerprints_to_accesses.entry(fingerprints[0]).or_default().push(i);
        access_to_fingerprints.insert(i, fingerprints);
    }

    consecutive_accesses
}