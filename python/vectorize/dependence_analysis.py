from llvmlite import binding as llvm
from typing import Dict, List, Set, Tuple

def is_less_than(se: llvm.ScalarEvolution, a: llvm.SCEV, b: llvm.SCEV) -> bool:
    return se.is_known_negative(se.get_minus_scev(a, b))

def refine_with_range(se: llvm.ScalarEvolution, expr: llvm.SCEV, cr: llvm.ConstantRange) -> llvm.SCEV:
    smin = llvm.APInt.get_signed_min_value(cr.get_bit_width())
    umin = llvm.APInt.get_min_value(cr.get_bit_width())
    smax = llvm.APInt.get_signed_max_value(cr.get_bit_width())
    umax = llvm.APInt.get_max_value(cr.get_bit_width())

    if cr.get_signed_min() != smax and llvm.ConstantRange(cr.get_signed_min(), smax).contains(cr):
        expr = se.get_smax_expr(expr, se.get_constant(cr.get_signed_min()))
    if cr.get_unsigned_min() != umax and llvm.ConstantRange(cr.get_unsigned_min(), umax).contains(cr):
        expr = se.get_umax_expr(expr, se.get_constant(cr.get_unsigned_min()))
    if cr.get_upper() != smin and llvm.ConstantRange(smin, cr.get_upper()).contains(cr):
        expr = se.get_smin_expr(expr, se.get_constant(cr.get_signed_max()))
    if umin != cr.get_upper() and llvm.ConstantRange(umin, cr.get_upper()).contains(cr):
        expr = se.get_umin_expr(expr, se.get_constant(cr.get_unsigned_max()))
    
    return expr

def refine_with_ranges(se: llvm.ScalarEvolution, expr: llvm.SCEV, ranges: Dict[llvm.Value, llvm.ConstantRange]) -> llvm.SCEV:
    value_to_scev = {v: refine_with_range(se, se.get_scev(v), r) for v, r in ranges.items()}
    return llvm.SCEVParameterRewriter.rewrite(expr, se, value_to_scev)

class UnknownSCEVCollector:
    def __init__(self, se: llvm.ScalarEvolution):
        self.se = se
        self.values: Set[llvm.Value] = set()

    def visit_unknown(self, expr: llvm.SCEVUnknown) -> llvm.SCEV:
        if expr.get_type().is_integer_ty():
            self.values.add(expr.get_value())
        return expr

def get_location(i: llvm.Instruction) -> llvm.MemoryLocation:
    if isinstance(i, llvm.StoreInst):
        return llvm.MemoryLocation.get(i)
    elif isinstance(i, llvm.LoadInst):
        return llvm.MemoryLocation.get(i)
    else:
        return llvm.MemoryLocation()

def is_simple(i: llvm.Instruction) -> bool:
    if isinstance(i, llvm.LoadInst):
        return i.is_simple()
    elif isinstance(i, llvm.StoreInst):
        return i.is_simple()
    elif isinstance(i, llvm.MemIntrinsic):
        return not i.is_volatile()
    return True

def get_loop_for_pointer(li: llvm.LoopInfo, ptr: llvm.Value) -> llvm.Loop:
    if isinstance(ptr, llvm.Instruction):
        return li.get_loop_for(ptr.parent)
    return None

def get_base_value(s: llvm.SCEV) -> llvm.Value:
    if isinstance(s, llvm.SCEVAddRecExpr):
        return get_base_value(s.start)
    elif isinstance(s, llvm.SCEVAddExpr):
        last = s.operands[-1]
        if last.type.is_pointer_ty():
            return get_base_value(last)
    elif isinstance(s, llvm.SCEVUnknown):
        return s.value
    return None

def is_aliased(i1: llvm.Instruction, i2: llvm.Instruction, aa: llvm.AliasAnalysis, se: llvm.ScalarEvolution, li: llvm.LoopInfo, lvi: llvm.LazyValueInfo) -> bool:
    loc1 = get_location(i1)
    loc2 = get_location(i2)
    f = i1.parent.parent

    if loc1.ptr and loc2.ptr and is_simple(i1) and is_simple(i2):
        result = aa.alias(loc1, loc2)
        if result != llvm.AliasResult.MayAlias:
            return result == llvm.AliasResult.MustAlias

    ptr1 = i1.get_operand(0) if isinstance(i1, (llvm.LoadInst, llvm.StoreInst)) else None
    ptr2 = i2.get_operand(0) if isinstance(i2, (llvm.LoadInst, llvm.StoreInst)) else None

    if not ptr1 or not ptr2:
        return True

    ptr1_scev = se.get_scev(ptr1)
    ptr2_scev = se.get_scev(ptr2)

    base1 = get_base_value(ptr1_scev)
    base2 = get_base_value(ptr2_scev)
    if base1 and base2 and base1 != base2:
        return aa.alias(llvm.MemoryLocation.get_before_or_after(base1),
                        llvm.MemoryLocation.get_before_or_after(base2)) == llvm.AliasResult.MustAlias

    loops = []
    def collect_loops(s):
        if isinstance(s, llvm.SCEVAddRecExpr):
            loops.append(s.loop)
        return True

    llvm.SCEVTraversal.visit_all(collect_loops, ptr1_scev)
    llvm.SCEVTraversal.visit_all(collect_loops, ptr2_scev)

    loops.sort(key=lambda l: (l, l.contains))
    for i in range(len(loops) - 1):
        if loops[i] != loops[i+1] and not loops[i].contains(loops[i+1]):
            return True

    lt = is_less_than(se, ptr1_scev, ptr2_scev)
    gt = is_less_than(se, ptr2_scev, ptr1_scev)
    if not lt and not gt:
        return True

    if gt:
        ptr1_scev, ptr2_scev = ptr2_scev, ptr1_scev

    ty = ptr1.type.element_type
    as_ = ptr1.type.addr_space
    dl = f.module.data_layout
    index_width = dl.get_index_size_in_bits(as_)
    size = llvm.Constant.int(llvm.IntType(index_width), dl.get_type_store_size(ty))

    return se.is_known_positive(se.get_minus_scev(se.get_add_expr([ptr1_scev, se.get_constant(size)]), ptr2_scev))

class GlobalDependenceAnalysis:
    def __init__(self, aa: llvm.AliasAnalysis, se: llvm.ScalarEvolution, li: llvm.LoopInfo, lvi: llvm.LazyValueInfo, f: llvm.Function, vp_ctx: 'VectorPackContext', no_alias: bool):
        self.aa = aa
        self.se = se
        self.li = li
        self.lvi = lvi
        self.f = f
        self.vp_ctx = vp_ctx
        self.no_alias = no_alias
        self.transitive_closure: Dict[llvm.Value, llvm.BitVector] = {}

        mem_refs: List[llvm.Instruction] = []
        dependences: Dict[llvm.Instruction, List[llvm.Instruction]] = {}
        rpo = compute_rpo(f, li)

        processed: Set[llvm.Instruction] = set()
        for bb in rpo:
            l = li.get_loop_for(bb)
            is_header = li.is_loop_header(bb)
            assert not is_header or l is not None

            for i in bb.instructions:
                processed.add(i)

                for op in i.operands:
                    if isinstance(op, llvm.Instruction):
                        is_loop_carried_dep = is_header and isinstance(i, llvm.PHINode) and l and l.contains(op)
                        if not is_loop_carried_dep:
                            dependences.setdefault(i, []).append(op)

                if isinstance(i, llvm.PHINode) and is_header and l:
                    pn_loop = li.get_loop_for(i.parent)
                    if pn_loop and pn_loop.header == i.parent and not pn_loop.contains(i):
                        if pn_loop.latch:
                            i2 = i.incoming_values[i.incoming_blocks.index(pn_loop.latch)]
                            if isinstance(i2, llvm.Instruction):
                                dependences.setdefault(i, []).append(i2)

                if not i.is_intrinsic('experimental_noalias_scope_decl') and \
                   not i.is_intrinsic('lifetime_start') and \
                   not i.is_intrinsic('lifetime_end') and \
                   (isinstance(i, llvm.ReturnInst) or no_alias or i.may_read_or_write_memory()):
                    for prev_ref in mem_refs:
                        if isinstance(prev_ref, llvm.ReturnInst) or prev_ref.may_write_to_memory() or i.may_write_to_memory():
                            if is_aliased(i, prev_ref, aa, se, li, lvi):
                                dependences.setdefault(i, []).append(prev_ref)

                    mem_refs.append(i)

        for bb in rpo:
            for i in bb.instructions:
                self.add_dependences(i, dependences.get(i, []))

    def add_dependences(self, i: llvm.Instruction, deps: List[llvm.Instruction]):
        assert self.vp_ctx.is_known_value(i)
        depended = llvm.BitVector(self.vp_ctx.get_num_values())
        for dep in deps:
            depended.set(self.vp_ctx.get_scalar_id(dep))
            if dep in self.transitive_closure:
                depended |= self.transitive_closure[dep]
        self.transitive_closure[i] = depended

    def add_instruction(self, i: llvm.Instruction):
        deps = [op for op in i.operands if isinstance(op, llvm.Instruction)]
        self.add_dependences(i, deps)

def compute_rpo(f: llvm.Function, li: llvm.LoopInfo) -> List[llvm.BasicBlock]:
    return list(f.basic_blocks)