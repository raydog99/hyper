import llvmlite.binding as llvm
from llvmlite import ir
import random
from typing import List, Dict, Tuple

def get_loop_nest(li: llvm.LoopInfo, v: llvm.Value) -> List[llvm.Loop]:
    if isinstance(v, llvm.Instruction):
        loop_nest = []
        l = li.get_loop_for(v.parent)
        while l:
            loop_nest.append(l)
            l = l.parent
        return list(reversed(loop_nest))
    return []

def get_address_space_operand(i: llvm.Value) -> int:
    if isinstance(i, llvm.LoadInst):
        return i.ptr.type.addrspace
    elif isinstance(i, llvm.StoreInst):
        return i.ptr.type.addrspace
    return -1

class AddRecLoopRewriter:
    def __init__(self, se: llvm.ScalarEvolution, loops: Dict[llvm.Loop, llvm.Loop]):
        self.se = se
        self.loops = loops
        self.success = True

    def rewrite(self, expr: llvm.SCEV) -> llvm.SCEV:
        if isinstance(expr, llvm.SCEVAddRecExpr):
            old_loop = expr.loop
            new_loop = self.loops.get(old_loop, old_loop)
            operands = [self.rewrite(op) for op in expr.operands]
            if all(self.se.is_available_at_loop_entry(op, new_loop) for op in operands):
                return self.se.get_add_rec_expr(operands, new_loop, expr.flags)
            else:
                self.success = False
                return expr
        return expr

def is_equivalent(ptr_a: llvm.Value, ptr_b: llvm.Value, se: llvm.ScalarEvolution, li: llvm.LoopInfo) -> bool:
    global num_equiv_checks
    num_equiv_checks += 1

    if se.get_scev(ptr_a) == se.get_scev(ptr_b):
        return True

    if not (isinstance(ptr_a, llvm.Instruction) and isinstance(ptr_b, llvm.Instruction)):
        return False

    if ptr_a.type != ptr_b.type:
        return False

    loop_nest1 = get_loop_nest(li, ptr_a)
    loop_nest2 = get_loop_nest(li, ptr_b)
    if len(loop_nest1) != len(loop_nest2):
        return False

    loops = {}
    for l1, l2 in zip(loop_nest1, loop_nest2):
        if not have_identical_trip_counts(l1, l2, se):
            return False
        loops[l2] = l1

    ptr_scev_a = se.get_scev(ptr_a)
    rewriter = AddRecLoopRewriter(se, loops)
    ptr_scev_b = rewriter.rewrite(se.get_scev(ptr_b))
    return ptr_scev_a == ptr_scev_b

def is_consecutive(a: llvm.Value, b: llvm.Value, dl: llvm.DataLayout, se: llvm.ScalarEvolution, li: llvm.LoopInfo) -> bool:
    global num_consec_checks
    num_consec_checks += 1

    ptr_a = get_load_store_pointer_operand(a)
    ptr_b = get_load_store_pointer_operand(b)
    if not (ptr_a and ptr_b):
        return False

    as_a = get_address_space_operand(a)
    as_b = get_address_space_operand(b)

    loop_nest1 = get_loop_nest(li, ptr_a)
    loop_nest2 = get_loop_nest(li, ptr_b)
    if len(loop_nest1) != len(loop_nest2):
        return False

    loops = {}
    for l1, l2 in zip(loop_nest1, loop_nest2):
        if l1 != l2 and not have_identical_trip_counts(l1, l2, se):
            return False
        if l1 != l2:
            loops[l2] = l1

    if as_a != as_b or ptr_a.type != ptr_b.type or ptr_a == ptr_b:
        return False

    idx_width = dl.get_index_size_in_bits(as_a)
    ty = ptr_a.type.element_type

    offset_a, stripped_ptr_a = strip_and_accumulate_inbounds_constant_offsets(dl, ptr_a)
    offset_b, stripped_ptr_b = strip_and_accumulate_inbounds_constant_offsets(dl, ptr_b)

    as_a = stripped_ptr_a.type.addrspace
    as_b = stripped_ptr_b.type.addrspace
    if as_a != as_b:
        return False

    idx_width = dl.get_index_size_in_bits(as_a)
    offset_a = ir.Constant(ir.IntType(idx_width), offset_a).sext(idx_width)
    offset_b = ir.Constant(ir.IntType(idx_width), offset_b).sext(idx_width)

    size = ir.Constant(ir.IntType(idx_width), dl.get_type_store_size(ty))

    offset_scev_a = se.get_constant(offset_a)
    offset_scev_b = se.get_constant(offset_b)
    offset_delta_scev = se.get_minus_scev(offset_scev_b, offset_scev_a)
    offset_delta = offset_delta_scev.constant_value

    if stripped_ptr_a == stripped_ptr_b:
        return offset_delta == size

    size_scev = se.get_constant(size)
    base_delta = se.get_minus_scev(size_scev, offset_delta_scev)

    ptr_scev_a = se.get_scev(stripped_ptr_a)
    ptr_scev_b = se.get_scev(stripped_ptr_b)

    if loops:
        rewriter = AddRecLoopRewriter(se, loops)
        ptr_scev_b = rewriter.rewrite(ptr_scev_b)

    x = se.get_add_expr([ptr_scev_a, base_delta])
    return x == ptr_scev_b

class SizeGenerator:
    def __init__(self):
        self.unknown_to_size = {}

    def get_size(self, expr: llvm.SCEV) -> int:
        i = len(self.unknown_to_size)
        return self.unknown_to_size.setdefault(expr, (2 << i) + i)

class IterationGenerator:
    def __init__(self, offset: int):
        self.iterations = {}
        self.offset = offset

    def get_iteration(self, l: llvm.Loop) -> int:
        return self.iterations.setdefault(l, random.randint(0, 31))

def fingerprint_scev(se: llvm.ScalarEvolution, expr: llvm.SCEV, sg: SizeGenerator, igs: List[IterationGenerator], n: int) -> List[int]:
    class SCEVFingerprinter:
        def __init__(self, se: llvm.ScalarEvolution, sg: SizeGenerator, ig: IterationGenerator):
            self.se = se
            self.sg = sg
            self.ig = ig

        def visit(self, expr: llvm.SCEV) -> llvm.SCEV:
            if isinstance(expr, llvm.SCEVAddRecExpr):
                return self.visit_add_rec_expr(expr)
            elif isinstance(expr, llvm.SCEVUnknown):
                return self.visit_unknown(expr)
            return expr

        def visit_add_rec_expr(self, expr: llvm.SCEVAddRecExpr) -> llvm.SCEV:
            operands = [self.visit(op) for op in expr.operands]
            l = expr.loop
            x = self.se.get_add_rec_expr(operands, l, expr.flags)
            if isinstance(x, llvm.SCEVAddRecExpr):
                return x.evaluate_at_iteration(self.se.get_constant(ir.IntType(64), self.ig.get_iteration(l)), self.se)
            return x

        def visit_unknown(self, expr: llvm.SCEVUnknown) -> llvm.SCEV:
            return self.se.get_constant(expr.type, self.sg.get_size(expr))

    return [SCEVFingerprinter(se, sg, ig).visit(expr).constant_value for ig in igs]

def find_consecutive_accesses(se: llvm.ScalarEvolution, dl: llvm.DataLayout, li: llvm.LoopInfo, accesses: List[llvm.Value], equivalent_accesses: EquivalenceClasses, num_fingerprints: int) -> List[Tuple[llvm.Value, llvm.Value]]:
    if not accesses:
        return []

    fingerprints_to_accesses = {}
    access_to_fingerprints = {}

    ptr = get_load_store_pointer_operand(accesses[0])
    ty = ptr.type.element_type
    size = dl.get_type_store_size(ty)

    consecutive_accesses = []
    sg = SizeGenerator()
    igs = [IterationGenerator(i) for i in range(num_fingerprints)]

    for i in accesses:
        ptr = get_load_store_pointer_operand(i)
        ptr_scev = se.get_scev(ptr)
        fingerprints = fingerprint_scev(se, ptr_scev, sg, igs, num_fingerprints)

        left = fingerprints[0] - size
        for left_i in fingerprints_to_accesses.get(left, []):
            left_fingerprints = access_to_fingerprints[left_i]
            if all(a + size == b for a, b in zip(left_fingerprints, fingerprints)) and is_consecutive(left_i, i, dl, se, li):
                consecutive_accesses.append((left_i, i))

        for i2 in fingerprints_to_accesses.get(fingerprints[0], []):
            fingerprints2 = access_to_fingerprints[i2]
            if fingerprints2 == fingerprints and is_equivalent(get_load_store_pointer_operand(i), get_load_store_pointer_operand(i2), se, li):
                equivalent_accesses.union_sets(i, i2)
                break

        right = fingerprints[0] + size
        for right_i in fingerprints_to_accesses.get(right, []):
            right_fingerprints = access_to_fingerprints[right_i]
            if all(a == b + size for a, b in zip(right_fingerprints, fingerprints)) and is_consecutive(i, right_i, dl, se, li):
                consecutive_accesses.append((i, right_i))
                fingerprints_to_accesses.setdefault(fingerprints[0], []).append(i)
        access_to_fingerprints[i] = fingerprints

    return consecutive_accesses

def get_load_store_pointer_operand(inst):
    if isinstance(inst, llvm.LoadInst):
        return inst.operands[0]
    elif isinstance(inst, llvm.StoreInst):
        return inst.operands[1]
    return None

def have_identical_trip_counts(loop1, loop2, se):
    return True

class EquivalenceClasses:
    def __init__(self):
        self.parent = {}
        self.size = {}

    def find(self, x):
        if x not in self.parent:
            self.parent[x] = x
            self.size[x] = 1
            return x
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union_sets(self, x, y):
        xroot = self.find(x)
        yroot = self.find(y)
        if xroot == yroot:
            return
        if self.size[xroot] < self.size[yroot]:
            self.parent[xroot] = yroot
            self.size[yroot] += self.size[xroot]
        else:
            self.parent[yroot] = xroot
            self.size[xroot] += self.size[yroot]