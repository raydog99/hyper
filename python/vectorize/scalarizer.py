import llvmlite.ir as ir
import llvmlite.binding as llvm

from collections import defaultdict
from typing import Dict, List, Tuple, Optional

SCALARIZE_VARIABLE_INSERT_EXTRACT = True
SCALARIZE_LOAD_STORE = False

ValueVector = List[ir.Value]
ScatterMap = Dict[ir.Value, ValueVector]
GatherList = List[Tuple[ir.Instruction, ValueVector]]

class LoopUnrollResult:
    UNMODIFIED = 0
    PARTIALLY_UNROLLED = 1
    FULLY_UNROLLED = 2

class UnrolledValue:
    def __init__(self, iteration: int, original: ir.Value):
        self.iteration = iteration
        self.original = original

class VectorLayout:
    def __init__(self, vec_ty: ir.VectorType, elem_ty: ir.Type, vec_align: int, elem_size: int):
        self.vec_ty = vec_ty
        self.elem_ty = elem_ty
        self.vec_align = vec_align
        self.elem_size = elem_size

class Scalarizer:
    def __init__(self, module: ir.Module):
        self.module = module
        self.builder = ir.IRBuilder()
        self.scattered: ScatterMap = defaultdict(list)
        self.gathered: GatherList = []
        self.potentially_dead_instrs: List[ir.Instruction] = []
        self.parallel_loop_access_md_kind = module.context.get_named_metadata("llvm.mem.parallel_loop_access")

    def skip_past_phi_nodes_and_dbg(self, iterator):
        while True:
            if isinstance(iterator, ir.PhiInstr):
                iterator = iterator.parent.instructions[iterator.parent.instructions.index(iterator) + 1]
            elif isinstance(iterator, ir.CallInstr) and iterator.called_function.name == "llvm.dbg.value":
                iterator = iterator.parent.instructions[iterator.parent.instructions.index(iterator) + 1]
            else:
                return iterator

    def scatter(self, point: ir.BasicBlock, v: ir.Value) -> ValueVector:
        def scatter_helper(bb: ir.BasicBlock, v: ir.Value) -> ValueVector:
            ty = v.type
            if isinstance(ty, ir.VectorType):
                size = ty.count
            elif isinstance(ty, ir.PointerType) and isinstance(ty.pointee, ir.VectorType):
                size = ty.pointee.count
            else:
                raise ValueError("Unexpected type in scatter_helper")

            cache = self.scattered.get(v, [None] * size)

            for i in range(size):
                if cache[i] is None:
                    if isinstance(ty, ir.PointerType):
                        if i == 0:
                            new_ptr_ty = ir.PointerType(ty.pointee.element)
                            new_val = self.builder.bitcast(v, new_ptr_ty, f"{v.name}.i0")
                        else:
                            gep = self.builder.gep(v, [ir.Constant(ir.IntType(32), i)], f"{v.name}.i{i}")
                            new_val = self.builder.load(gep, f"{v.name}.i{i}")
                    else:
                        new_val = self.builder.extract_element(v, ir.Constant(ir.IntType(32), i), f"{v.name}.i{i}")
                    cache[i] = new_val

            self.scattered[v] = cache
            return cache

        if isinstance(v, ir.Argument):
            func = v.parent
            entry = func.entry_basic_block
            return scatter_helper(entry, v)
        elif isinstance(v, ir.Instruction):
            bb = v.parent
            iterator = self.skip_past_phi_nodes_and_dbg(bb.instructions[bb.instructions.index(v) + 1])
            return scatter_helper(bb, v)
        else:
            return scatter_helper(point, v)

    def gather(self, op: ir.Instruction, cv: ValueVector):
        self.transfer_metadata_and_ir_flags(op, cv)

        if op in self.scattered:
            sv = self.scattered[op]
            for i, v in enumerate(cv):
                if sv[i] is not None and sv[i] != v:
                    sv[i].replace_all_uses_with(v)
                    self.potentially_dead_instrs.append(sv[i])

        self.scattered[op] = cv
        self.gathered.append((op, cv))

    def can_transfer_metadata(self, tag: str) -> bool:
        return tag in ["tbaa", "fpmath", "tbaa.struct", "invariant.load", "alias.scope", "noalias", 
                       "llvm.mem.parallel_loop_access", "access_group"]

    def transfer_metadata_and_ir_flags(self, op: ir.Instruction, cv: ValueVector):
        for v in cv:
            if isinstance(v, ir.Instruction):
                for tag, md in op.metadata.items():
                    if self.can_transfer_metadata(tag):
                        v.set_metadata(tag, md)
                # Note: Python LLVM bindings might not have direct equivalents for copyFastMathFlags
                # You may need to implement these separately or use lower-level LLVM API calls
                if op.dbgloc and not v.dbgloc:
                    v.set_debug_loc(op.dbgloc)

    def get_vector_layout(self, ty: ir.Type, alignment: int) -> Optional[VectorLayout]:
        if isinstance(ty, ir.VectorType):
            elem_ty = ty.element
            if self.module.data_layout.get_type_alloc_size(elem_ty) == self.module.data_layout.get_type_store_size(elem_ty):
                return VectorLayout(ty, elem_ty, alignment, self.module.data_layout.get_type_store_size(elem_ty))
        return None

    def visit_select_inst(self, si: ir.SelectInstr) -> bool:
        if isinstance(si.type, ir.VectorType):
            num_elems = si.type.count
            vop1 = self.scatter(si.parent, si.trueval)
            vop2 = self.scatter(si.parent, si.falseval)
            res = []

            if isinstance(si.condition.type, ir.VectorType):
                vop0 = self.scatter(si.parent, si.condition)
                for i in range(num_elems):
                    res.append(self.builder.select(vop0[i], vop1[i], vop2[i], f"{si.name}.i{i}"))
            else:
                for i in range(num_elems):
                    res.append(self.builder.select(si.condition, vop1[i], vop2[i], f"{si.name}.i{i}"))

            self.gather(si, res)
            return True
        return False

    def visit_cmp_inst(self, ci: ir.CmpInstr, is_fcmp: bool) -> bool:
        if isinstance(ci.type, ir.VectorType):
            num_elems = ci.type.count
            vop0 = self.scatter(ci.parent, ci.operands[0])
            vop1 = self.scatter(ci.parent, ci.operands[1])
            res = []

            for i in range(num_elems):
                if is_fcmp:
                    res.append(self.builder.fcmp(ci.predicate, vop0[i], vop1[i], f"{ci.name}.i{i}"))
                else:
                    res.append(self.builder.icmp(ci.predicate, vop0[i], vop1[i], f"{ci.name}.i{i}"))

            self.gather(ci, res)
            return True
        return False

    def visit_binary_operator(self, bo: ir.BinaryOperator) -> bool:
        if isinstance(bo.type, ir.VectorType):
            num_elems = bo.type.count
            vop0 = self.scatter(bo.parent, bo.operands[0])
            vop1 = self.scatter(bo.parent, bo.operands[1])
            res = []

            for i in range(num_elems):
                res.append(self.builder.binop(bo.opcode, vop0[i], vop1[i], f"{bo.name}.i{i}"))

            self.gather(bo, res)
            return True
        return False

    def visit_gep_inst(self, gepi: ir.GetElementPtrInstr) -> bool:
        if isinstance(gepi.type, ir.VectorType):
            num_elems = gepi.type.count
            vbase = self.scatter(gepi.parent, gepi.operands[0])
            vindices = [self.scatter(gepi.parent, idx) for idx in gepi.operands[1:]]
            res = []

            for i in range(num_elems):
                indices = [vindex[i] for vindex in vindices]
                res.append(self.builder.gep(vbase[i], indices, f"{gepi.name}.i{i}"))

            self.gather(gepi, res)
            return True
        return False

    def visit_cast_inst(self, ci: ir.CastInstr) -> bool:
        if isinstance(ci.type, ir.VectorType):
            num_elems = ci.type.count
            vop = self.scatter(ci.parent, ci.operands[0])
            res = []

            for i in range(num_elems):
                res.append(self.builder.cast(ci.opcode, vop[i], ci.type.element, f"{ci.name}.i{i}"))

            self.gather(ci, res)
            return True
        return False

    def visit_load_inst(self, li: ir.LoadInstr) -> bool:
        if not SCALARIZE_LOAD_STORE or li.is_volatile:
            return False

        layout = self.get_vector_layout(li.type, li.align)
        if layout:
            num_elems = layout.vec_ty.count
            vptr = self.scatter(li.parent, li.operands[0])
            res = []

            for i in range(num_elems):
                load = self.builder.load(vptr[i], f"{li.name}.i{i}")
                load.align = li.align
                res.append(load)

            self.gather(li, res)
            return True
        return False

    def visit_store_inst(self, si: ir.StoreInstr) -> bool:
        if not SCALARIZE_LOAD_STORE or si.is_volatile:
            return False

        layout = self.get_vector_layout(si.value.type, si.align)
        if layout:
            num_elems = layout.vec_ty.count
            vptr = self.scatter(si.parent, si.ptr)
            vval = self.scatter(si.parent, si.value)

            for i in range(num_elems):
                store = self.builder.store(vval[i], vptr[i])
                store.align = si.align

            return True
        return False

    def visit_call_inst(self, ci: ir.CallInstr) -> bool:
        if isinstance(ci.type, ir.VectorType):
            num_elems = ci.type.count
            vargs = [self.scatter(ci.parent, arg) for arg in ci.args]
            res = []

            for i in range(num_elems):
                args = [varg[i] for varg in vargs]
                res.append(self.builder.call(ci.called_function, args, f"{ci.name}.i{i}"))

            self.gather(ci, res)
            return True
        return False

    def visit_phi_node(self, phi: ir.PhiInstr) -> bool:
        if isinstance(phi.type, ir.VectorType):
            num_elems = phi.type.count
            res = []

            for i in range(num_elems):
                new_phi = self.builder.phi(phi.type.element, f"{phi.name}.i{i}")
                for value, block in phi.incoming:
                    vop = self.scatter(block, value)
                    new_phi.add_incoming(vop[i], block)
                res.append(new_phi)

            self.gather(phi, res)
            return True
        return False

    def visit_instruction(self, inst: ir.Instruction) -> bool:
        if isinstance(inst, ir.SelectInstr):
            return self.visit_select_inst(inst)
        elif isinstance(inst, ir.ICmpInstr):
            return self.visit_cmp_inst(inst, False)
        elif isinstance(inst, ir.FCmpInstr):
            return self.visit_cmp_inst(inst, True)
        elif isinstance(inst, ir.BinaryOperator):
            return self.visit_binary_operator(inst)
        elif isinstance(inst, ir.GetElementPtrInstr):
            return self.visit_gep_inst(inst)
        elif isinstance(inst, ir.CastInstr):
            return self.visit_cast_inst(inst)
        elif isinstance(inst, ir.LoadInstr):
            return self.visit_load_inst(inst)
        elif isinstance(inst, ir.StoreInstr):
            return self.visit_store_inst(inst)
        elif isinstance(inst, ir.CallInstr):
            return self.visit_call_inst(inst)
        elif isinstance(inst, ir.PhiInstr):
            return self.visit_phi_node(inst)
        return False

    def visit_basic_block(self, bb: ir.BasicBlock) -> bool:
        changed = False
        for inst in bb.instructions:
            if self.visit_instruction(inst):
                changed = True
        return changed

    def visit_function(self, func: ir.Function) -> bool:
        changed = False
        for bb in func.basic_blocks:
            if self.visit_basic_block(bb):
                changed = True
        return changed or self.finish()

    def finish(self) -> bool:
        if not self.gathered and not self.scattered:
            return False

        for op, cv in self.gathered:
            if not isinstance(op, ir.Constant):
                ty = op.type
                if isinstance(ty, ir.VectorType):
                    count = ty.count
                    res = ir.Constant(ty, None)
                    for i, v in enumerate(cv):
                        res = self.builder.insert_element(res, v, ir.Constant(ir.IntType(32), i))
                else:
                    assert len(cv) == 1 and ty == cv[0].type
                    res = cv[0]

                if op != res:
                    op.replace_all_uses_with(res)
                    self.potentially_dead_instrs.append(op)

        for inst in self.potentially_dead_instrs:
            if isinstance(inst, ir.Constant) or not inst.uses:
                inst.parent.remove(inst)

        self.gathered.clear()
        self.scattered.clear()
        self.potentially_dead_instrs.clear()

        return True

    def run_on_module(self) -> bool:
        changed = False
        for func in self.module.functions:
            if self.visit_function(func):
                changed = True
        return changed