from typing import List, Optional
import llvmlite.ir as ir
import llvmlite.binding as llvm

class VectorPack:
    def __init__(self, vp_ctx):
        self.kind = None
        self.producer = None
        self.matches = []
        self.loads = []
        self.stores = []
        self.phis = []
        self.rdx = None
        self.rdx_len = 0
        self.geps = []
        self.gammas = []
        self.cmps = []
        self.cp = None
        self.is_gather_scatter = False
        self.operand_packs = []
        self.ordered_values = []
        self.cost = 0.0
        self.producing_cost = 0.0
        self.vp_ctx = vp_ctx

    def compute_operand_packs_for_general(self) -> List['OperandPack']:
        sig = self.producer.get_signature()
        num_inputs = sig.num_inputs()
        lane_ops = self.producer.get_lane_ops()
        num_lanes = len(lane_ops)
        operand_packs = [OperandPack() for _ in range(num_inputs)]

        for i in range(num_inputs):
            input_values = []
            element_size = 0
            for j in range(num_lanes):
                bound_slices = lane_ops[j].get_bound_slices()
                for k, bs in enumerate(bound_slices):
                    if bs.input_id == i:
                        element_size = bs.size()
                        v = self.matches[j].inputs[k] if self.matches[j] else None
                        input_values.append(BoundInput(bs, v))
            assert element_size != 0

            input_values.sort(key=lambda x: x.slice.lo)
            cur_offset = 0
            stride = input_values[0].slice.size()
            op = operand_packs[i]
            for bv in input_values:
                while cur_offset < bv.slice.lo:
                    op.push(None)
                    cur_offset += stride
                assert cur_offset == bv.slice.lo
                op.push(bv.v)
                cur_offset += stride
            input_size = sig.input_bitwidths[i]
            while cur_offset < input_size:
                op.push(None)
                cur_offset += stride
            assert op.size() * stride == input_size

            if op.front() is None and op.is_splat():
                op.ty = ir.VectorType(ir.IntType(element_size), op.size())

        return operand_packs