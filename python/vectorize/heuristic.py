from typing import List, Dict, Set, Optional
import llvmlite.binding as llvm
from packer import Packer
from solver import Solver
from vector_pack import VectorPack, OperandPack, VectorPackContext

C_SPLAT = 1.0
C_PERM = 2.0
C_INSERT = 2.0
C_SHUFFLE = 2.0
C_EXTRACT = 1.0

ALLOW_DEINTERLEAVE = False
ALLOW_TRANSPOSE = False

class Solution:
    def __init__(self, cost: float, packs: List[VectorPack]):
        self.cost = cost
        self.packs = packs

class Heuristic:
    def __init__(self, pkr: Packer):
        self.solutions: Dict[OperandPack, Solution] = {}
        self.scalar_costs: Dict[llvm.ValueRef, float] = {}
        self.pkr = pkr

    def get_cost(self, vp: VectorPack) -> float:
        cost = vp.get_producing_cost()
        for op in vp.get_operand_packs():
            if not all(isinstance(v, llvm.CmpInst) for v in op):
                cost += self.get_cost(op)
        return cost

    @staticmethod
    def deinterleave(vp_ctx: VectorPackContext, op: OperandPack, stride: int) -> List[OperandPack]:
        # Implementation of deinterleave
        return []

    @staticmethod
    def transpose(vp_ctx: VectorPackContext, op: OperandPack, n: int) -> Optional[OperandPack]:
        if len(op) % n != 0:
            return None
        m = len(op) // n
        t = [op[j * m + i] for i in range(m) for j in range(n)]
        return vp_ctx.get_canonical_operand_pack(t)

    def solve(self, op: OperandPack) -> Solution:
        if op in self.solutions:
            return self.solutions[op]

        self.solutions[op] = Solution(0, [])

        cost = 0.0
        inserted: Set[llvm.ValueRef] = set()
        for v in op:
            if not isinstance(v, llvm.Constant) and v not in inserted:
                inserted.add(v)
                cost += self.get_cost_value(v) + C_INSERT

        sol = Solution(cost, [])
        if cost == 0:
            self.solutions[op] = sol
            return sol

        broadcast_cost = self.get_cost_value(op[0]) + C_SPLAT
        if op.is_splat() and cost > broadcast_cost:
            return Solution(broadcast_cost, [])

        vp_ctx = self.pkr.get_context()
        deduped = vp_ctx.dedup(op)
        extra_cost = C_SHUFFLE if deduped != op else 0

        opi = self.pkr.get_producer_info(deduped)
        for vp in opi.get_producers():
            new_sol = Solution(self.get_cost(vp) + extra_cost, [vp])
            if new_sol.cost < sol.cost:
                sol = new_sol

        if ALLOW_TRANSPOSE:
            for n in [2, 4, 8]:
                t = self.transpose(vp_ctx, op, n)
                if t:
                    opi = self.pkr.get_producer_info(t)
                    for vp in opi.get_producers():
                        new_sol = Solution(self.get_cost(vp) + C_PERM, [vp])
                        if new_sol.cost < sol.cost:
                            sol = new_sol

        if ALLOW_DEINTERLEAVE:
            for stride in [2, 4, 8]:
                if len(deduped) % stride == 0:
                    ops = self.deinterleave(vp_ctx, deduped, stride)
                    cost = C_SHUFFLE * len(ops)
                    packs = []
                    for op2 in ops:
                        sol2 = self.solve(op2)
                        packs.extend(sol2.packs)
                        cost += sol2.cost
                        if cost > sol.cost:
                            break
                    if cost < sol.cost:
                        sol = Solution(cost, packs)

        self.solutions[op] = sol
        return sol

    def get_cost_value(self, v: llvm.ValueRef) -> float:
        if not isinstance(v, llvm.Instruction):
            return 0

        if v in self.scalar_costs:
            return self.scalar_costs[v]

        self.scalar_costs[v] = 0
        cost = self.pkr.get_scalar_cost(v)
        for op in v.operands:
            cost += self.get_cost_value(op)
        self.scalar_costs[v] = cost
        return cost