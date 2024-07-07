from typing import List, Dict
from llvmlite import ir
import operator

class MatchManager:
    def __init__(self):
        self.op_matches: Dict[Operation, List[Operation.Match]] = {}

    @staticmethod
    def sort_by_output(a: Operation.Match, b: Operation.Match):
        return operator.lt(a.output, b.output)

    def match_value(self, v: ir.Value):
        for op, matches in self.op_matches.items():
            op.match(v, matches)

    @classmethod
    def create(cls, insts: List[InstBinding], f: ir.Function):
        mm = cls()
        for inst in insts:
            for lane_op in inst.get_lane_ops():
                op = lane_op.get_operation()
                if op not in mm.op_matches:
                    mm.op_matches[op] = []
        
        for bb in f.blocks:
            for inst in bb.instructions:
                mm.match_value(inst)
        
        for matches in mm.op_matches.values():
            matches.sort(key=lambda m: m.output)
        
        return mm

    @classmethod
    def create_with_instructions(cls, insts: List[InstBinding], to_match: List[ir.Instruction]):
        mm = cls()
        for inst in insts:
            for lane_op in inst.get_lane_ops():
                op = lane_op.get_operation()
                if op not in mm.op_matches:
                    mm.op_matches[op] = []
        
        for inst in to_match:
            mm.match_value(inst)
        
        for matches in mm.op_matches.values():
            matches.sort(key=lambda m: m.output)
        
        return mm

    def get_matches(self, op: Operation) -> List[Operation.Match]:
        return self.op_matches.get(op, [])

    def get_matches_for_output(self, op: Operation, output: ir.Value) -> List[Operation.Match]:
        matches = self.get_matches(op)
        dummy_match = Operation.Match(live_in=False, operands=[], output=output)
        lower = next((i for i, m in enumerate(matches) if not self.sort_by_output(m, dummy_match)), len(matches))
        upper = next((i for i, m in enumerate(matches) if self.sort_by_output(dummy_match, m)), len(matches))
        return matches[lower:upper]