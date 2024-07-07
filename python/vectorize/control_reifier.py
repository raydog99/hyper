from typing import Optional, Dict, List, Tuple
import llvmlite.ir as ir
from control_dependence import ControlCondition, ConditionAnd, ConditionOr
from vloop import VLoop

class ControlReifier:
    def __init__(self, ctx: ir.Context):
        self.ctx = ctx
        self.reified_values: Dict[Tuple[ControlCondition, VLoop], ir.Value] = {}
        self.inserted_insts: List[ir.Instruction] = []

    def reify(self, c: Optional[ControlCondition], vl: VLoop) -> ir.Value:
        if c is None:
            return ir.Constant(ir.IntType(1), 1)

        key = (c, vl)
        if key in self.reified_values:
            return self.reified_values[key]

        if isinstance(c, ConditionAnd):
            self.reify(c.parent, vl)
            cond = c.cond
            if not c.is_true:
                not_inst = ir.IRBuilder(vl.builder.block).not_(cond)
                self.inserted_insts.append(not_inst)
                vl.add_instruction(not_inst, c.parent)
                cond = not_inst
            reified = vl.create_one_hot_phi(c.parent, cond, ir.Constant(ir.IntType(1), 0), "reified.onehot")
        elif isinstance(c, ConditionOr):
            reified = self.reify(c.conds[0], vl)
            for c2 in c.conds[1:]:
                tmp = ir.IRBuilder(vl.builder.block).or_(reified, self.reify(c2, vl))
                self.inserted_insts.append(tmp)
                vl.add_instruction(tmp, None)
                reified = tmp
        else:
            raise ValueError(f"Unsupported control condition type: {type(c)}")

        self.reified_values[key] = reified

        if isinstance(c, ConditionAnd):
            self.reify(c.complement, vl)

        return reified

    def has_value(self, c: Optional[ControlCondition], vl: VLoop) -> bool:
        return c is None or (c, vl) in self.reified_values

    def get_value(self, c: Optional[ControlCondition], vl: VLoop) -> ir.Value:
        assert self.has_value(c, vl)
        if c is None:
            return ir.Constant(ir.IntType(1), 1)
        return self.reified_values[(c, vl)]