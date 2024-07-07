from typing import Dict, List, Optional, Callable
from enum import Enum
from dataclasses import dataclass
import llvmlite.ir as ir
import llvmlite.binding as llvm

class ControlConditionType(Enum):
    AND = 1
    OR = 2

@dataclass
class ControlCondition:
    type: ControlConditionType

@dataclass
class ConditionAnd(ControlCondition):
    parent: Optional['ControlCondition']
    cond: ir.Value
    is_true: bool
    complement: Optional['ControlCondition']

    def __init__(self, parent, cond, is_true, complement):
        super().__init__(ControlConditionType.AND)
        self.parent = parent
        self.cond = cond
        self.is_true = is_true
        self.complement = complement

@dataclass
class ConditionOr(ControlCondition):
    conds: List['ControlCondition']
    greatest_common_cond: Optional['ControlCondition']

    def __init__(self, conds, greatest_common_cond):
        super().__init__(ControlConditionType.OR)
        self.conds = conds
        self.greatest_common_cond = greatest_common_cond

class BlockBuilder:
    def __init__(self, entry_bb: ir.Block, emit_condition: Callable[[ir.Value], ir.Value]):
        self.ctx = entry_bb.function.module.context
        self.function = entry_bb.function
        self.emit_condition = emit_condition
        self.active_conds: Dict[Optional[ControlCondition], ir.Block] = {None: entry_bb}
        self.semi_active_conds: Dict[ControlCondition, List[ControlCondition]] = {}
        self.dummy_counter = 0

    def create_block(self) -> ir.Block:
        return self.function.append_basic_block(name="")

    def get_block_for(self, c: Optional[ControlCondition]) -> ir.Block:
        if c in self.active_conds:
            return self.active_conds[c]

        if c in self.semi_active_conds:
            return self._handle_semi_active_cond(c)

        if isinstance(c, ConditionAnd):
            return self._handle_condition_and(c)