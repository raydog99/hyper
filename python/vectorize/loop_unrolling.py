import llvmlite.ir as ir
import llvmlite.binding as llvm
from typing import Dict, List, Optional, Set
import argparse

class Loop:
    def __init__(self, header: ir.Block, latch: ir.Block, blocks: List[ir.Block], exits: List[ir.Block]):
        self.header = header
        self.latch = latch
        self.blocks = blocks
        self.exits = exits

class UnrollLoopOptions:
    def __init__(self, count: int, trip_count: int, trip_multiple: int, peel_count: int,
                 allow_runtime: bool, allow_expensive_trip_count: bool, force: bool,
                 unroll_remainder: bool, forget_all_scev: bool):
        self.count = count
        self.trip_count = trip_count
        self.trip_multiple = trip_multiple
        self.peel_count = peel_count
        self.allow_runtime = allow_runtime
        self.allow_expensive_trip_count = allow_expensive_trip_count
        self.force = force
        self.unroll_remainder = unroll_remainder
        self.forget_all_scev = forget_all_scev

class LoopUnrollResult:
    UNMODIFIED = 0
    PARTIALLY_UNROLLED = 1
    FULLY_UNROLLED = 2

class UnrolledValue:
    def __init__(self, iteration: int, original: ir.Value):
        self.iteration = iteration
        self.original = original

# Global options
UNROLL_RUNTIME_EPILOG = False

def need_to_insert_phis_for_lcssa(l: Loop, blocks: List[ir.Block], li) -> bool:
    for bb in blocks:
        if li.get_loop_for(bb) == l:
            continue
        for instr in bb.instructions:
            for op in instr.operands:
                if isinstance(op, ir.Instruction):
                    def_loop = li.get_loop_for(op.parent)
                    if def_loop and def_loop.contains(l):
                        return True
    return False

def simplify_loop_after_unroll2(l: Loop, simplify_ivs: bool, li, se, dt, ac, tti):
    if se and simplify_ivs:
        dead_insts = []
        simplify_loop_ivs(l, se, dt, li, tti, dead_insts)
        for inst in dead_insts:
            inst.erase_from_parent()

    dl = l.header.function.module.data_layout
    for bb in l.blocks:
        i = 0
        while i < len(bb.instructions):
            inst = bb.instructions[i]
            simplified = simplify_instruction(inst, dl, None, dt, ac)
            if simplified and li.replacement_preserves_lcssa_form(inst, simplified):
                inst.replace_all_uses_with(simplified)
            if is_instruction_trivially_dead(inst):
                inst.erase_from_parent()
            else:
                i += 1

def is_epilog_profitable(l: Loop) -> bool:
    preheader = l.header.predecessors[0]
    for phi in l.header.phi_nodes:
        for incoming in phi.incoming:
            if incoming[1] == preheader and isinstance(incoming[0], ir.Constant):
                return True
    return False

def unroll_loop_with_vmap(l: Loop, ulo: UnrollLoopOptions, li, se, dt, ac, tti,
                          preserve_lcssa: bool, unroll_to_orig_map: Dict[ir.Value, UnrolledValue],
                          remainder_loop: Optional[Loop]) -> LoopUnrollResult:
    if not l.preheader or not l.latch or not l.is_safe_to_clone():
        return LoopUnrollResult.UNMODIFIED

    if ulo.trip_count != 0 and ulo.count > ulo.trip_count:
        ulo.count = ulo.trip_count

    if ulo.trip_count == 0 and ulo.count < 2 and ulo.peel_count == 0:
        return LoopUnrollResult.UNMODIFIED

    completely_unroll = ulo.count == ulo.trip_count
    runtime_trip_count = ulo.trip_count == 0 and ulo.count > 0 and ulo.allow_runtime

    peeled = False
    if ulo.peel_count > 0:
        peeled = peel_loop(l, ulo.peel_count, li, se, dt, ac, preserve_lcssa)
        if peeled:
            exiting_block = l.latch
            ulo.trip_count = se.get_small_constant_trip_count(l, exiting_block)
            ulo.trip_multiple = se.get_small_constant_trip_multiple(l, exiting_block)

    orig_loop_blocks = l.blocks.copy()

    if se:
        if ulo.forget_all_scev:
            se.forget_all_loops()
        else:
            se.forget_topmost_loop(l)

    breakout_trip = ulo.trip_count % ulo.count if ulo.trip_count != 0 else ulo.trip_multiple
    ulo.trip_multiple = min(ulo.count, ulo.trip_multiple) if ulo.trip_count == 0 else 0

    last_value_map = {}
    orig_phi_nodes = [phi for phi in l.header.phi_nodes]

    headers = [l.header]
    latches = [l.latch]
    unrolled_loop_blocks = l.blocks.copy()

    loops_to_simplify = set(l.sub_loops)

    for it in range(1, ulo.count):
        new_blocks = []
        new_loops = {l: l}

        for bb in orig_loop_blocks:
            new_bb = clone_basic_block(bb, f".{it}")
            l.header.function.basic_blocks.append(new_bb)

            unroll_to_orig_map[new_bb] = UnrolledValue(it, bb)

            old_loop = add_cloned_block_to_loop_info(bb, new_bb, li, new_loops)
            if old_loop:
                loops_to_simplify.add(new_loops[old_loop])

            if bb == l.header:
                for i, orig_phi in enumerate(orig_phi_nodes):
                    new_phi = new_bb.instructions[0]
                    in_val = new_phi.incoming[-1][0]
                    if it > 1 and l.contains(in_val.parent):
                        last_value_map[orig_phi] = last_value_map[in_val]
                    else:
                        last_value_map[orig_phi] = in_val
                    new_phi.erase_from_parent()

            for old_inst, new_inst in zip(bb.instructions, new_bb.instructions):
                last_value_map[old_inst] = new_inst
                unroll_to_orig_map[new_inst] = UnrolledValue(it, old_inst)

            for succ in bb.successors:
                if not l.contains(succ):
                    for phi in succ.phi_nodes:
                        incoming = phi.get_incoming_value_for_block(bb)
                        new_incoming = last_value_map.get(incoming, incoming)
                        phi.add_incoming(new_incoming, new_bb)

            if bb == l.header:
                headers.append(new_bb)
            if bb == l.latch:
                latches.append(new_bb)

            new_blocks.append(new_bb)
            unrolled_loop_blocks.append(new_bb)

            if dt:
                if bb == l.header:
                    dt.add_new_block(new_bb, latches[it - 1])
                else:
                    bb_dom_node = dt.get_node(bb)
                    bb_idom = bb_dom_node.get_idom()
                    original_bb_idom = bb_idom.get_block()
                    dt.add_new_block(new_bb, last_value_map[original_bb_idom])

        remap_instructions_in_blocks(new_blocks, last_value_map)
        for new_block in new_blocks:
            for inst in new_block.instructions:
                if isinstance(inst, ir.CallInstr) and inst.called_function.name == "llvm.assume":
                    ac.register_assumption(inst)

    for pn in orig_phi_nodes:
        if completely_unroll:
            incoming = pn.get_incoming_value_for_block(l.preheader)
            pn.replace_all_uses_with(incoming)
            pn.erase_from_parent()
        elif ulo.count > 1:
            in_val = pn.remove_incoming_value(l.latch)
            if l.contains(in_val.parent):
                in_val = last_value_map[in_val]
            pn.add_incoming(in_val, latches[-1])

    for i, latch in enumerate(latches):
        dest = headers[(i + 1) % len(headers)]
        term = latch.terminator
        if isinstance(term, ir.BranchInstr) and term.is_conditional:
            term.set_successor(0, dest)
        else:
            new_br = ir.BranchInstr(dest)
            term.replace_all_uses_with(new_br)
            term.erase_from_parent()

    simplify_loop_after_unroll2(l, not completely_unroll and (ulo.count > 1 or peeled), li, se, dt, ac, tti)

    outer_l = l.parent_loop
    if completely_unroll:
        li.erase(l)

    if preserve_lcssa and outer_l and completely_unroll:
        need_to_fix_lcssa = need_to_insert_phis_for_lcssa(outer_l, unrolled_loop_blocks, li)
        if need_to_fix_lcssa:
            latch_loop = li.get_loop_for(latches[-1])
            fix_lcssa_loop = outer_l
            while fix_lcssa_loop.parent_loop != latch_loop:
                fix_lcssa_loop = fix_lcssa_loop.parent_loop
            form_lcssa_recursively(fix_lcssa_loop, dt, li, se)

    if dt:
        if outer_l:
            simplify_loop(outer_l, dt, li, se, ac, None, preserve_lcssa)
        else:
            for loop_to_simplify in loops_to_simplify:
                simplify_loop(loop_to_simplify, dt, li, se, ac, None, preserve_lcssa)

    return LoopUnrollResult.FULLY_UNROLLED if completely_unroll else LoopUnrollResult.PARTIALLY_UNROLLED