from typing import List, Set
from collections import deque
from llvmlite import binding as llvm

class Loop:
    def __init__(self, header):
        self.header = header

    def get_header(self):
        return self.header

class LoopInfo:
    def __init__(self):
        self.loops = []

    def analyze(self, function):
        self.loops.clear()
        blocks = deque()
        visited = set()

        current_block = function.blocks[0]

        blocks.append(current_block)

        while blocks:
            block = blocks.popleft()
            if block in visited:
                continue
            visited.add(block)

            if self.is_loop_header(block):
                self.loops.append(Loop(block))

            for succ in block.terminator.operands:
                if succ not in visited:
                    blocks.append(succ)

    def is_loop_header(self, block):
        return True

    def get_loops(self):
        return self.loops

def compute_rpo(f: 'Function', li: LoopInfo, rpo: List['BasicBlock']):
    worklist = deque([f.entry_basic_block])
    visited = set()

    while worklist:
        bb = worklist.popleft()
        if bb not in visited:
            visited.add(bb)
            for succ in bb.successors:
                if li.get_loop_for(succ) is not None:
                    worklist.append(succ)
                else:
                    worklist.appendleft(succ)
            rpo.insert(0, bb)

def compute_loop_rpo(li: LoopInfo, l: Loop, rpo: List['BasicBlock']):
    worklist = deque([li.get_header(l)])
    visited = set()

    while worklist:
        bb = worklist.popleft()
        if bb not in visited:
            visited.add(bb)
            for succ in bb.successors:
                if li.contains(l, succ):
                    if succ != li.get_header(l):
                        worklist.append(succ)
                else:
                    worklist.appendleft(succ)
            rpo.insert(0, bb)

def compute_nested_loop_rpo(li: LoopInfo, l: Loop, rpo: List['BasicBlock']):
    sub_loops = []
    worklist = deque([li.get_header(l)])
    visited = set()

    while worklist:
        bb = worklist.popleft()
        if bb not in visited:
            visited.add(bb)
            for succ in bb.successors:
                if li.contains(l, succ):
                    succ_loop = li.get_loop_for(succ)
                    if succ_loop is not None and li.get_parent_loop(succ_loop) == l:
                        sub_loops.append(succ_loop)
                    elif succ != li.get_header(l):
                        worklist.append(succ)
                else:
                    worklist.appendleft(succ)
            rpo.insert(0, bb)

    for sub_loop in reversed(sub_loops):
        compute_loop_rpo(li, sub_loop, rpo)

def la_rpo_recurse(li: LoopInfo, l: Loop, rpo: List['BasicBlock']):
    if li.is_outermost_loop(l):
        compute_rpo(li.get_header(l).parent, li, rpo)
    elif li.has_nested_loops(l):
        compute_nested_loop_rpo(li, l, rpo)
    else:
        compute_loop_rpo(li, l, rpo)

def compute_la_rpo(f: 'Function', li: LoopInfo) -> List['BasicBlock']:
    rpo = []
    compute_rpo(f, li, rpo)
    
    for l in li.get_top_level_loops():
        la_rpo_recurse(li, l, rpo)

    return rpo