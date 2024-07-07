use std::collections::{HashSet, VecDeque};
use llvm_sys::prelude::*;
use llvm_sys::core::*;

pub struct Loop {
    raw: *mut LLVMBasicBlock
}

impl Loop {
    pub fn new(block: *mut LLVMBasicBlock) -> Self {
        Loop { raw: block }
    }

    pub fn get_header(&self) -> *mut LLVMBasicBlock {
        self.raw
    }
}

pub struct LoopInfo {
    loops: Vec<Loop>,
}

impl LoopInfo {
    pub fn new() -> Self {
        LoopInfo {
            loops: Vec::new(),
        }
    }

    pub fn analyze(&mut self, function: LLVMValueRef) {
        let mut blocks = VecDeque::new();
        let mut visited = HashSet::new();

        unsafe {
            let mut current_block = LLVMGetFirstBasicBlock(function);

            while !current_block.is_null() {
                blocks.push_back(current_block);
                current_block = LLVMGetNextBasicBlock(current_block);
            }
        }

        while let Some(block) = blocks.pop_front() {
            if !visited.contains(&block) {
                visited.insert(block);

                if self.is_loop_header(block) {
                    self.loops.push(Loop::new(block));
                }

                unsafe {
                    let mut succ = LLVMGetFirstSuccessor(block);
                    while !succ.is_null() {
                        blocks.push_back(succ);
                        succ = LLVMGetNextSuccessor(block);
                    }
                }
            }
        }
    }

    pub fn is_loop_header(&self, block: *mut LLVMBasicBlock) -> bool {
        true
    }

    pub fn get_loops(&self) -> &Vec<Loop> {
        &self.loops
    }
}

pub unsafe fn compute_rpo(f: LLVMValueRef, li: &LoopInfo, rpo: &mut Vec<LLVMBasicBlockRef>) {
    let mut worklist = VecDeque::new();
    let mut visited = HashSet::new();

    let entry_bb = LLVMGetEntryBasicBlock(f);
    worklist.push_back(entry_bb);

    while let Some(bb) = worklist.pop_front() {
        if !visited.contains(&bb) {
            visited.insert(bb);

            let term = LLVMGetBasicBlockTerminator(bb);
            let num_succs = LLVMGetNumSuccessors(term);
            for i in 0..num_succs {
                let succ = LLVMGetSuccessor(term, i);
                if li.get_loop_for(succ).is_some() {
                    worklist.push_back(succ);
                } else {
                    worklist.push_front(succ);
                }
            }

            rpo.insert(0, bb);
        }
    }
}

pub unsafe fn compute_loop_rpo(li: &LoopInfo, l: &Loop, rpo: &mut Vec<LLVMBasicBlockRef>) {
    let mut worklist = VecDeque::new();
    let mut visited = HashSet::new();

    let header = li.get_header(l);
    worklist.push_back(header);

    while let Some(bb) = worklist.pop_front() {
        if !visited.contains(&bb) {
            visited.insert(bb);

            let term = LLVMGetBasicBlockTerminator(bb);
            let num_succs = LLVMGetNumSuccessors(term);
            for i in 0..num_succs {
                let succ = LLVMGetSuccessor(term, i);
                if li.contains(l, succ) {
                    if succ != header {
                        worklist.push_back(succ);
                    }
                } else {
                    worklist.push_front(succ);
                }
            }

            rpo.insert(0, bb);
        }
    }
}

pub unsafe fn compute_nested_loop_rpo(li: &LoopInfo, l: &Loop, rpo: &mut Vec<LLVMBasicBlockRef>) {
    let mut sub_loops = Vec::new();
    let mut worklist = VecDeque::new();
    let mut visited = HashSet::new();

    let header = li.get_header(l);
    worklist.push_back(header);

    while let Some(bb) = worklist.pop_front() {
        if !visited.contains(&bb) {
            visited.insert(bb);

            let term = LLVMGetBasicBlockTerminator(bb);
            let num_succs = LLVMGetNumSuccessors(term);
            for i in 0..num_succs {
                let succ = LLVMGetSuccessor(term, i);
                if li.contains(l, succ) {
                    if let Some(succ_loop) = li.get_loop_for(succ) {
                        if li.get_parent_loop(succ_loop) == Some(l) {
                            sub_loops.push(succ_loop);
                        } else if succ != header {
                            worklist.push_back(succ);
                        }
                    } else if succ != header {
                        worklist.push_back(succ);
                    }
                } else {
                    worklist.push_front(succ);
                }
            }

            rpo.insert(0, bb);
        }
    }

    for sub_loop in sub_loops.iter().rev() {
        compute_loop_rpo(li, sub_loop, rpo);
    }
}

pub unsafe fn la_rpo_recurse(li: &LoopInfo, l: &Loop, rpo: &mut Vec<LLVMBasicBlockRef>) {
    if li.is_outermost_loop(l) {
        let header = li.get_header(l);
        let f = LLVMGetBasicBlockParent(header);
        compute_rpo(f, li, rpo);
    } else if li.has_nested_loops(l) {
        compute_nested_loop_rpo(li, l, rpo);
    } else {
        compute_loop_rpo(li, l, rpo);
    }
}

pub unsafe fn compute_la_rpo(f: LLVMValueRef, li: &LoopInfo) -> Vec<LLVMBasicBlockRef> {
    let mut rpo = Vec::new();
    compute_rpo(f, li, &mut rpo);
    
    for l in li.get_top_level_loops() {
        la_rpo_recurse(li, l, &mut rpo);
    }

    rpo
}